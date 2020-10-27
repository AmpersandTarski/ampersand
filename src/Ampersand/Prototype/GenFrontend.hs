{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module Ampersand.Prototype.GenFrontend (doGenFrontend, doGenBackend) where

import           Ampersand.ADL1
import           Ampersand.Basics
import           Ampersand.Basics.BuildInfo_Generated (cabalVersionStr)
import           Ampersand.Classes.Relational
import           Ampersand.Core.ShowAStruct
import           Ampersand.FSpec.FSpec
import           Ampersand.FSpec.ToFSpec.NormalForms
import           Ampersand.Misc.HasClasses
import           Ampersand.Output.FSpec2SQL (databaseStructureSql)
import           Ampersand.Output.ToJSON.ToJson
import           Ampersand.Prototype.ProtoUtil
import           Ampersand.Runners (logLevel)
import           Ampersand.Types.Config
import           Data.Hashable (hash)
import qualified RIO.ByteString.Lazy  as BL
import qualified RIO.Text as T
import qualified RIO.List as L
import           RIO.Time
import           Salve
import           System.Directory
import           System.FilePath
import           Text.StringTemplate(Stringable, StringTemplate, setAttribute, newSTMP, checkTemplateDeep, render)
import           Text.StringTemplate.GenericStandard () -- only import instances

{- TODO
- Be more consistent with record selectors/pattern matching
- HStringTemplate hangs on uninitialized vars in anonymous template? (maybe only fields?)
- isRoot is a bit dodgy (maybe make dependency on ONE and SESSIONS a bit more apparent)
- Keeping templates as statics requires that the static files are written before templates are used.
  Maybe we should keep them as cabal data-files instead. (file extensions and directory structure are predictable)


NOTE: interface refs are handled as follows:

INTERFACE MyInterface 
  BOX [ ref : rel[$a*$b]
        INTERFACE RefInterface
      ]
      
INTERFACE RefInterface relRef[$b*$c]
  BOX [ .. : ..[$c*$d]
      ]

is basically mapped onto:

INTERFACE MyInterface 
  BOX [ ref : (rel;relRef)[$a*$c]
        BOX [ .. : ..[$c*$d]
            ]
      ]

This is considered editable iff the composition rel;relRef yields an editable relation (e.g. for editableR;I).

-}

        
-- For useful info on the template language, see
-- https://theantlrguy.atlassian.net/wiki/display/ST4/StringTemplate+cheat+sheet
-- NOTE: due to a bug in HStringTemplate's checkTemplateDeep, non-existent attribute names on
--       composite attributes in anonymous templates will hang the generator :-(
--       Eg.  "$subObjects:{subObj| .. $subObj.nonExistentField$ .. }$"

doGenFrontend :: (HasFSpecGenOpts env, HasRunner env, HasDirPrototype env) =>
                 FSpec -> RIO env ()
doGenFrontend fSpec = do
    now <- getCurrentTime
    logInfo "Generating frontend..."
    copyTemplates
    feInterfaces <- buildInterfaces fSpec
    genViewInterfaces fSpec feInterfaces
    genControllerInterfaces fSpec feInterfaces
    genRouteProvider fSpec feInterfaces
    writePrototypeAppFile ".timestamp" (tshow . hash . show $ now) -- this hashed timestamp is used by the prototype framework to prevent browser from using the wrong files from cache
    logInfo "Frontend generated"

doGenBackend :: (Show env, HasRunner env, HasProtoOpts env, HasDirPrototype env, HasCheckCompilerVersion env) =>
                FSpec -> RIO env ()
doGenBackend fSpec = do
  env <- ask
  checkCompilerVersion <- view checkCompilerVersionL
  logInfo "Generating backend..."
  let dir = getGenericsDir env
  if checkCompilerVersion
    then do checkCompilerCompatibility
    else do logInfo "Skipping compiler version check"
  writeFileUtf8 (dir </> "database"   <.>"sql" ) $ databaseStructureSql fSpec
  writeFile (dir </> "settings"   <.>"json") $ settingsToJSON env fSpec
  writeFile (dir </> "relations"  <.>"json") $ relationsToJSON env fSpec
  writeFile (dir </> "rules"      <.>"json") $ rulesToJSON env fSpec
  writeFile (dir </> "concepts"   <.>"json") $ conceptsToJSON env fSpec
  writeFile (dir </> "conjuncts"  <.>"json") $ conjunctsToJSON env fSpec
  writeFile (dir </> "interfaces" <.>"json") $ interfacesToJSON env fSpec
  writeFile (dir </> "views"      <.>"json") $ viewsToJSON env fSpec
  writeFile (dir </> "roles"      <.>"json") $ rolesToJSON env fSpec
  writeFile (dir </> "populations"<.>"json") $ populationToJSON env fSpec
  logInfo "Backend generated"

checkCompilerCompatibility :: (HasLogFunc env, HasDirPrototype env) => RIO env ()
checkCompilerCompatibility =
  do
    env <- ask
    let filePath = compilerVersionFile env
    res <- readUTF8File filePath
    case res of
      Left err ->
        -- For now, log a warning when compiler-version.txt cannot be read (e.g. when file does not exists)
        -- #TODO when prototype framework is updated (i.e. contains compiler-version.txt), throw an error and exit
        logWarn $ "WARNING: Cannot determine compiler compatibility. Error reading compiler version file: " <> displayShow err
      Right content -> do
        let constraints = map makeConstraint $ lines (T.unpack content)
        let failedConstraints = checkConstraints compilerVersion constraints
        case failedConstraints of
          [] -> logInfo "Ampersand compiler is compatible with targeted prototype framework"
          _  -> do
            mapM_ (\ constraint -> logInfo $ "Ampersand compiler version " <> displayShow (renderVersion compilerVersion) <> " does not satisfy constraint " <> displayShow (renderConstraint constraint)) failedConstraints
            exitWith $ FailedToGeneratePrototypeBackend ["Ampersand compiler is not compatible with deployed prototype framework. Check version constraints in ", T.pack (compilerVersionFile env)]
  where
    makeConstraint :: String -> Constraint
    makeConstraint constraintStr =
      case parseConstraint constraintStr of
        Just constraint -> constraint
        Nothing -> exitWith $ FailedToGeneratePrototypeBackend ["Cannot parse Ampersand compiler version constraint '" <> T.pack constraintStr <> "'"]
    
    compilerVersion = 
      case parseVersion (T.unpack cabalVersionStr) of
        Just version -> version
        Nothing -> exitWith $ FailedToGeneratePrototypeBackend ["Cannot parse Ampersand compiler version " <> cabalVersionStr]

    compilerVersionFile :: HasDirPrototype a => a -> FilePath
    compilerVersionFile env = getGenericsDir env </> "compiler-version.txt"

    checkConstraints :: Version -> [Constraint] -> [Constraint]
    checkConstraints version constraints =
      filter (\ constraint -> not $ satisfiesConstraint constraint version) constraints

writeFile :: (HasLogFunc env) => FilePath -> BL.ByteString -> RIO env()
writeFile filePath content = do
  logDebug $ "  Generating "<>display (T.pack filePath) 
  liftIO $ createDirectoryIfMissing True (takeDirectory filePath)
  BL.writeFile filePath content
  
copyTemplates :: (HasFSpecGenOpts env, HasDirPrototype env, HasLogFunc env) =>
                 RIO env ()
copyTemplates = do
  env <- ask
  let tempDir = dirSource env </> "templates"
      toDir = getTemplateDir env
  tempDirExists <- liftIO $ doesDirectoryExist tempDir
  if tempDirExists then do
         logDebug $ "Copying project specific templates from " <> display (T.pack tempDir) <> " -> " <> display (T.pack toDir)
         copyDirRecursively tempDir toDir -- recursively copy all templates
  else
         logDebug $ "No project specific templates (there is no directory " <> display (T.pack tempDir) <> ")"

------ Build intermediate data structure
-- NOTE: _ disables 'not used' warning for fields
data FEInterface = FEInterface { ifcName :: Text
                               , ifcLabel :: Text
                               , _ifcExp :: Expression
                               , _ifcSource :: A_Concept
                               , _ifcTarget :: A_Concept
                               , _ifcRoles :: [Role]
                               , _ifcObj :: FEObject2
                               } deriving (Typeable, Data)

data FEObject2 =
    FEObjE { objName     :: Text
           , objExp      :: Expression
           , objSource   :: A_Concept
           , objTarget   :: A_Concept
           , objCrudC    :: Bool
           , objCrudR    :: Bool
           , objCrudU    :: Bool
           , objCrudD    :: Bool
           , exprIsUni   :: Bool
           , exprIsTot   :: Bool
           , relIsProp   :: Bool -- True iff the expression is a kind of simple relation and that relation is a property.
           , exprIsIdent :: Bool
           , atomicOrBox :: FEAtomicOrBox
           }
  | FEObjT { objName     :: Text
           , objTxt      :: Text
           } deriving (Show, Data, Typeable )

-- Once we have mClass also for Atomic, we can get rid of FEAtomicOrBox and pattern match on _ifcSubIfcs to determine atomicity.
data FEAtomicOrBox = FEAtomic { objMPrimTemplate :: Maybe ( FilePath -- the absolute path to the template
                                                          , [Text] -- the attributes of the template
                                                          ) }
                   | FEBox    { objMClass :: BoxHeader
                              , ifcSubObjs :: [FEObject2] 
                              } deriving (Show, Data,Typeable)

buildInterfaces :: (HasDirPrototype env) => FSpec -> RIO env [FEInterface]
buildInterfaces fSpec = mapM (buildInterface fSpec allIfcs) topLevelUserInterfaces
  where
    allIfcs :: [Interface]
    allIfcs = interfaceS fSpec

    topLevelUserInterfaces :: [Interface]
    topLevelUserInterfaces = filter (not . ifcIsAPI) allIfcs

buildInterface :: (HasDirPrototype env) => FSpec -> [Interface] -> Interface -> RIO env FEInterface
buildInterface fSpec allIfcs ifc = do
  obj <- buildObject (BxExpr $ ifcObj ifc)
  return 
    FEInterface { ifcName = escapeIdentifier $ name ifc
                , ifcLabel = name ifc
                , _ifcExp = objExp obj
                , _ifcSource = objSource obj
                , _ifcTarget = objTarget obj
                , _ifcRoles = ifcRoles ifc
                , _ifcObj = obj
                }
    -- NOTE: due to Amperand's interface data structure, expression, source, and target are taken from the root object. 
    --       (name comes from interface, but is equal to object name)
 
  where    
    buildObject :: (HasDirPrototype env) => BoxItem -> RIO env FEObject2
    buildObject (BxExpr object') = do
      env <- ask
      let object = substituteReferenceObjectDef fSpec object'
      let iExp = conjNF env $ objExpression object
      (aOrB, iExp') <-
        case objmsub object of
          Nothing -> do
            let ( _ , _ , tgt) = getSrcDclTgt iExp
            let mView = maybe (getDefaultViewForConcept fSpec tgt) (Just . lookupView fSpec) (objmView object)
            mSpecificTemplatePath <-
                  case mView of
                    Just Vd{vdhtml=Just (ViewHtmlTemplateFile fName), vdats=viewSegs}
                              -> return $ Just (fName, mapMaybe vsmlabel viewSegs)
                    _ -> do
                       -- no view, or no view with an html template, so we fall back to target-concept template
                       -- TODO: once we can encode all specific templates with views, we will probably want to remove this fallback
                      let templatePath = "Atomic-" <> T.unpack (idWithoutType tgt) <.> ".html"
                      hasSpecificTemplate <- doesTemplateExist templatePath
                      return $ if hasSpecificTemplate then Just (templatePath, []) else Nothing
            return (FEAtomic { objMPrimTemplate = mSpecificTemplatePath}
                   , iExp)
          Just si ->
            case si of
              Box{} -> do
                subObjs <- mapM buildObject (siObjs si)
                return (FEBox { objMClass  = siHeader si
                              , ifcSubObjs = subObjs
                              }
                        , iExp)
              InterfaceRef{} -> 
                case filter (\rIfc -> name rIfc == siIfcId si) allIfcs of -- Follow interface ref
                  []      -> fatal ("Referenced interface " <> siIfcId si <> " missing")
                  (_:_:_) -> fatal ("Multiple relations of referenced interface " <> siIfcId si)
                  [i]     -> 
                        if siIsLink si
                        then do
                          let templatePath = "View-LINKTO.html"
                          return (FEAtomic { objMPrimTemplate = Just (templatePath, [])}
                                 , iExp)
                        else do 
                          refObj <- buildObject  (BxExpr $ ifcObj i)
                          let comp = ECps (iExp, objExp refObj) 
                               -- Dont' normalize, to prevent unexpected effects (if X;Y = I then ((rel;X) ; (Y)) might normalize to rel)
                          return (atomicOrBox refObj, comp)
                               -- TODO: in Generics.php interface refs create an implicit box, which may cause problems for the new front-end
      let (src, mDecl, tgt) = getSrcDclTgt iExp'
      return FEObjE  { objName = name object
                      , objExp = iExp'
                      , objSource = src
                      , objTarget = tgt
                      , objCrudC = crudC . objcrud $ object
                      , objCrudR = crudR . objcrud $ object
                      , objCrudU = crudU . objcrud $ object
                      , objCrudD = crudD . objcrud $ object
                      , exprIsUni = isUni iExp'
                      , exprIsTot = isTot iExp'
                      , relIsProp  = case mDecl of
                                      Nothing  -> False
                                      Just dcl -> isProp (EDcD dcl)
                      , exprIsIdent = isIdent iExp'
                      , atomicOrBox = aOrB
                      }

      where getSrcDclTgt expr = 
              case getExpressionRelation expr of
                Nothing                          -> (source expr, Nothing  , target expr)
                Just (declSrc, decl, declTgt, _) -> (declSrc    , Just decl, declTgt    ) 
                                                   -- if the expression is a relation, use the (possibly narrowed type) from getExpressionRelation
    buildObject (BxTxt object') = do
      return FEObjT{ objName = name object'
                   , objTxt = objtxt object'
                   }

------ Generate RouteProvider.js

genRouteProvider :: (HasRunner env, HasDirPrototype env) =>
                    FSpec -> [FEInterface] -> RIO env ()
genRouteProvider fSpec ifcs = do
  runner <- view runnerL
  let loglevel' = logLevel runner
  template <- readTemplate "routeProvider.config.js"
  let contents = renderTemplate Nothing template $
                   setAttribute "contextName"         (fsName fSpec)
                 . setAttribute "ampersandVersionStr" ampersandVersionStr
                 . setAttribute "ifcs"                ifcs
                 . setAttribute "verbose"             (loglevel' == LevelDebug)
                 . setAttribute "loglevel"            (show loglevel')
  writePrototypeAppFile "routeProvider.config.js" contents
      
------ Generate view html code
isTopLevel :: A_Concept -> Bool
isTopLevel cpt = isONE cpt || isSESSION cpt
      
genViewInterfaces :: (HasRunner env, HasDirPrototype env) => 
                     FSpec -> [FEInterface] -> RIO env ()
genViewInterfaces fSpec = mapM_ (genViewInterface fSpec)

genViewInterface :: (HasRunner env, HasDirPrototype env) => 
                    FSpec -> FEInterface -> RIO env ()
genViewInterface fSpec interf = do
  runner <- view runnerL
  let loglevel' = logLevel runner
  lns <- genViewObject fSpec 0 (_ifcObj interf)
  template <- readTemplate "interface.html"
  let contents = renderTemplate Nothing template $
                    setAttribute "contextName"         (addSlashes . fsName $ fSpec)
                  . setAttribute "isTopLevel"          (isTopLevel . source . _ifcExp $ interf)
                  . setAttribute "roles"               (map show . _ifcRoles $ interf) -- show string, since StringTemplate does not elegantly allow to quote and separate
                  . setAttribute "ampersandVersionStr" ampersandVersionStr
                  . setAttribute "interfaceName"       (ifcName  interf)
                  . setAttribute "interfaceLabel"      (ifcLabel interf) -- no escaping for labels in templates needed
                  . setAttribute "expAdl"              (showA . _ifcExp $ interf)
                  . setAttribute "source"              (idWithoutType . _ifcSource $ interf)
                  . setAttribute "target"              (idWithoutType . _ifcTarget $ interf)
                  . setAttribute "crudC"               (objCrudC (_ifcObj interf))
                  . setAttribute "crudR"               (objCrudR (_ifcObj interf))
                  . setAttribute "crudU"               (objCrudU (_ifcObj interf))
                  . setAttribute "crudD"               (objCrudD (_ifcObj interf))
                  . setAttribute "contents"            (T.intercalate "\n" lns) -- intercalate, because unlines introduces a trailing \n
                  . setAttribute "verbose"             (loglevel' == LevelDebug)
                  . setAttribute "loglevel"            (show loglevel')
  let filename :: FilePath
      filename = "ifc" <>(T.unpack . ifcName $ interf)<> ".view.html" 
  writePrototypeAppFile filename contents 
  
-- Helper data structure to pass attribute values to HStringTemplate
data SubObjectAttr2 = SubObjAttr{ subObjName :: Text
                                , subObjLabel :: Text
                                , subObjContents :: Text 
                                , subObjExprIsUni :: Bool
                                } deriving (Show, Data, Typeable)
 
genViewObject :: (HasRunner env, HasDirPrototype env) =>
                 FSpec -> Int -> FEObject2 -> RIO env [Text]
genViewObject fSpec depth obj =
  case obj of 
    FEObjE{} -> do
      runner <- view runnerL
      let loglevel' = logLevel runner
      let atomicAndBoxAttrs :: StringTemplate String -> StringTemplate String
          atomicAndBoxAttrs = setAttribute "exprIsUni"  (exprIsUni obj)
                            . setAttribute "exprIsTot"  (exprIsTot obj)
                            . setAttribute "name"       (escapeIdentifier . objName $ obj)
                            . setAttribute "label"      (objName obj) -- no escaping for labels in templates needed
                            . setAttribute "expAdl"     (showA . objExp $ obj) 
                            . setAttribute "source"     (idWithoutType . objSource $ obj)
                            . setAttribute "target"     (idWithoutType . objTarget $ obj)
                            . setAttribute "crudC"      (objCrudC obj)
                            . setAttribute "crudR"      (objCrudR obj)
                            . setAttribute "crudU"      (objCrudU obj)
                            . setAttribute "crudD"      (objCrudD obj)
                            . setAttribute "verbose"    (loglevel' == LevelDebug)
                            . setAttribute "loglevel"   (show loglevel')
      case atomicOrBox obj of
            FEAtomic{} -> do
              {-
                  logDebug (getOpts fSpec) $ replicate depth ' ' <> "ATOMIC "<>show nm <> 
                                                " [" <> name src <> "*"<> name tgt <> "], " <>
                                                (if isEditable then "" else "not ") <> "editable"
              -}
              -- For now, we choose specific template based on target concept. This will probably be too weak. 
              -- (we might want a single concept to could have multiple presentations, e.g. BOOL as checkbox or as string)
              -- logInfo $ nm <> ":" <> show mPrimTemplate
              conceptTemplate <- getTemplateForObject
              let (templateFilename, _) = fromMaybe (conceptTemplate, []) (objMPrimTemplate . atomicOrBox $ obj) -- Atomic is the default template
              template <- readTemplate templateFilename
                        
              return . indentation
                     . T.lines 
                     . renderTemplate Nothing template $ 
                       atomicAndBoxAttrs

            FEBox { objMClass  = header
                  , ifcSubObjs = subObjs
                  } -> do
              subObjAttrs <- mapM genView_SubObject subObjs
                        
              parentTemplate <- readTemplate $ "Box-" <> T.unpack (btType header) <.> "html"
                
              return . indentation
                     . T.lines 
                     . renderTemplate (Just . btKeys $ header) parentTemplate $ 
                           atomicAndBoxAttrs
                         . setAttribute "isRoot"     (depth == 0)
                         . setAttribute "subObjects" subObjAttrs
    FEObjT{} -> pure []
  where 
    indentation :: [Text] -> [Text]
    indentation = map (T.replicate (if depth == 0 then 4 else 16) " " <>)
    genView_SubObject :: (HasRunner env, HasDirPrototype env) =>
                         FEObject2 -> RIO env SubObjectAttr2
    genView_SubObject subObj =
      case subObj of
        FEObjE{} -> 
          do lns <- genViewObject fSpec (depth + 1) subObj
             return SubObjAttr{ subObjName = escapeIdentifier $ objName subObj
                              , subObjLabel = objName subObj -- no escaping for labels in templates needed
                              , subObjContents = T.intercalate "\n" lns
                              , subObjExprIsUni = exprIsUni subObj
                              } 
        FEObjT{} -> 
          do return SubObjAttr{ subObjName = escapeIdentifier $ objName subObj
                              , subObjLabel = objName subObj
                              , subObjContents = objTxt subObj
                              , subObjExprIsUni = True
                              }
    getTemplateForObject :: (HasDirPrototype env) =>
                            RIO env FilePath
    getTemplateForObject 
       | relIsProp obj && (not . exprIsIdent) obj  -- special 'checkbox-like' template for propery relations
                   = return $ "View-PROPERTY"<>".html"
       | otherwise = getTemplateForConcept (objTarget obj)
    getTemplateForConcept :: (HasDirPrototype env) =>
                             A_Concept -> RIO env FilePath
    getTemplateForConcept cpt = do 
         exists <- doesTemplateExist cptfn
         return $ if exists
                  then cptfn
                  else "Atomic-"<>show ttp<.>"html" 
       where ttp = cptTType fSpec cpt
             cptfn = "Concept-"<> T.unpack (name cpt) <.> "html"


------ Generate controller JavaScript code
genControllerInterfaces :: (HasRunner env, HasDirPrototype env) => FSpec -> [FEInterface] -> RIO env ()
genControllerInterfaces fSpec = mapM_ (genControllerInterface fSpec)

genControllerInterface :: (HasRunner env, HasDirPrototype env) => FSpec -> FEInterface -> RIO env ()
genControllerInterface fSpec interf = do
    let controlerTemplateName = "interface.controller.js"
    template <- readTemplate controlerTemplateName
    runner <- view runnerL
    let loglevel' = logLevel runner
    let contents = renderTemplate Nothing template $
                       setAttribute "contextName"              (fsName fSpec)
                     . setAttribute "isRoot"                   (isTopLevel . source . _ifcExp $ interf)
                     . setAttribute "roles"                    (map show . _ifcRoles $ interf) -- show string, since StringTemplate does not elegantly allow to quote and separate
                     . setAttribute "ampersandVersionStr"      ampersandVersionStr
                     . setAttribute "interfaceName"            (ifcName interf)
                     . setAttribute "interfaceLabel"           (ifcLabel interf) -- no escaping for labels in templates needed
                     . setAttribute "expAdl"                   (showA . _ifcExp $ interf)
                     . setAttribute "exprIsUni"                (exprIsUni (_ifcObj interf))
                     . setAttribute "source"                   (idWithoutType . _ifcSource $ interf)
                     . setAttribute "target"                   (idWithoutType . _ifcTarget $ interf)
                     . setAttribute "crudC"                    (objCrudC (_ifcObj interf))
                     . setAttribute "crudR"                    (objCrudR (_ifcObj interf))
                     . setAttribute "crudU"                    (objCrudU (_ifcObj interf))
                     . setAttribute "crudD"                    (objCrudD (_ifcObj interf))
                     . setAttribute "verbose"                  (loglevel' == LevelDebug)
                     . setAttribute "loglevel"                 (show loglevel')
                     . setAttribute "usedTemplate"             controlerTemplateName
    let filename = "ifc" <> T.unpack (ifcName interf) <> ".controller.js"
    writePrototypeAppFile filename contents 

------ Utility functions
-- data type to keep template and source file together for better errors
data Template = Template (StringTemplate FilePath) Text

-- TODO: better abstraction for specific template and fallback to default
doesTemplateExist :: (HasDirPrototype env) => FilePath -> RIO env Bool
doesTemplateExist templatePath = do
  env <- ask
  let absPath = getTemplateDir env </> templatePath
  liftIO $ doesFileExist absPath

readTemplate :: (HasDirPrototype env) =>
                FilePath -> RIO env Template
readTemplate templatePath = do
  env <- ask
  let absPath = getTemplateDir env </> templatePath
  res <- readUTF8File absPath
  case res of
    Left err   -> exitWith $ ReadFileError $ "Error while reading template." : err
    Right cont -> return $ Template (newSTMP . T.unpack $ cont) (T.pack absPath)

-- having Bool attributes prevents us from using a [(Text, Text)] parameter for attribute settings
renderTemplate :: Maybe [TemplateKeyValue] -> Template -> (StringTemplate String -> StringTemplate String) -> Text
renderTemplate userAtts (Template template absPath) setRuntimeAtts =
    case checkTemplateDeep appliedTemplate of
             ([],  [],    []) -> T.pack $ render appliedTemplate
             (parseErrs@(_:_), _, _)
                -> templateError . T.concat $
                      [ T.pack $ "Parse error in " <> tmplt <> " " <> err <> "\n" 
                      | (tmplt,err) <- parseErrs
                      ]
             ([], attrs@(_:_), _)
                | isJust userAtts -> T.pack . render . fillInTheBlanks (L.nub attrs) $ appliedTemplate
                | otherwise -> templateError $  
                      "The following attributes are expected by the template, but not supplied: " <> tshow attrs
             ([], [], ts@(_:_))
                -> templateError $ 
                      "Missing invoked templates: " <> tshow ts -- should not happen as we don't invoke templates
  where templateError msg = exitWith $ ReadFileError 
            ["*** TEMPLATE ERROR in:" <> absPath
            , msg
            ]
        appliedTemplate = setRuntimeAtts . setUserAtts (fromMaybe [] userAtts) $ template
        -- Set all attributes not specified to False
        fillInTheBlanks :: [String] -> StringTemplate String -> StringTemplate String
        fillInTheBlanks [] = id
        fillInTheBlanks (h:tl) = setAttribute h False . fillInTheBlanks tl
        setUserAtts :: [TemplateKeyValue]  -> (StringTemplate String -> StringTemplate String)
        setUserAtts kvPairs = foldl' fun id kvPairs
          where
            fun :: (Stringable b) => (StringTemplate b -> StringTemplate b) -> TemplateKeyValue -> (StringTemplate b -> StringTemplate b)
            fun soFar keyVal = soFar . doAttribute keyVal
            doAttribute :: (Stringable b) => TemplateKeyValue -> (StringTemplate b -> StringTemplate b)
            doAttribute h = case tkval h of
                Nothing ->  setAttribute (T.unpack $ tkkey h) True
                Just val -> setAttribute (T.unpack $ tkkey h) val
