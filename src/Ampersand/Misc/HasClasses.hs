﻿{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE UndecidableInstances #-}
module Ampersand.Misc.HasClasses

where
import Ampersand.Basics
import Ampersand.Misc.Defaults (defaultDirPrototype)
import RIO.FilePath
import qualified RIO.List as L
import qualified RIO.Text as T

class HasOptions a where
  showOptions :: HasLogFunc env => a -> RIO env ()
  showOptions opts = mapM_ showOpt . L.sortOn fst . optsList $ opts
    where showOpt :: HasLogFunc env => (Text,Text) -> RIO env ()
          showOpt (key,value) = 
            logDebug . display $ key <>" "<> value
  optsList :: a -> [(Text,Text)] -- A tuple containing the 'key' and the value of the options.   
  {-# MINIMAL optsList #-}
instance (HasOptions a, HasOptions b) => HasOptions (a,b) where
  optsList (a,b) = optsList a <> optsList b
--instance (HasOptions a, Foldable f, Functor f) => HasOptions (f a) where
--  optsList xs = concat . toList . fmap optsList $ xs

class HasFSpecGenOpts a where
  fSpecGenOptsL :: Lens' a FSpecGenOpts
  sqlBinTablesL :: Lens' a Bool
  sqlBinTablesL = fSpecGenOptsL . lens xsqlBinTables (\x y -> x { xsqlBinTables = y })
  genInterfacesL :: Lens' a Bool -- 
  genInterfacesL = fSpecGenOptsL . lens xgenInterfaces (\x y -> x { xgenInterfaces = y })
  namespaceL :: Lens' a Text -- prefix database identifiers with this namespace, to isolate namespaces within the same database.
  namespaceL = fSpecGenOptsL . lens xnamespace (\x y -> x { xnamespace = y })
  defaultCrudL :: Lens' a (Bool,Bool,Bool,Bool) -- Default values for CRUD functionality in interfaces
  defaultCrudL = fSpecGenOptsL . lens xdefaultCrud (\x y -> x { xdefaultCrud = y })
  trimXLSXCellsL :: Lens' a Bool
  trimXLSXCellsL = fSpecGenOptsL . lens xtrimXLSXCells (\x y -> x { xtrimXLSXCells = y })
  recipeL :: Lens' a Recipe
  recipeL = fSpecGenOptsL . lens xrecipe (\x y -> x { xrecipe = y })
  allowInvariantViolationsL :: Lens' a Bool
  allowInvariantViolationsL = fSpecGenOptsL . lens xallowInvariantViolations (\x y -> x { xallowInvariantViolations = y })
instance HasFSpecGenOpts FSpecGenOpts where
  fSpecGenOptsL = id
  {-# INLINE fSpecGenOptsL #-}

instance HasFSpecGenOpts DevOutputOpts where
  fSpecGenOptsL = lens x8fSpecGenOpts (\x y -> x { x8fSpecGenOpts = y })
instance HasFSpecGenOpts ValidateOpts where
  fSpecGenOptsL = protoOptsL . fSpecGenOptsL
instance HasFSpecGenOpts UmlOpts where
  fSpecGenOptsL = lens x7fSpecGenOpts (\x y -> x { x7fSpecGenOpts = y })
instance HasFSpecGenOpts ProofOpts where
  fSpecGenOptsL = lens x6fSpecGenOpts (\x y -> x { x6fSpecGenOpts = y })
instance HasFSpecGenOpts PopulationOpts where
  fSpecGenOptsL = lens x5fSpecGenOpts (\x y -> x { x5fSpecGenOpts = y })
instance HasFSpecGenOpts InputOutputOpts where
  fSpecGenOptsL = lens x4fSpecGenOpts (\x y -> x { x4fSpecGenOpts = y })
instance HasFSpecGenOpts DocOpts where
  fSpecGenOptsL = lens x3fSpecGenOpts (\x y -> x { x3fSpecGenOpts = y })
instance HasFSpecGenOpts DaemonOpts where
  fSpecGenOptsL = lens x2fSpecGenOpts (\x y -> x { x2fSpecGenOpts = y })
instance HasFSpecGenOpts ProtoOpts where
  fSpecGenOptsL = lens x1fSpecGenOpts (\x y -> x { x1fSpecGenOpts = y })
class (HasRootFile a) => HasDirPrototype a where
  dirPrototypeL :: Lens' a (Maybe FilePath)
  getTemplateDir :: a -> FilePath
  getTemplateDir x =
    getDirPrototype x </> "templates"
  getAppDir :: a -> FilePath
  getAppDir x =
    getDirPrototype x </> "public" </> "app" </> "project"
  getGenericsDir :: a -> FilePath
  getGenericsDir x =
    getDirPrototype x </> "generics"
  getMetamodelDir :: a -> FilePath
  getMetamodelDir x =
    getDirPrototype x </> "metamodel"
  getDirPrototype :: a -> FilePath
  getDirPrototype x = fromMaybe defaultDirPrototype . view dirPrototypeL $ x
instance HasDirPrototype ProtoOpts where
  dirPrototypeL = lens xdirPrototype (\x y -> x { xdirPrototype = y })

class HasGenerateFrontend a where
  generateFrontendL :: Lens' a Bool
instance HasGenerateFrontend ProtoOpts where
  generateFrontendL = lens xgenerateFrontend (\x y -> x { xgenerateFrontend = y })
class HasGenerateBackend a where
  generateBackendL :: Lens' a Bool
instance HasGenerateBackend ProtoOpts where
  generateBackendL = lens xgenerateBackend (\x y -> x { xgenerateBackend = y })
class HasGenerateMetamodel a where
  generateMetamodelL :: Lens' a Bool
instance HasGenerateMetamodel ProtoOpts where
  generateMetamodelL = lens xgenerateMetamodel (\x y -> x { xgenerateMetamodel = y })

class HasRootFile a where
  rootFileL :: Lens' a (Maybe FilePath)
  baseName :: a -> FilePath
  baseName  =
    maybe
      (fatal "Cannot determine the basename of the script that is being compiled")
      takeBaseName
    . view rootFileL
  dirSource :: a -> FilePath -- the directory of the script that is being compiled
  dirSource =
    maybe
      (fatal "Cannot determine the directory of the script that is being compiled")
      takeDirectory
    . view rootFileL
instance HasFSpecGenOpts a => HasRootFile a where
  rootFileL = fSpecGenOptsL . lens xrootFile (\x y -> x { xrootFile = y })

class HasOutputLanguage a where
  languageL :: Lens' a (Maybe Lang)  -- The language in which the user wants the documentation to be printed.
instance HasOutputLanguage ProtoOpts where
  languageL = lens x1OutputLanguage (\x y -> x { x1OutputLanguage = y })
instance HasOutputLanguage DocOpts where
  languageL = lens x3OutputLanguage (\x y -> x { x3OutputLanguage = y })
instance HasOutputLanguage UmlOpts where
  languageL = lens x4OutputLanguage (\x y -> x { x4OutputLanguage = y })

class HasShowWarnings a where
  showWarningsL :: Lens' a Bool  -- Should warnings be given to the output?
instance HasDaemonOpts a => HasShowWarnings a where
  showWarningsL = daemonOptsL . lens xshowWarnings (\x y -> x { xshowWarnings = y })

class HasDirCustomizations a where
  dirCustomizationsL :: Lens' a (Maybe [FilePath]) -- the directories that are copied after generating the prototype
instance HasDirCustomizations ProtoOpts where
  dirCustomizationsL = lens xdirCustomizations (\x y -> x { xdirCustomizations = y })

class HasZwolleVersion a where
  zwolleVersionL :: Lens' a FilePath -- the version in github of the prototypeFramework. can be a tagname, a branchname or a SHA
instance HasZwolleVersion ProtoOpts where
  zwolleVersionL = lens xzwolleVersion (\x y -> x { xzwolleVersion = y })

class HasDirOutput a where
  dirOutputL :: Lens' a FilePath -- the directory to generate the output in.

class HasOutputLanguage a => HasDocumentOpts a where
  documentOptsL :: Lens' a DocOpts
  chaptersL :: Lens' a [Chapter]
  chaptersL = documentOptsL . lens xchapters (\x y -> x { xchapters = y })
  fspecFormatL :: Lens' a FSpecFormat -- the format of the generated (pandoc) document(s)
  fspecFormatL = documentOptsL . lens xfspecFormat (\x y -> x { xfspecFormat = y })
  genLegalRefsL :: Lens' a Bool   -- Generate a table of legal references in Natural Language chapter
  genLegalRefsL = documentOptsL . lens xgenLegalRefs (\x y -> x { xgenLegalRefs = y })
  genGraphicsL :: Lens' a Bool -- Generate graphics during generation of functional design document.
  genGraphicsL = documentOptsL . lens xgenGraphics (\x y -> x { xgenGraphics = y })

instance HasDocumentOpts DocOpts where
  documentOptsL = id

class HasBlackWhite a where
  blackWhiteL :: Lens' a Bool    -- only use black/white in graphics
instance HasBlackWhite DocOpts where
  blackWhiteL = lens xblackWhite (\x y -> x { xblackWhite = y })

class HasOutputFile a where
  outputfileL :: Lens' a FilePath
instance HasOutputFile InputOutputOpts where
  outputfileL = lens x4outputFile (\x y -> x { x4outputFile = y })

class HasVersion a where
  preVersionL :: Lens' a Text
  postVersionL :: Lens' a Text

class HasProtoOpts env where
   protoOptsL :: Lens' env ProtoOpts
   forceReinstallFrameworkL :: Lens' env Bool
   forceReinstallFrameworkL
             = protoOptsL . lens xforceReinstallFramework (\x y -> x { xforceReinstallFramework = y })
instance HasProtoOpts ProtoOpts where
   protoOptsL = id
   {-# INLINE protoOptsL #-}
instance HasProtoOpts ValidateOpts where
   protoOptsL = lens protoOpts (\x y -> x { protoOpts = y })
class HasDevoutputOpts env where
   devoutputOptsL :: Lens' env DevOutputOpts
class HasInitOpts env where
   initOptsL :: Lens' env InitOpts
class HasProofOpts env where
   proofOptsL :: Lens' env ProofOpts
class HasPopulationOpts env where
   populationOptsL :: Lens' env PopulationOpts
   outputFormatL :: Lens' env PopulationOutputFormat
   outputFormatL = populationOptsL . lens xoutputFormat (\x y -> x { xoutputFormat = y })
class HasValidateOpts env where
   validateOptsL :: Lens' env ValidateOpts
class HasTestOpts env where
   testOptsL :: Lens' env TestOpts
instance HasTestOpts TestOpts where
   testOptsL = id
   {-# INLINE testOptsL #-}

-- | Options for @ampersand daemon@.
data DaemonOpts = DaemonOpts
  { xdaemonConfig :: !FilePath
   -- ^ The path (relative from current directory OR absolute) and filename of a file that contains the root file(s) to be watched by the daemon.
  , x2fSpecGenOpts :: !FSpecGenOpts
  , xshowWarnings :: !Bool -- ^ Enable/disable show of warnings (if any).

  }
instance HasOptions DaemonOpts where
  optsList opts =
     [ ("--daemonconfig", tshow $ xdaemonConfig opts)
     ] <>
     optsList (x2fSpecGenOpts opts) <>
     [ ("--[no-]warnings", tshow $ xshowWarnings opts)
     ]

class (HasFSpecGenOpts a) => HasDaemonOpts a where
  daemonOptsL :: Lens' a DaemonOpts
  daemonConfigL :: Lens' a FilePath
  daemonConfigL = daemonOptsL . lens xdaemonConfig (\x y -> x { xdaemonConfig = y })
instance HasDaemonOpts DaemonOpts where
  daemonOptsL = id
  {-# INLINE daemonOptsL #-}

-- | An enumeration type for building an FSpec in some common way
data Recipe =
    Standard  -- ^ Plain way of building. No fancy stuff. 
  | Grind     -- ^ Generates population for an atlas.
              --   It assumes that the database is fit to receive that population, as RAP does.
  | Prototype -- ^ A recipe to build a prototyping environment.
  | RAP       -- ^ A recipe to build a Repository for Ampersand Projects (RAP)
              --   The option 'RAP' generates a database that is fit to receive metamodels, so an Atlas is possible.
              --   The 'makeAtlas' button in RAP uses the 'Grind' option to populate the metamodel.
    deriving (Show, Enum, Bounded)

data FSpecGenOpts = FSpecGenOpts
  { xrootFile :: !(Maybe FilePath)  --relative path. Must be set the first time it is read.
  , xsqlBinTables :: !Bool
  , xgenInterfaces :: !Bool -- 
  , xnamespace :: !Text -- prefix database identifiers with this namespace, to isolate namespaces within the same database.
  , xdefaultCrud :: !(Bool,Bool,Bool,Bool)
  , xtrimXLSXCells :: !Bool
  , xrecipe :: !Recipe
  -- ^ Which recipe for generating code?
  , xallowInvariantViolations :: !Bool
  -- ^ Should invariant violations be ignored?
} deriving Show
instance HasOptions FSpecGenOpts where
  optsList opts =
     [ ("AMPERSAND_SCRIPT", maybe "" T.pack $ xrootFile opts)
     , ("--sql-bin-tables", tshow $ xsqlBinTables opts)
     , ("--interfaces", tshow $ xgenInterfaces opts)
     , ("--namespace", tshow $ xnamespace opts)
     , ("--crud-defaults", let (c,r,u,d) = xdefaultCrud opts
                               f :: Bool -> Text -> Text
                               f b = (if b then T.toUpper else T.toLower)  
                           in mconcat [f c "c", f r "r",f u "u",f d "d"]
       )
     , ("--[no-]trim-cellvalues", tshow $ xtrimXLSXCells opts)
     , ("--build-recipe", tshow $ xrecipe opts)
     , ("--ignore-invariant-violations", tshow $ xallowInvariantViolations opts)
     ]

data FSpecFormat =
         FPandoc
       | Fasciidoc
       | Fcontext
       | Fdocbook
       | Fdocx
       | Fhtml
       | Fman
       | Fmarkdown
       | Fmediawiki
       | Fopendocument
       | Forg
       | Fpdf
       | Fplain
       | Frst
       | Frtf
       | Flatex
       | Ftexinfo
       | Ftextile
       deriving (Show, Eq, Enum, Bounded)

-- | Options for @ampersand export@.
newtype ExportOpts = ExportOpts
   { xexport2adl :: FilePath  --relative path
   }
-- | Options for @ampersand dataAnalysis@ and @ampersand export@.
data InputOutputOpts = InputOutputOpts
   { x4fSpecGenOpts :: !FSpecGenOpts
   , x4outputFile :: !FilePath --relative path 
   }
instance HasOptions InputOutputOpts where
  optsList opts =
     optsList (x4fSpecGenOpts opts) <>
     [ ("OUTPUTDIRECTORY", tshow $ x4outputFile opts)
     ]

-- | Options for @ampersand proto@.
data ProtoOpts = ProtoOpts
   { xforceReinstallFramework :: !Bool
   -- ^ when true, an existing prototype directory will be destroyed and re-installed
   , x1OutputLanguage :: !(Maybe Lang)
   , x1fSpecGenOpts :: !FSpecGenOpts
   , xdirPrototype :: !(Maybe FilePath)
   , xdirCustomizations :: !(Maybe [FilePath])
   , xzwolleVersion :: !FilePath
   , xgenerateFrontend :: !Bool
   , xgenerateBackend :: !Bool
   , xgenerateMetamodel :: !Bool
  } deriving Show
instance HasOptions ProtoOpts where
  optsList opts =
     [ ("--force-reinstall-framework", tshow $ xforceReinstallFramework opts)
     , ("--language", tshow $ x1OutputLanguage opts)
     ] <>
     optsList (x1fSpecGenOpts opts) <>
     [ ("--proto-dir", maybe "" tshow $ xdirPrototype opts)
     , ("--customizations", maybe "" (T.intercalate "; " . fmap T.pack) $ xdirCustomizations opts)
     , ("--prototype-framework-version", tshow $ xzwolleVersion opts)
     , ("--[no-]frontend", tshow $ xgenerateFrontend opts)
     , ("--[no-]backend", tshow $ xgenerateBackend opts)
     , ("--[no-]metamodel", tshow $ xgenerateMetamodel opts)
     ]

-- | Options for @ampersand documentation@.
data DocOpts = DocOpts
   { xblackWhite :: !Bool
   -- ^ avoid coloring conventions to facilitate readable pictures in black and white.
   , xchapters :: ![Chapter]
   -- ^ a list containing all chapters that are required to be in the generated documentation
   , xgenGraphics :: !Bool
   -- ^ enable/disable generation of graphics while generating documentation
   , xfspecFormat :: !FSpecFormat
   -- ^ the format of the documentation 
   , x3fSpecGenOpts :: !FSpecGenOpts
   -- ^ Options required to build the fSpec
   , x3OutputLanguage :: !(Maybe Lang)
   -- ^ Language of the output document
   , xgenLegalRefs :: !Bool
   -- ^ enable/disable generation of legal references in the documentation
   } deriving Show
instance HasOptions DocOpts where
  optsList opts =
     [ ("--blackWhite", tshow $ xblackWhite opts)
     ] <> 
     fmap chapters [minBound ..] <>
     [ ("--[no-]graphics", tshow $ xgenGraphics opts)
     , ("--format", tshow $ xfspecFormat opts)
     ] <>
     optsList (x3fSpecGenOpts opts) <>
     [ ("--language", tshow $ x3OutputLanguage opts)
     , ("--[no-]legal-refs", tshow $ xgenLegalRefs opts)
     ]
    where
      chapters :: Chapter -> (Text,Text)
      chapters chp = ("--[no-]"<> tshow chp,tshow $ chp `elem` xchapters opts)
data PopulationOutputFormat =
    XLSX
  | JSON
  deriving (Show, Enum, Bounded)
-- | Options for @ampersand population@
data PopulationOpts = PopulationOpts
   { x5fSpecGenOpts :: !FSpecGenOpts
   -- ^ Options required to build the fSpec
   , xoutputFormat :: !PopulationOutputFormat
   } deriving Show
instance HasOptions PopulationOpts where
  optsList opts =
     optsList (x5fSpecGenOpts opts) <>
     [ ("--output-format", tshow $ xoutputFormat opts)
     ]

instance HasPopulationOpts PopulationOpts where
   populationOptsL = id
   outputFormatL = populationOptsL . lens xoutputFormat (\x y -> x { xoutputFormat = y })
   {-# INLINE populationOptsL #-}
-- | Options for @ampersand proofs@
newtype ProofOpts = ProofOpts
   { x6fSpecGenOpts :: FSpecGenOpts
   -- ^ Options required to build the fSpec
   } deriving Show
instance HasOptions ProofOpts where
  optsList opts =
     optsList (x6fSpecGenOpts opts)

-- | Options for @ampersand init@
data InitOpts = InitOpts
   deriving Show
-- | Options for @ampersand uml@
data UmlOpts = UmlOpts
   { x7fSpecGenOpts :: !FSpecGenOpts
   -- ^ Options required to build the fSpec
   , x4OutputLanguage :: !(Maybe Lang)
   -- ^ Language of the output document
   } deriving Show
instance HasOptions UmlOpts where
  optsList opts =
     optsList (x7fSpecGenOpts opts) <>
     [ ("--language", tshow $ x4OutputLanguage opts)
     ]

-- | Options for @ampersand validate@
newtype ValidateOpts = ValidateOpts
   { protoOpts :: ProtoOpts
   -- ^ Options required to build the fSpec
   } deriving Show
instance HasOptions ValidateOpts where
  optsList opts =
     optsList (protoOpts opts)

-- | Options for @ampersand devoutput@
data DevOutputOpts = DevOutputOpts
   { x8fSpecGenOpts :: !FSpecGenOpts
   -- ^ Options required to build the fSpec
   , x5outputFile :: !FilePath --relative path  
   } deriving Show
instance HasOptions DevOutputOpts where
  optsList opts =
     optsList (x8fSpecGenOpts opts) <>
     [ ("OUTPUTDIRECTORY", tshow $ x5outputFile opts)
     ]

newtype TestOpts = TestOpts
   { rootTestDir :: FilePath --relative path to directory containing test scripts
   } deriving Show
instance HasOptions TestOpts where
  optsList opts =
     [ ("TESTDIRECTORY", tshow $ rootTestDir opts)
     ]
data Chapter = Intro
             | SharedLang
             | Diagnosis
             | ConceptualAnalysis
             | DataAnalysis
             deriving (Eq, Show, Enum, Bounded)

