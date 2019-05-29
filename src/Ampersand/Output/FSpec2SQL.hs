{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Ampersand.Output.FSpec2SQL
  (dumpSQLqueries,generateDatabaseFile)
where
import           Ampersand.ADL1
import           Ampersand.Basics
import           Ampersand.Core.ShowAStruct
import           Ampersand.FSpec
import           Ampersand.FSpec.SQL
import           Ampersand.Misc
import           Ampersand.Prototype.TableSpec
import           Ampersand.Prototype.ProtoUtil(getGenericsDir)
import qualified Data.List.NonEmpty as NEL
import qualified Data.Text as Text
import qualified RIO.List as L
import           System.Directory
import           System.FilePath

generateDatabaseFile :: MultiFSpecs -> RIO App ()
generateDatabaseFile multi = 
   do env <- ask
      let opts@Options{..} = getOptions env
      verboseLn $ "  Generating "++file
      liftIO $ createDirectoryIfMissing True (takeDirectory (fullFile opts))
      liftIO $ writeFile (fullFile opts) content
  where 
   content = Text.unpack (databaseStructureSql multi)
   file = "database" <.> "sql"
   fullFile opts = getGenericsDir opts </> file

databaseStructureSql :: MultiFSpecs -> Text.Text
databaseStructureSql multi
   = Text.intercalate "\n" $ 
         header (Text.pack ampersandVersionStr)
       <>header "Database structure queries"
       <>map (addSeparator . queryAsSQL) (generateDBstructQueries fSpec True) 
   where
     fSpec = userFSpec multi

generateDBstructQueries :: FSpec -> Bool -> [SqlQuery]
generateDBstructQueries fSpec withComment 
  =    concatMap (tableSpec2Queries withComment) ([plug2TableSpec p | InternalPlug p <- plugInfos fSpec])
    <> additionalDatabaseSettings 

dumpSQLqueries :: Options -> MultiFSpecs -> Text.Text
dumpSQLqueries opts@Options{..} multi
   = Text.intercalate "\n" $ 
         header (Text.pack ampersandVersionStr)
       <>header "Database structure queries"
       <>map (addSeparator . queryAsSQL) (generateDBstructQueries fSpec True) 
       <>header "Violations of conjuncts"
       <>concatMap showConjunct (allConjuncts fSpec)
       <>header "Queries per relation"
       <>concatMap showDecl (vrels fSpec)
       <>header "Queries of interfaces"
       <>concatMap showInterface y 
   where
     y :: [Interface]
     y = interfaceS fSpec <> interfaceG fSpec
     fSpec = userFSpec multi
     showInterface :: Interface -> [Text.Text]
     showInterface ifc 
        = header ("INTERFACE: "<>Text.pack (name ifc))
        <>(map ("  " <>) . showObjDef . ifcObj) ifc
        where 
          showObjDef :: ObjectDef -> [Text.Text]
          showObjDef obj
            = (header . Text.pack . showA . objExpression) obj
            <>[queryAsSQL . prettySQLQueryWithPlaceholder 2 fSpec . objExpression $ obj]
            <>case objmsub obj of
                 Nothing  -> []
                 Just sub -> showSubInterface sub
            <>header ("Broad query for the object at " <> (Text.pack . show . origin) obj)
            <>[Text.pack . prettyBroadQueryWithPlaceholder 2 fSpec $ obj]
          showSubInterface :: SubInterface -> [Text.Text]
          showSubInterface sub = 
            case sub of 
              Box{} -> concatMap showObjDef [e | BxExpr e <- siObjs sub]
              InterfaceRef{} -> []

     showConjunct :: Conjunct -> [Text.Text]
     showConjunct conj 
        = header (Text.pack$ rc_id conj)
        <>["/*"
          ,"Conjunct expression:"
          ,"  " <> (Text.pack . showA . rc_conjunct $ conj)
          ,"Rules for this conjunct:"]
        <>map showRule (NEL.toList $ rc_orgRules conj)
        <>["*/"
          ,(queryAsSQL . prettySQLQuery 2 fSpec . conjNF opts . notCpl . rc_conjunct $ conj) <> ";"
          ,""]
        where
          showRule r 
            = Text.pack ("  - "<>name r<>": "<>showA r)
     showDecl :: Relation -> [Text.Text]
     showDecl decl 
        = header (Text.pack$ showA decl)
        <>[(queryAsSQL . prettySQLQuery 2 fSpec $ decl)<>";",""]

header :: Text.Text -> [Text.Text]
header title = 
    [ "/*"
    , Text.replicate width "*"
    , "***"<>spaces firstspaces<>title<>spaces (width-6-firstspaces-l)<>"***"
    , Text.replicate width "*"
    , "*/"
    ]
  where 
    width = case L.maximumMaybe [80 , l + 8] of
              Nothing -> fatal "Impossible"
              Just x  -> x
    l = Text.length title
    spaces :: Int -> Text.Text
    spaces i = Text.replicate i " "
    firstspaces :: Int
    firstspaces = (width - 6 - l) `quot` 2 
addSeparator :: Text.Text -> Text.Text
addSeparator t = t <> ";"
