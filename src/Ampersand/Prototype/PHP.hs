{-# LANGUAGE OverloadedStrings #-}
module Ampersand.Prototype.PHP 
         ( evaluateExpSQL
         , signalTableSpec
         , sessionTableSpec
         , plug2TableSpec
         , getTableName
         , createTempDatabase
         , tempDbName
         , tableSpec2Queries
         , SqlQuery
         , sqlQuery2Text
         , additionalDatabaseSettings
         ) where

import Prelude hiding (exp,putStrLn,readFile,writeFile)
import Control.Exception
import Control.Monad
import Data.Monoid
import Data.List
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Process
import System.IO hiding (hPutStr,hGetContents,putStrLn,readFile,writeFile)
import System.Directory
import System.FilePath
import Ampersand.Prototype.ProtoUtil
import Ampersand.FSpec.SQL
import Ampersand.FSpec
import Ampersand.FSpec.ToFSpec.ADL2Plug(suitableAsKey)
import Ampersand.Basics
import Ampersand.Misc
import Ampersand.Core.AbstractSyntaxTree


data TableSpec
  = TableSpec { tsCmnt :: [String]  -- Without leading "// "
              , tsName :: String
              , tsflds :: [AttributeSpec]
              , tsKey ::  [String]
              }
data AttributeSpec
  = AttributeSpec { fsname :: Text.Text
                  , fstype :: TType
                  , fsIsPrimKey :: Bool
                  , fsDbNull :: Bool
                  }


getTableName :: TableSpec -> Text.Text
getTableName = Text.pack . tsName

createTablePHP :: TableSpec -> [Text.Text]
createTablePHP tSpec =
  map (Text.pack . ("// "<>)) (tsCmnt tSpec) <>
  [-- Drop table if it already exists
    "if($columns = mysqli_query($DB_link, "<>showPhpStr ("SHOW COLUMNS FROM `"<>Text.pack (tsName tSpec)<>"`")<>")){"
  , "    mysqli_query($DB_link, "<>showPhpStr ("DROP TABLE `"<>Text.pack (tsName tSpec)<>"`")<>");"
  , "}"
  ] <>
  [ "$sql="<>showPhpStr (Text.unlines $ createTableSql True tSpec)<>";"
  , "mysqli_query($DB_link,$sql);" 
  , "if($err=mysqli_error($DB_link)) {"
  , "  $error=true; echo $err.'<br />';"
  , "}"
  , ""
  ]

createTableSql :: Bool -> TableSpec -> [Text.Text]
createTableSql _withComment tSpec = 
      [ "CREATE TABLE `"<>Text.pack (tsName tSpec)<>"`"] <>
      [ Text.replicate indnt " " <> Text.pack [pref] <> " " <> addColumn att 
      | (pref, att) <- zip ('(' : repeat ',') (tsflds tSpec)] <>
      [ Text.replicate indnt " " <> "," <> doubleQuote "ts_insertupdate"<>" TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP"]<>
      [ Text.replicate indnt " " <> ") ENGINE     = InnoDB DEFAULT CHARACTER SET UTF8 COLLATE UTF8_BIN" ]<>
      [ Text.replicate indnt " " <> ", ROW_FORMAT = DYNAMIC"]
  where
    indnt = 23
    addColumn :: AttributeSpec -> Text.Text
    addColumn att 
       =    quote (fsname att) <> " " 
         <> (Text.pack . showSQL . fstype) att 
         <> (if fsIsPrimKey att then " UNIQUE" else "")
         <> (if fsDbNull att then " DEFAULT NULL" else " NOT NULL")


plug2TableSpec :: PlugSQL -> TableSpec
plug2TableSpec plug 
  = TableSpec 
     { tsCmnt = commentBlockSQL $
                   ["Plug "<>name plug
                   ,""
                   ,"attributes:"
                   ]<> concat
                   [ [showADL (attExpr x)
                     , "  "<>(show.properties.attExpr) x ]
                   | x <- plugAttributes plug
                   ]
     , tsName = name plug
     , tsflds = map fld2AttributeSpec $ plugAttributes plug
     , tsKey  = case (plug, (head.plugAttributes) plug) of
                 (BinSQL{}, _)   -> [  "PRIMARY KEY (" 
                                       <> intercalate ", " (map (show . attName) (plugAttributes plug))
                                       <> ")"
                                    | all (suitableAsKey . attType) (plugAttributes plug)
                                    ] 
                 (TblSQL{}, primFld) ->
                      case attUse primFld of
                         PrimaryKey _ -> ["PRIMARY KEY (" <> (show . attName) primFld <> ")" ]
                         ForeignKey c -> fatal 195 ("ForeignKey "<>name c<>"not expected here!")
                         PlainAttr    -> []
     }
fld2AttributeSpec ::SqlAttribute -> AttributeSpec
fld2AttributeSpec att 
  = AttributeSpec { fsname = Text.pack (name att)
                  , fstype = attType att
                  , fsIsPrimKey = isPrimaryKey att
                  , fsDbNull = attDBNull att 
                  }


signalTableSpec :: TableSpec
signalTableSpec =
    TableSpec { tsCmnt = ["Signal table"]
              , tsName = "__all_signals__"
              , tsflds = [ AttributeSpec 
                             { fsname      = "conjId"
                             , fstype      = Alphanumeric
                             , fsIsPrimKey = True
                             , fsDbNull    = False
                             }
                         , AttributeSpec 
                             { fsname      = "src"
                             , fstype      = Alphanumeric
                             , fsIsPrimKey = False
                             , fsDbNull    = False
                             }
                         , AttributeSpec 
                             { fsname      = "tgt"
                             , fstype      = Alphanumeric
                             , fsIsPrimKey = False
                             , fsDbNull    = False
                             }        
                         ]
              , tsKey =  ["PRIMARY KEY (`conjId`)" ]
              }

sessionTableSpec :: TableSpec
sessionTableSpec = 
    TableSpec { tsCmnt = ["Session timeout table"]
              , tsName = "__SessionTimeout__"
              , tsflds = [ AttributeSpec 
                             { fsname      = "SESSION"
                             , fstype      = Alphanumeric
                             , fsIsPrimKey = True
                             , fsDbNull    = False
                             }
                         , AttributeSpec 
                             { fsname      = "lastAccess"
                             , fstype      = Integer --HJO: Why not DateTime???
                             , fsIsPrimKey = False
                             , fsDbNull    = False
                             }
                         ]
              , tsKey =  ["PRIMARY KEY (`SESSION`)" ]
              }


-- evaluate normalized exp in SQL
evaluateExpSQL :: FSpec -> Text.Text -> Expression -> IO [(String,String)]
evaluateExpSQL fSpec dbNm exp =
 do { -- verboseLn (getOpts fSpec) ("evaluateExpSQL fSpec "++showADL exp)
    ; -- verboseLn (getOpts fSpec) (intercalate "\n" . showPrf showADL . cfProof (getOpts fSpec)) exp
    ; -- verboseLn (getOpts fSpec) "End of proof"
    ; performQuery fSpec dbNm (Text.pack violationsQuery)
    }
 where violationsExpr = conjNF (getOpts fSpec) exp
       violationsQuery = prettySQLQuery 26 fSpec violationsExpr

performQuery :: FSpec -> Text.Text -> Text.Text -> IO [(String,String)]
performQuery fSpec dbNm queryStr =
 do { queryResult <- (executePHPStr . showPHP) php
    ; if "Error" `isPrefixOf` queryResult -- not the most elegant way, but safe since a correct result will always be a list
      then do verboseLn opts{verboseP=True} (Text.unpack$ "\n******Problematic query:\n"<>queryStr<>"\n******")
              fatal 141 $ "PHP/SQL problem: "<>queryResult
      else case reads queryResult of
             [(pairs,"")] -> return pairs
             _            -> fatal 143 $ "Parse error on php result: \n"<>(unlines . indent 5 . lines $ queryResult)
    } 
   where
    opts = getOpts fSpec
    php :: [Text.Text]
    php =
      connectToMySqlServerPHP opts (Just dbNm) <>
      [ "$sql="<>showPhpStr queryStr<>";"
      , "$result=mysqli_query($DB_link,$sql);"
      , "if(!$result)"
      , "  die('Error '.($ernr=mysqli_errno($DB_link)).': '.mysqli_error($DB_link).'(Sql: $sql)');"
      , "$rows=Array();"
      , "  while ($row = mysqli_fetch_array($result)) {"
      , "    $rows[]=$row;"
      , "    unset($row);"
      , "  }"
      , "echo '[';"
      , "for ($i = 0; $i < count($rows); $i++) {"
      , "  if ($i==0) echo ''; else echo ',';"
      , "  echo '(\"'.addslashes($rows[$i]['src']).'\", \"'.addslashes($rows[$i]['tgt']).'\")';"
      , "}"
      , "echo ']';"
      ]

-- call the command-line php with phpStr as input
executePHPStr :: Text.Text -> IO String
executePHPStr phpStr =
 do { tempdir <- catch getTemporaryDirectory
                       (\e -> do let err = show (e :: IOException)
                                 hPutStr stderr ("Warning: Couldn't find temp directory. Using current directory : " <> err)
                                 return ".")
    ; (tempPhpFile, temph) <- openTempFile tempdir "tmpPhpQueryOfAmpersand.php"
    ; Text.hPutStr temph phpStr
    ; hClose temph
    ; results <- executePHP tempPhpFile
  --  ; removeFile tempPhpFile
    ; return (normalizeNewLines results)
    }
normalizeNewLines :: String -> String
normalizeNewLines = f . intercalate "\n" . lines
  where 
    f [] = []
    f ('\r':'\n':rest) = '\n':f rest
    f (c:cs) = c: f cs 

executePHP :: String -> IO String
executePHP phpPath =
 do { let cp = (shell command) 
                   { cwd = Just (takeDirectory phpPath)
                   }
          inputFile = phpPath
          outputFile = inputFile++"Result"
          command = "php "++show inputFile++" > "++show outputFile
    ; _ <- readCreateProcess cp ""
    ; result <- readFile outputFile
    ; removeFile outputFile
    ; return result
    }

showPHP :: [Text.Text] -> Text.Text
showPHP phpLines = Text.unlines $ ["<?php"]<>phpLines<>["?>"]


tempDbName :: Text.Text
tempDbName = "ampersandTempDB"

connectToMySqlServerPHP :: Options -> Maybe Text.Text-> [Text.Text]
connectToMySqlServerPHP opts mDbName =
    [ "// Try to connect to the MySQL server"
    , "global $DB_host,$DB_user,$DB_pass;"
    , "$DB_host='"<>subst sqlHost <>"';"
    , "$DB_user='"<>subst sqlLogin<>"';"
    , "$DB_pass='"<>subst sqlPwd  <>"';"
    , ""
    ]<>
    (case mDbName of
       Nothing   ->
         [ "$DB_link = mysqli_connect($DB_host,$DB_user,$DB_pass);"
         , "// Check connection"
         , "if (mysqli_connect_errno()) {"
         , "  die(\"Failed to connect to MySQL: \" . mysqli_connect_error());"
         , "}"
         , ""
         ]
       Just dbNm ->
         ["$DB_name='"<>dbNm<>"';"]<>
         connectToTheDatabasePHP
    ) <>
    [ "$sql=\"SET SESSION sql_mode = 'ANSI,TRADITIONAL'\";" -- ANSI because of the syntax of the generated SQL
                                                            -- TRADITIONAL because of some more safety
    , "if (!mysqli_query($DB_link,$sql)) {"
    , "  die(\"Error setting sql_mode: \" . mysqli_error($DB_link));"
    , "  }"
    , ""
    ]
  where
   subst :: (Options -> String) -> Text.Text
   subst x = addSlashes . Text.pack . x $ opts

connectToTheDatabasePHP :: [Text.Text]
connectToTheDatabasePHP =
    [ "// Connect to the database"
    , "$DB_link = mysqli_connect($DB_host,$DB_user,$DB_pass,$DB_name);"
    , "// Check connection"
    , "if (mysqli_connect_errno()) {"
    , "  die(\"Failed to connect to the database: \" . mysqli_connect_error());"
    , "  }"
    , ""
    ]

createTempDatabase :: FSpec -> IO ()
createTempDatabase fSpec =
 do { result <- executePHPStr .
           showPHP $ phpStr
    ; unless (null result) $ verboseLn (getOpts fSpec) result
    }
 where 
  phpStr :: [Text.Text]
  phpStr = 
    connectToMySqlServerPHP (getOpts fSpec) Nothing <>
    [ "/*** Set global varables to ensure the correct working of MySQL with Ampersand ***/"
    , ""
    , "    /* file_per_table is required for long columns */"
    , "    $result=mysqli_query($DB_link, \"SET GLOBAL innodb_file_per_table = true\");"
    , "       if(!$result)"
    , "         die('Error '.($ernr=mysqli_errno($DB_link)).': '.mysqli_error($DB_link).'(Sql: $sql)');"
    , "" 
    , "    /* file_format = Barracuda is required for long columns */"
    , "    $result=mysqli_query($DB_link, \"SET GLOBAL innodb_file_format = `Barracuda` \");"
    , "       if(!$result)"
    , "         die('Error '.($ernr=mysqli_errno($DB_link)).': '.mysqli_error($DB_link).'(Sql: $sql)');"
    , ""
    , "    /* large_prefix gives max single-column indices of 3072 bytes = win! */"
    , "    $result=mysqli_query($DB_link, \"SET GLOBAL innodb_large_prefix = true \");"
    , "       if(!$result)"
    , "         die('Error '.($ernr=mysqli_errno($DB_link)).': '.mysqli_error($DB_link).'(Sql: $sql)');"
    , ""
    ]<> 
    [ "$DB_name='"<>addSlashes (tempDbName)<>"';"
    , "// Drop the database if it exists"
    , "$sql=\"DROP DATABASE $DB_name\";"
    , "mysqli_query($DB_link,$sql);"
    , "// Don't bother about the error if the database didn't exist..."
    , ""
    , "// Create the database"
    , "$sql=\"CREATE DATABASE $DB_name DEFAULT CHARACTER SET UTF8 COLLATE utf8_bin\";"
    , "if (!mysqli_query($DB_link,$sql)) {"
    , "  die(\"Error creating the database: \" . mysqli_error($DB_link));"
    , "  }"
    , ""
    ] <> 
    connectToTheDatabasePHP <>       
    [ "/*** Create new SQL tables ***/"
    , ""
    ] <>
    createTablePHP sessionTableSpec <>
    createTablePHP signalTableSpec <>
    [ ""
    , "//// Number of plugs: " <> Text.pack (show (length (plugInfos fSpec)))
    ]
    -- Create all plugs
    <> concatMap (createTablePHP . plug2TableSpec) [p | InternalPlug p <- plugInfos fSpec]
    -- Populate all plugs
    <> concatMap populatePlugPHP [p | InternalPlug p <- plugInfos fSpec]
  
    where
      populatePlugPHP plug =
        case tableContents fSpec plug of
          [] -> []
          tblRecords 
             -> ( "mysqli_query($DB_link, "<>showPhpStr ( "INSERT INTO "<>quote (Text.pack (name plug))
                                                        <>" ("<>Text.intercalate "," [quote (Text.pack$ attName f) |f<-plugAttributes plug]<>")"
                                                        <>phpIndent 17<>"VALUES " <> Text.intercalate (phpIndent 22<>", ") [ "(" <>valuechain md<> ")" | md<-tblRecords]
                                                        <>phpIndent 16
                                                        )
                                           <>");"
                ):["if($err=mysqli_error($DB_link)) { $error=true; echo $err.'<br />'; }"]
       where
        valuechain record = Text.intercalate ", " [case att of Nothing -> "NULL" ; Just val -> showValPHP val | att<-record]


-- *** MySQL stuff below:

data SqlQuery = SqlQuery [Text.Text]

tableSpec2Queries :: Bool -> TableSpec -> [SqlQuery]
tableSpec2Queries withComment tSpec = 
 (SqlQuery $ createTableSql withComment tSpec 
 ):
 [SqlQuery [ Text.pack $ "CREATE INDEX "<> show (tsName tSpec<>"_"<>(Text.unpack . fsname) fld)
                             <>" ON "<>show (tsName tSpec)
                             <>" ("<>(show . Text.unpack . fsname) fld<>")"
           ]
 | fld <- tsflds tSpec
 , not (fsIsPrimKey fld)
 , suitableAsKey (fstype  fld)
 ]

additionalDatabaseSettings :: [SqlQuery]
additionalDatabaseSettings = [ SqlQuery ["SET TRANSACTION ISOLATION LEVEL SERIALIZABLE"]]

sqlQuery2Text :: Bool -> SqlQuery -> Text.Text
sqlQuery2Text withComment (SqlQuery ts)
   = if withComment 
     then Text.intercalate "\n" ts
     else Text.unwords . Text.words . Text.unlines $ ts

doubleQuote :: Text.Text -> Text.Text
doubleQuote s = "\"" <> s <> "\""

