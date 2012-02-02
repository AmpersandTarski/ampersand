{-# OPTIONS_GHC -Wall -XPatternGuards #-}
module DatabaseDesign.Ampersand.Misc.Options 
        (Options(..),getOptions,defaultFlags,usageInfo'
        ,ParserVersion(..)
        ,verboseLn,verbose,FspecFormat(..),ImportFormat(..)
        ,DocTheme(..),allFspecFormats,helpNVersionTexts)
where
import System                (getArgs, getProgName)
import System.Environment    (getEnvironment)
import DatabaseDesign.Ampersand.Misc.Languages (Lang(..))
import Data.Char (toUpper)
import System.Console.GetOpt
import System.FilePath
import System.Directory
import Time
import Control.Monad
import Data.Maybe
import DatabaseDesign.Ampersand.Basics  
import Prelude hiding (writeFile,readFile,getContents,putStr,putStrLn)
import Data.List

fatal :: Int -> String -> a
fatal = fatalMsg "Misc.Options"

data ParserVersion = PV211 | PV664
instance Show ParserVersion where
  show PV211 = "syntax since Ampersand 2.1.1."
  show PV664 = "syntax664"

-- | This data constructor is able to hold all kind of information that is useful to 
--   express what the user would like Ampersand to do. 
data Options = Options { showVersion   :: Bool
                       , preVersion    :: String
                       , postVersion   :: String  --built in to aid DOS scripting... 8-(( Bummer. 
                       , showHelp      :: Bool
                       , verboseP      :: Bool
                       , development   :: Bool
                       , genPrototype  :: Bool 
                       , autoid        :: Bool --implies forall Concept A => value::A->Datatype [INJ]. where instances of A are autogenerated 
                       , dirPrototype  :: String  -- the directory to generate the prototype in.
                       , allInterfaces :: Bool
                       , dbName        :: String
                       , genAtlas      :: Bool
                       , namespace     :: String
                       , deprecated    :: Bool
                       , autoRefresh   :: Maybe Int
                       , testRule      :: Maybe String                       
                       , customCssFile :: Maybe FilePath                       
                       , importfile    :: FilePath --a file with content to populate some (Populated a)
                                                   --class Populated a where populate::a->b->a
                       , importformat  :: ImportFormat --file format that can be parsed to some b to populate some Populated a 
                       , theme         :: DocTheme --the theme of some generated output. (style, content differentiation etc.)
                       , genXML        :: Bool
                       , genFspec      :: Bool   -- if True, generate a functional specification
                       , diag          :: Bool   -- if True, generate a diagnosis only
                       , fspecFormat   :: FspecFormat
                       , genGraphics   :: Bool   -- if True, graphics will be generated for use in Ampersand products like the Atlas or Functional Spec
                       , genEcaDoc     :: Bool   -- if True, generate ECA rules in the Functional Spec
                       , proofs        :: Bool
                       , haskell       :: Bool   -- if True, generate the ECps-structure as a Haskell source file
                       , dirOutput     :: String -- the directory to generate the output in.
                       , outputfile    :: String -- the file to generate the output in.
                       , crowfoot      :: Bool   -- if True, generate conceptual models and data models in crowfoot notation
                       , blackWhite    :: Bool   -- only use black/white in graphics
                       , showPredExpr  :: Bool   -- for generated output, show predicate logic?
                       , noDiagnosis   :: Bool   -- omit the diagnosis chapter from the functional specification document
                       , diagnosisOnly :: Bool   -- give a diagnosis only (by omitting the rest of the functional specification document)
                       , genLegalRefs  :: Bool   -- Generate a table of legal references in Natural Language chapter
                       , genUML        :: Bool   -- Generate a UML 2.0 data model
                       , language      :: Lang
                       , dirExec       :: String --the base for relative paths to input files
                       , progrName     :: String --The name of the adl executable
                       , fileName      :: FilePath --the file with the Ampersand context
                       , baseName      :: String
                       , logName       :: String
                       , genTime       :: ClockTime
                       , interfacesG   :: Bool
                       , test          :: Bool
                       , pangoFont     :: String  -- use specified font in PanDoc. May be used to avoid pango-warnings.
                       , sqlHost       :: Maybe String  -- do database queries to the specified host
                       , sqlLogin      :: Maybe String  -- pass login name to the database server
                       , sqlPwd        :: Maybe String  -- pass password on to the database server
                       , forcedParserVersion :: Maybe ParserVersion
                       } deriving Show
    
defaultFlags :: Options 
defaultFlags = Options {genTime       = fatal 81 "No monadic options available."
                      , dirOutput     = fatal 82 "No monadic options available."
                      , outputfile    = fatal 83 "No monadic options available."
                      , autoid        = False
                      , dirPrototype  = fatal 84 "No monadic options available."
                      , dbName        = fatal 85 "No monadic options available."
                      , logName       = fatal 86 "No monadic options available."
                      , dirExec       = fatal 87 "No monadic options available."
                      , preVersion    = fatal 88 "No monadic options available."
                      , postVersion   = fatal 89 "No monadic options available."
                      , theme         = DefaultTheme
                      , showVersion   = False
                      , showHelp      = False
                      , verboseP      = False
                      , development   = False
                      , genPrototype  = False
                      , allInterfaces = False
                      , genAtlas      = False   
                      , namespace     = []
                      , deprecated    = False
                      , autoRefresh   = Nothing
                      , testRule      = Nothing
                      , customCssFile = Nothing
                      , importfile    = []
                      , importformat  = fatal 101 "--importformat is required for --import."
                      , genXML        = False
                      , genFspec      = False 
                      , diag          = False 
                      , fspecFormat   = fatal 105 $ "Unknown fspec format. Currently supported formats are "++allFspecFormats++"."
                      , genGraphics   = True
                      , genEcaDoc     = False
                      , proofs        = False
                      , haskell       = False
                      , crowfoot      = False
                      , blackWhite    = False
                      , showPredExpr  = False
                      , noDiagnosis   = False
                      , diagnosisOnly = False
                      , genLegalRefs  = False
                      , genUML        = False
                      , language      = Dutch
                      , progrName     = fatal 118 "No monadic options available."
                      , fileName      = fatal 119 "no default value for fileName."
                      , baseName      = fatal 120 "no default value for baseName."
                      , interfacesG   = False
                      , test          = False
                      , pangoFont     = "Sans"
                      , sqlHost       = Nothing
                      , sqlLogin      = Nothing
                      , sqlPwd        = Nothing
                      , forcedParserVersion = Nothing
                      }
                
getOptions :: IO Options
getOptions =
   do args     <- getArgs
      progName <- getProgName
      defaultOpts <- defaultOptionsM
      (flags,fNames)  <- case getOpt Permute (each options) args of
                         ([],[],[])   -> return (defaultOpts{showHelp=True} ,[])
                         (o ,n ,[])   -> return (foldl (flip id) defaultOpts o ,n)
                         (_, _, errs) -> error $ concat errs ++ "Type '"++ progName++" --help' for usage info."
      checkNSetOptionsAndFileNameM (flags,fNames)
  where 
     defaultOptionsM :: IO Options 
     defaultOptionsM  =
           do clocktime <- getClockTime
              progName <- getProgName
              exePath <- findExecutable progName
              env <- getEnvironment
              return
               defaultFlags
                      { genTime       = clocktime
                      , dirOutput     = fromMaybe "."       (lookup envdirOutput    env)
                      , dirPrototype  = fromMaybe "."       (lookup envdirPrototype env)
                      , dbName        = fromMaybe ""        (lookup envdbName       env)
                      , logName       = fromMaybe "Ampersand.log" (lookup envlogName      env)
                      , dirExec       = case exePath of
                                          Nothing -> fatal 155 $ "Specify the path location of "++progName++" in your system PATH variable."
                                          Just s  -> takeDirectory s
                      , preVersion    = fromMaybe ""        (lookup "CCPreVersion"  env)
                      , postVersion   = fromMaybe ""        (lookup "CCPostVersion" env)
                      , progrName     = progName
                      }



     checkNSetOptionsAndFileNameM :: (Options,[String]) -> IO Options 
     checkNSetOptionsAndFileNameM (flags,fNames) = 
          if showVersion flags || showHelp flags 
          then return flags 
          else case fNames of
                []      -> fatal 171 $ "no file to parse" ++useHelp
                [fName] -> verboseLn flags "Checking output directories..."
                        >> checkLogName flags
                        >> checkDirOutput flags
                        --REMARK -> checkExecOpts in comments because it is redundant
                        --          it may throw fatals about PATH not set even when you do not need the dir of the executable.
                        --          if you need the dir of the exec, then you should use (dirExec flags) which will throw the fatal about PATH when needed.
                        -- >> checkExecOpts flags
                        >> checkProtoOpts flags
                        >> return flags { fileName    = if hasExtension fName
                                                         then fName
                                                         else addExtension fName "adl" 
                                        , baseName    = takeBaseName fName
                                        , dbName      = case dbName flags of
                                                            ""  -> takeBaseName fName
                                                            str -> str
                                        , genAtlas = not (null(importfile flags)) && importformat flags==Adl1Format
                                        , importfile  = if null(importfile flags) || hasExtension(importfile flags)
                                                        then importfile flags
                                                        else case importformat flags of 
                                                                Adl1Format -> addExtension (importfile flags) "adl"
                                                                Adl1PopFormat -> addExtension (importfile flags) "pop"
                                        }
                x:xs    -> fatal 191 $ "too many files: "++ intercalate ", " (x:xs) ++useHelp
       
       where
          useHelp :: String
          useHelp = " (use --help for help) "
          checkLogName :: Options -> IO ()
          checkLogName   f = createDirectoryIfMissing True (takeDirectory (logName f))
          checkDirOutput :: Options -> IO ()
          checkDirOutput f = createDirectoryIfMissing True (dirOutput f)

          --checkExecOpts :: Options -> IO ()
          --checkExecOpts f = do execPath <- findExecutable (progrName f) 
            --                   when (execPath == Nothing) 
              --                      (fatal 206 $ "Specify the path location of "++(progrName f)++" in your system PATH variable.")
          checkProtoOpts :: Options -> IO ()
          checkProtoOpts f = when (genPrototype f) (createDirectoryIfMissing True (dirPrototype f))
            
data DisplayMode = Public | Hidden 
data FspecFormat = FPandoc | FRtf | FOpenDocument | FLatex | FHtml  deriving (Show, Eq)
data ImportFormat = Adl1Format | Adl1PopFormat  deriving (Show, Eq) --file format that can be parsed to some b to populate some Populated a
data DocTheme = DefaultTheme   -- Just the functional specification
              | ProofTheme     -- A document with type inference proofs
              | StudentTheme   -- An adjusted func spec for students of the business rules course
                 deriving (Show, Eq)
    
usageInfo' :: Options -> String
-- When the user asks --help, then the public options are listed. However, if also --verbose is requested, the hidden ones are listed too.  
usageInfo' opts = usageInfo (infoHeader (progrName opts)) (if verboseP opts then each options else publics options)
          
infoHeader :: String -> String
infoHeader progName = "\nUsage info:\n " ++ progName ++ " options file ...\n\nList of options:"

publics :: [(a, DisplayMode) ] -> [a]
publics opts = [o | (o,Public)<-opts]
each :: [(a, DisplayMode) ] -> [a]
each opts = [o |(o,_) <- opts]

options :: [(OptDescr (Options -> Options), DisplayMode) ]
options = map pp
          [ (Option "v"     ["version"]     (NoArg versionOpt)          "show version and exit.", Public)
          , (Option "h?"    ["help"]        (NoArg helpOpt)             "get (this) usage information.", Public)
          , (Option ""      ["verbose"]     (NoArg verboseOpt)          "verbose error message format.", Public)
          , (Option ""      ["dev"]         (NoArg developmentOpt)      "Report and generate extra development information", Hidden)
          , (Option "p"     ["proto"]       (OptArg prototypeOpt "dir") ("generate a functional prototype (overwrites environment variable "
                                                                           ++ envdirPrototype ++ ")."), Public)
          , (Option "d"     ["dbName"]      (ReqArg dbNameOpt "name")   ("database name (overwrites environment variable "
                                                                           ++ envdbName ++ ", defaults to filename)"), Public)
          , (Option []      ["theme"]       (ReqArg themeOpt "theme")   "differentiate between certain outputs e.g. student", Public)
          , (Option "x"     ["interfaces"]  (NoArg maxInterfacesOpt)    "generate interfaces.", Public)
          , (Option "e"     ["export"]      (OptArg interfacesOpt "file") "export as ASCII Ampersand syntax.", Public)
          , (Option "o"     ["outputDir"]   (ReqArg outputDirOpt "dir") ("output directory (dir overwrites environment variable "
                                                                           ++ envdirOutput ++ ")."), Public)
          , (Option []      ["log"]         (ReqArg logOpt "name")      ("log file name (name overwrites environment variable "
                                                                           ++ envlogName  ++ ")."), Hidden)
          , (Option []      ["import"]      (ReqArg importOpt "file")   "import this file as the population of the context.", Public)
          , (Option []      ["importformat"](ReqArg iformatOpt "format")("format of import file (format="
                                                                           ++allImportFormats++")."), Public)
          , (Option []      ["namespace"]   (ReqArg namespaceOpt "ns")  "places the population in this namespace within the context.", Public)
          , (Option "f"     ["fspec"]       (ReqArg fspecRenderOpt "format")  
                                                                         ("generate a functional specification document in specified format (format="
                                                                         ++allFspecFormats++")."), Public)
          , (Option []        ["deprecated"]  (NoArg (\opts -> opts{deprecated = True})) "Force generation of old php prototype (strongly discouraged!)", Hidden)
          , (Option []        ["refresh"]     (OptArg autoRefreshOpt "interval") "Experimental auto-refresh feature", Hidden)
          , (Option []        ["testRule"]    (ReqArg (\ruleName opts -> opts{ testRule = Just ruleName }) "rule name")
                                                                          "Show contents and violations of specified rule.", Hidden)
          , (Option []        ["css"]         (ReqArg (\pth opts -> opts{ customCssFile = Just pth }) "file")
                                                                          "Custom.css file to customize the style of the prototype.", Public)
          , (Option []        ["noGraphics"]  (NoArg noGraphicsOpt)       "save compilation time by not generating any graphics.", Public)
          , (Option []        ["ECA"]         (NoArg genEcaDocOpt)        "generate documentation with ECA rules.", Public)
          , (Option []        ["proofs"]      (NoArg proofsOpt)           "generate derivations.", Public)
          , (Option []        ["XML"]         (NoArg xmlOpt)              "generate internal data structure, written in XML (for debugging).", Public)
          , (Option []        ["haskell"]     (NoArg haskellOpt)          "generate internal data structure, written in Haskell (for debugging).", Public)
          , (Option []        ["crowfoot"]    (NoArg crowfootOpt)         "generate crowfoot notation in graphics.", Public)
          , (Option []        ["blackWhite"]  (NoArg blackWhiteOpt)       "do not use colours in generated graphics", Public)
          , (Option []        ["predLogic"]   (NoArg predLogicOpt)        "show logical expressions in the form of predicate logic." , Public)
          , (Option []        ["noDiagnosis"] (NoArg noDiagnosisOpt)      "omit the diagnosis chapter from the functional specification document." , Public)
          , (Option []        ["diagnosis"]   (NoArg diagnosisOpt)        "diagnose your Ampersand script (generates a .pdf file).", Public)
          , (Option []        ["legalrefs"]   (NoArg (\opts -> opts{genLegalRefs = True}))
                                                                          "generate a table of legal references in Natural Language chapter.", Public)
          , (Option []        ["uml"]         (NoArg (\opts -> opts{genUML = True}))
                                                                          "Generate a UML 2.0 data model.", Hidden)
          , (Option []        ["language"]    (ReqArg languageOpt "lang") "language to be used, ('NL' or 'EN').", Public)
          , (Option []        ["test"]        (NoArg testOpt)             "Used for test purposes only.", Hidden)

          , (Option []        ["pango"]       (OptArg pangoOpt "fontname") "specify font name for Pango in graphics.", Hidden)
          , (Option []        ["sqlHost"]     (OptArg sqlHostOpt "name")  "specify database host name.", Hidden)
          , (Option []        ["sqlLogin"]    (OptArg sqlLoginOpt "name") "specify database login name.", Hidden)
          , (Option []        ["sqlPwd"]      (OptArg sqlPwdOpt "str")    "specify database password.", Hidden)
          , (Option []        ["forceSyntax"] (ReqArg forceSyntaxOpt "versionNumber") "version number of the syntax to be used, ('1' or '2'). Without this, ampersand will guess the version used.", Public) 
          ]
     where pp :: (OptDescr (Options -> Options), DisplayMode) -> (OptDescr (Options -> Options), DisplayMode)
           pp (Option a b' c d,e) = (Option a b' c d',e)
              where d' =  afkappen [] [] (words d) 40
                    afkappen :: [[String]] -> [String] -> [String] -> Int -> String
                    afkappen regels []    []   _ = intercalate "\n" (map unwords regels)
                    afkappen regels totnu []   b = afkappen (regels++[totnu]) [] [] b
                    afkappen regels totnu (w:ws) b 
                          | length (unwords totnu) < b - length w = afkappen regels (totnu++[w]) ws b
                          | otherwise                             = afkappen (regels++[totnu]) [w] ws b     
           
                    
envdirPrototype :: String
envdirPrototype = "CCdirPrototype"
envdirOutput :: String
envdirOutput="CCdirOutput"
envdbName :: String
envdbName="CCdbName"
envlogName :: String
envlogName="CClogName"

versionOpt :: Options -> Options
versionOpt      opts = opts{showVersion  = True}            
helpOpt :: Options -> Options
helpOpt         opts = opts{showHelp     = True}            
verboseOpt :: Options -> Options
verboseOpt      opts = opts{ verboseP     = True} 
developmentOpt :: Options -> Options
developmentOpt opts = opts{ development   = True}
autoRefreshOpt :: Maybe String -> Options -> Options
autoRefreshOpt (Just interval) opts | [(i,"")] <- reads interval = opts{autoRefresh = Just i}
autoRefreshOpt _               opts                              = opts{autoRefresh = Just 5}
prototypeOpt :: Maybe String -> Options -> Options
prototypeOpt nm opts 
  = opts { dirPrototype = fromMaybe (dirPrototype opts) nm
         , genPrototype = True}
importOpt     :: String -> Options -> Options
importOpt nm opts 
  = opts { importfile = nm }
iformatOpt :: String -> Options -> Options
iformatOpt f opts = case map toUpper f of
     "ADL" -> opts{importformat = Adl1Format}
     "ADL1"-> opts{importformat = Adl1Format}
     "POP" -> opts{importformat = Adl1PopFormat}
     "POP1"-> opts{importformat = Adl1PopFormat}
     _     -> opts
maxInterfacesOpt :: Options -> Options
maxInterfacesOpt  opts = opts{allInterfaces  = True}                            
themeOpt :: String -> Options -> Options
themeOpt t opts = opts{theme = case map toUpper t of 
                                    "STUDENT" -> StudentTheme
                                    "PROOF"   -> ProofTheme
                                    _         -> DefaultTheme}
dbNameOpt :: String -> Options -> Options
dbNameOpt nm opts = opts{dbName = if nm == "" 
                                    then baseName opts
                                    else nm
                        }                          
namespaceOpt :: String -> Options -> Options
namespaceOpt x opts = opts{namespace = x}
xmlOpt :: Options -> Options
xmlOpt          opts = opts{genXML       = True}
fspecRenderOpt :: String -> Options -> Options
fspecRenderOpt w opts = opts{ genFspec=True
                            , fspecFormat= case map toUpper w of
                                                 ('R': _ ) -> FRtf
                                                 ('L': _ ) -> FLatex
                                                 ('H': _ ) -> FHtml
                                                 ('P': _ ) -> FPandoc
                                                 ('O': _ ) -> FOpenDocument
                                                 _         -> fspecFormat opts
                                                
                            }
allFspecFormats :: String
allFspecFormats                     = "Pandoc, Rtf, OpenDocument, Latex, Html"
allImportFormats :: String
allImportFormats                    = "ADL (.adl), ADL1 (.adl), POP (.pop), POP1 (.pop)"
noGraphicsOpt :: Options -> Options
noGraphicsOpt opts                  = opts{genGraphics   = False}
genEcaDocOpt :: Options -> Options
genEcaDocOpt opts                   = opts{genEcaDoc     = True}
proofsOpt :: Options -> Options
proofsOpt opts                      = opts{proofs        = True}
interfacesOpt :: Maybe String -> Options -> Options
interfacesOpt mbnm opts             = opts{interfacesG   = True
                                          ,outputfile=fromMaybe "Generated.adl" mbnm}
haskellOpt :: Options -> Options
haskellOpt opts                     = opts{haskell       = True}
outputDirOpt :: String -> Options -> Options
outputDirOpt nm opts                = opts{dirOutput     = nm}
crowfootOpt :: Options -> Options
crowfootOpt opts                    = opts{crowfoot      = True}
blackWhiteOpt :: Options -> Options
blackWhiteOpt opts                  = opts{blackWhite    = True}
predLogicOpt :: Options -> Options
predLogicOpt opts                   = opts{showPredExpr  = True}
noDiagnosisOpt :: Options -> Options
noDiagnosisOpt opts                 = opts{noDiagnosis   = True}
diagnosisOpt :: Options -> Options
diagnosisOpt opts                   = opts{diagnosisOnly = True}
languageOpt :: String -> Options -> Options
languageOpt l opts                  = opts{language = case map toUpper l of
                                                       "NL"  -> Dutch
                                                       "UK"  -> English
                                                       "US"  -> English
                                                       "EN"  -> English
                                                       _     -> Dutch}
forceSyntaxOpt :: String -> Options -> Options
forceSyntaxOpt s opts               = opts{forcedParserVersion = case s of
                                              "1" -> Just PV664
                                              "2" -> Just PV211
                                              "0" -> Just PV211 --indicates latest
                                              _   -> error $ "Unknown value for syntax version: "++s++". Known values are 0, 1 or 2. 0 indicates latest."
                                          } 
logOpt :: String -> Options -> Options
logOpt nm opts                      = opts{logName       = nm}
pangoOpt :: Maybe String -> Options -> Options
pangoOpt (Just nm) opts             = opts{pangoFont     = nm}
pangoOpt Nothing  opts              = opts
sqlHostOpt :: Maybe String -> Options -> Options
sqlHostOpt mnm opts           = opts{sqlHost       = mnm}
sqlLoginOpt :: Maybe String -> Options -> Options
sqlLoginOpt mnm opts          = opts{sqlLogin      = mnm}
sqlPwdOpt :: Maybe String -> Options -> Options
sqlPwdOpt mnm opts            = opts{sqlPwd        = mnm}
testOpt :: Options -> Options
testOpt opts                        = opts{test          = True}

verbose :: Options -> String -> IO ()
verbose flags x
   | verboseP flags = putStr x
   | otherwise      = return ()
   
verboseLn :: Options -> String -> IO ()
verboseLn flags x
   | verboseP flags = -- each line is handled separately, so the buffer will be flushed in time. (see ticket #179)
                      sequence_ (map putStrLn (lines x))
   | otherwise      = return ()
helpNVersionTexts :: String -> Options -> [String]
helpNVersionTexts vs flags          = [preVersion flags++vs++postVersion flags++"\n" | showVersion flags]++
                                      [usageInfo' flags                              | showHelp    flags]
