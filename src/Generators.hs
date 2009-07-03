{-# OPTIONS_GHC -Wall #-}
module Generators (doGenAtlas
                  ,doGenXML
                  ,doGenHaskell
                  ,doGenProto
                  ,doGenFspec
                  ,serviceGen
                  ,prove)
where

import Classes.Graphics
import Data.GraphViz
import System (system, ExitCode(ExitSuccess,ExitFailure))
import System.IO.Unsafe (unsafePerformIO) 

import System.FilePath(combine,replaceExtension)
import Options
import FspecDef
import ShowHS       (showHS)
import ShowADL      (showADLcode, printadl)
import ShowXML      (showXML)
import Strings      (chain)
import Calc         (deriveProofs)
import Prototype.ObjBinGen
import Adl
import Fspec2Pandoc  -- Als je Pandoc niet hebt geinstalleerd, dan kan je deze regel disabelen door 
--import Fspec2PandocDummy  -- maar dan moet je deze regel enabelen....
import Version
--import System
serviceGen :: Fspc -> Options -> IO()
serviceGen    fSpec flags
  = (writeFile outputFile $ printadl fSpec' 0 fSpec')
    >> verboseLn flags ("ADL written to " ++ outputFile ++ ".")
    where fSpec'= if (allServices flags)
                  then fSpec{serviceS=serviceG fSpec} 
                  else fSpec
          outputFile = combine (dirOutput flags) "Generated.adl"

prove :: Fspc -> Options -> IO()
prove fSpec _
    = putStr (deriveProofs fSpec)

doGenHaskell :: Fspc -> Options -> IO()
doGenHaskell fSpec flags
   =  verboseLn flags ("Generating Haskell source code for "++name fSpec)
   >> writeFile outputFile haskellCode 
   >> verboseLn flags ("Haskell written into " ++ outputFile ++ ".")
   where outputFile
               = combine (dirOutput flags) (replaceExtension (baseName flags) ".hs")
         haskellCode =
                ("{-# OPTIONS_GHC -Wall #-}"
             ++"\n{-Generated code by "++versionbanner++" at "++show (genTime flags)++"-}"
             ++"\nmodule Main where"
             ++"\n  import UU_Scanner"
             ++"\n  --import Classification"
             ++"\n  import Typology"
             ++"\n  import Adl"
             ++"\n  import ShowHS (showHS)"
             ++"\n  import Data.Fspec"
             ++"\n"
             ++"\n  main :: IO ()"
             ++"\n  main = putStr (showHS \"\\n  \" fSpec_"++baseName flags++")"
             ++"\n"
             ++"\n  fSpec_"++baseName flags++" :: Fspc"
             ++"\n  fSpec_"++baseName flags++"\n   = "++showHS "\n     " fSpec
                ) 
doGenAtlas :: Fspc -> Options -> IO()
doGenAtlas fSpec flags =
     verboseLn flags "Generation of Atlas is currently not supported."
  >> verboseLn flags ("Atlas would be generated in " ++ show (dirAtlas flags) ++ ".")
  >> run "test"
     where 
     outputFile fnm = combine (dirOutput flags) fnm
     testdot = toDot fSpec flags (head pts)
     pts = [pattern u | t<-themes fSpec,u<-units t]
     run fnm =
         do 
         writeFile (outputFile (fnm++".dot")) (show testdot)
         putStrLn ("Processing "++fnm++".dot ... :")
         result <- system $ "dot.exe -Tpng "++(outputFile (fnm++".dot"))++" -o "++(outputFile (fnm++".png"))
         case result of
            ExitSuccess   -> putStrLn ("  "++fnm++".png created.")
            ExitFailure x -> putStrLn $ "Failure: " ++ show x
   --REMARK -> the Data.GraphViz.Command function does not work properly (tested on Windows only)
   --    success <- runGraphviz testdot Png (outputFile fnm)
   --    return ()
   
doGenXML :: Fspc -> Options -> IO()
doGenXML fSpec flags 
   =  verboseLn flags "Generating XML..." >>
      writeFile outputFile ( showXML fSpec (genTime flags))   
   >> verboseLn flags ("XML written into " ++ outputFile ++ ".")
   where outputFile
               = combine (dirOutput flags) (replaceExtension (baseName flags) ".xml")
               
doGenProto :: Fspc -> Options -> IO()
doGenProto fSpec flags
   =  verboseLn flags "Generating prototype..."
     >> verboseLn flags ("Prototype files are written to " ++  (dirPrototype flags) ++ "." ) 
     >> phpObjServices fSpec (dirPrototype flags) (allServices flags)  
 
doGenFspec :: Fspc -> Options -> IO()
doGenFspec fSpec flags
   =  do
      verboseLn flags "Generating functional specification document..."
      customheader <- readFile (texHdrFile flags)
      writeFile outputFile  ( render2Pandoc flags customheader (fSpec2Pandoc fSpec' flags))
      verboseLn flags ("Functional specification  written into " ++ outputFile ++ ".")
   where  
   fSpec'= if (allServices flags)
           then fSpec{serviceS=serviceG fSpec} 
           else fSpec
   outputFile = combine (dirOutput flags) (replaceExtension (baseName flags) (outputExt $ fspecFormat flags))        
   outputExt FPandoc  = ".pandoc"
   outputExt FWord    = ".doc"
   outputExt FLatex   = ".tex"
   outputExt FHtml    = ".html"
   outputExt FUnknown = ".pandoc"
