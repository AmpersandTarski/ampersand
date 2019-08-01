{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
-- | Reads a project and parses it
module Ampersand.Daemon.Parser (
    parseProject
) where

import           Ampersand.Basics
import           Ampersand.Core.ParseTree
import           Ampersand.Daemon.Types
import           Ampersand.Input.Parsing
import           Ampersand.Input.ADL1.CtxError
import qualified Data.List.NonEmpty as NEL
import           Ampersand.FSpec.MetaModels
import           Ampersand.Types.Config
import           Ampersand.Options.FSpecGenOptsParser
parseProject :: (HasRunner env) => 
                FilePath ->  RIO env ([Load],[FilePath])
parseProject rootAdl = do
    let fSpecGenOpts = defFSpecGenOpts rootAdl 
    extendWith fSpecGenOpts $ do 
        (pc,gPctx) <- parseADL rootAdl 
        env2 <- ask
        let loadedFiles = map pcCanonical pc
            gActx = pCtx2Fspec env2 <$> gPctx
        return ( case gActx of
                Checked _ ws -> map warning2Load $ ws
                Errors  es   -> NEL.toList . fmap error2Load $ es
            , loadedFiles
            )

warning2Load :: Warning -> Load
warning2Load warn = Message
    {loadSeverity = Warning
    ,loadFile = file 
    ,loadFilePos = (line,col) 
    ,loadFilePosEnd = (line,col)
    ,loadMessage = lines $ show warn
    }
  where (file, line, col) = (filenm warn, linenr warn, colnr warn)
        
error2Load :: CtxError -> Load
error2Load err = Message
    {loadSeverity = Error
    ,loadFile = file 
    ,loadFilePos = (line,col) 
    ,loadFilePosEnd = (line,col)
    ,loadMessage = lines $ show err
    }
  where (file, line, col) = (filenm err, linenr err, colnr err)
