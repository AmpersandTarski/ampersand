{-# LANGUAGE NoMonomorphismRestriction #-}

{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ampersand.Test.Parser.ParserTest (
    parseScripts, showErrors
) where

import           Ampersand.Basics
import           Ampersand.Input.ADL1.CtxError (Guarded(..),CtxError)
import           Ampersand.Input.Parsing
import           Ampersand.Options.FSpecGenOptsParser
import           Ampersand.Types.Config
import qualified RIO.NonEmpty as NE
import qualified RIO.Text as T
-- Tries to parse all the given files
parseScripts :: (HasRunner env) => 
                [FilePath] ->  RIO env Bool
parseScripts paths =
  case paths of
    [] -> return True
    (f:fs) -> do
        let fSpecGenOpts = defFSpecGenOpts f
        parsed <- snd <$> extendWith fSpecGenOpts (parseFileTransitive f)
        case parsed of
            Checked _ ws -> do
                logInfo $ "Parsed: " <> display (T.pack f)
                mapM_ logWarn (fmap displayShow ws)
                parseScripts fs
            Errors  e -> do 
                logError $ "Cannot parse: " <> display (T.pack f)
                showErrors (NE.toList e)
                return False

showErrors :: (HasLogFunc env) => [CtxError] ->  RIO env ()
showErrors = mapM_ (logError . displayShow)

