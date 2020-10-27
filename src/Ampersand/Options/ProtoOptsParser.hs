{-# LANGUAGE NoImplicitPrelude #-}
module Ampersand.Options.ProtoOptsParser 
   (protoOptsParser)
where

import           Options.Applicative.Builder.Extra
import           Ampersand.Commands.Proto (ProtoOpts (..))
import           Ampersand.Basics
import           Ampersand.Misc.Defaults (defaultDirPrototype)
import           Ampersand.Options.Utils
import           Ampersand.Options.FSpecGenOptsParser
import           Options.Applicative

-- | Command-line parser for the proto command.
protoOptsParser :: Parser ProtoOpts
protoOptsParser = 
   ( \  outputLanguage fSpecGenOpts 
        dirPrototype 
        generateFrontend generateBackend checkCompilerVersion -> ProtoOpts
            { x1OutputLanguage = outputLanguage
            , x1fSpecGenOpts = fSpecGenOpts
            , xdirPrototype = dirPrototype
            , xgenerateFrontend = generateFrontend
            , xgenerateBackend = generateBackend
            , xcheckCompilerVersion = checkCompilerVersion
            }) 
  <$> outputLanguageP <*> fSpecGenOptsParser False
  <*> optional dirPrototypeP
  <*> generateFrontendP <*> generateBackendP <*> checkCompilerVersionP

dirPrototypeP :: Parser String
dirPrototypeP = strOption
        ( long "proto-dir"
        <> metavar "DIRECTORY"
        <> value defaultDirPrototype
        <> showDefault
        <> help "Specify the directory where the prototype will be generated"
        )
generateFrontendP :: Parser Bool
generateFrontendP = boolFlags True "frontend"
        "Generate prototype frontend files (Angular application)"
        mempty

generateBackendP :: Parser Bool
generateBackendP = boolFlags True "backend"
        "Generate backend files (PHP application)"
        mempty

checkCompilerVersionP :: Parser Bool
checkCompilerVersionP = boolFlags True "check-compiler-version"
        "Check compiler version constraints set by prototype framework (backend)"
        mempty
