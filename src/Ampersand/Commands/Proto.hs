{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Generate a prototype from a project.
module Ampersand.Commands.Proto
    (proto
    ,ProtoOpts(..)
    ,HasProtoOpts(..)
    ) where

import           Ampersand.Basics
import           Ampersand.FSpec
import           Ampersand.Misc.HasClasses
import           Ampersand.Prototype.GenFrontend (doGenFrontend, doGenBackend)
import           Ampersand.Types.Config
import qualified RIO.Text as T
import           System.Directory
-- | Builds a prototype of the current project.
--
proto :: (Show env, HasRunner env, HasFSpecGenOpts env, HasProtoOpts env, HasDirPrototype env, HasGenerateFrontend env, HasGenerateBackend env, HasCheckCompilerVersion env) 
       => FSpec -> RIO env ()
proto fSpec = do
    env <- ask
    let dirPrototype = getDirPrototype env
    logDebug "Generating prototype..."
    liftIO $ createDirectoryIfMissing True dirPrototype
    generateFrontend <- view generateFrontendL
    generateBackend <- view generateBackendL
    if generateFrontend 
     then do doGenFrontend fSpec
     else do logDebug "  Skipping generating frontend files"
    if generateBackend
      then do doGenBackend fSpec
      else do logDebug "  Skipping generating backend files"
    dirPrototypeA <- liftIO $ makeAbsolute dirPrototype
    logInfo $ "Prototype files have been written to " <> display (T.pack dirPrototypeA)

