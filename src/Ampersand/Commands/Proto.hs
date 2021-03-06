﻿{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}


-- | Generate a prototype from a project.
module Ampersand.Commands.Proto
    (proto
    ,ProtoOpts(..)
    ,HasProtoOpts(..)
    ) where

import           Ampersand.Basics
import           Ampersand.FSpec
import           Ampersand.Misc.HasClasses
import           Ampersand.Prototype.GenFrontend
import           Ampersand.Types.Config
import qualified RIO.Text as T
import           System.Directory
-- | Builds a prototype of the current project.
proto :: ( Show env
         , HasRunner env
         , HasFSpecGenOpts env
         , HasDirCustomizations env
         , HasZwolleVersion env
         , HasProtoOpts env
         , HasDirPrototype env
         , HasGenerateFrontend env
         , HasGenerateBackend env
         , HasGenerateMetamodel env
         ) 
       => FSpec -> RIO env ()
proto fSpec = do
    env <- ask
    let dirPrototype = getDirPrototype env
    logDebug "Generating prototype..."
    liftIO $ createDirectoryIfMissing True dirPrototype
    generateFrontend <- view generateFrontendL
    if generateFrontend 
     then do doGenFrontend fSpec
     else do logDebug "  Skipping generating frontend files"
    generateBackend <- view generateBackendL
    if generateBackend
      then do doGenBackend fSpec
      else do logDebug "  Skipping generating backend files"
    generateMetamodel <- view generateMetamodelL
    if generateMetamodel
      then do doGenMetaModel fSpec
      else do logDebug "  Skipping generating metamodel.adl"
    copyCustomizations
    dirPrototypeA <- liftIO $ makeAbsolute dirPrototype
    logInfo $ "Prototype files have been written to " <> display (T.pack dirPrototypeA)
