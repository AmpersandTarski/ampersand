﻿{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
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
import           Ampersand.Prototype.GenFrontend (doGenFrontend, doGenBackend, copyCustomizations)
import           Ampersand.Types.Config
import qualified RIO.Text as T
import           System.Directory
-- | Builds a prototype of the current project.
--
proto :: (Show env, HasRunner env, HasDirCustomizations env, HasZwolleVersion env, HasProtoOpts env, HasAllowInvariantViolations env, HasDirPrototype env, HasGenerateFrontend env, HasGenerateBackend env) 
       => FSpec -> RIO env ()
proto fSpec = do
    env <- ask
    let dirPrototype = getDirPrototype env
    allowInvariantViolations <- view allowInvariantViolationsL
    let violatedRules :: [(Rule,AAtomPairs)]
        violatedRules = violationsOfInvariants fSpec
    if null violatedRules || allowInvariantViolations
    then do
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
       copyCustomizations
       dirPrototypeA <- liftIO $ makeAbsolute dirPrototype
       logInfo $ "Prototype files have been written to " <> display (T.pack dirPrototypeA)
    else exitWith $ NoPrototypeBecauseOfRuleViolations (violationMessages violatedRules)

violationMessages :: [(Rule,AAtomPairs)] -> [String]
violationMessages = concatMap violationMessage
  where
    violationMessage :: (Rule,AAtomPairs) -> [String]
    violationMessage (r,ps) = 
      [if length ps == 1 
        then "There is " <>show (length ps)<>" violation of RULE " <>show (name r)<>":"
        else "There are "<>show (length ps)<>" violations of RULE "<>show (name r)<>":"
      ] 
      <> (map ("  "<>) . listPairs 3 . toList $ ps)
    listPairs :: Int -> [AAtomPair] -> [String]
    listPairs i xs = 
                case xs of
                  [] -> []
                  h:tl 
                    | i == 0 -> ["  ... ("<>show (length xs)<>" more)"]
                    | otherwise -> showAP h : listPairs (i-1) tl
        where
          showAP :: AAtomPair -> String
          showAP x= "("<>aavstr (apLeft x)<>", "<>aavstr (apRight x)<>")"
        