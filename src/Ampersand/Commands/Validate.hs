{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Generate a prototype from a project.
module Ampersand.Commands.Validate
    (validate
    ,ValidateOpts(..)
    ,HasValidateOpts(..)
    ) where

import           Ampersand.Basics
import           Ampersand.Misc.HasClasses
import           Ampersand.FSpec
import           Ampersand.Prototype.ValidateSQL (validateRulesSQL)

-- | Builds a prototype of the current project.
--
validate :: (HasLogFunc env)
       => FSpec -> RIO env ()
validate fSpec = do
    logInfo "Validating SQL expressions..."
    errMsg <- validateRulesSQL fSpec
    unless (null errMsg) (exitWith $ InvalidSQLExpression errMsg)



