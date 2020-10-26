{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE OverloadedStrings #-} 
module Ampersand.Output.ToJSON.Interfaces 
   (Interfaces)
where
import Ampersand.Output.ToJSON.JSONutils 
import Ampersand.ADL1
import Ampersand.FSpec.ToFSpec.NormalForms
import Ampersand.FSpec.ToFSpec.Calc

newtype Interfaces = Interfaces [JSONInterface] deriving (Generic, Show)
data JSONInterface = JSONInterface
  { ifcJSONid                 :: Text
  , ifcJSONlabel              :: Text
  , ifcJSONifcObject          :: JSONObjectDef
  , ifcJSONisAPI              :: Bool
  } deriving (Generic, Show)
data JSONObjectDef = 
  JSONObjectDef
    { ifcobjJSONtype               :: Text
    , ifcobjJSONtxt                :: Maybe Text
    , ifcobjJSONid                 :: Text
    , ifcobjJSONlabel              :: Text
    , ifcobjJSONviewId             :: Maybe Text
    , ifcobjJSONNormalizationSteps :: Maybe [Text] -- Not used in frontend. Just informative for analysis
    , ifcobjJSONrelation           :: Maybe Text
    , ifcobjJSONrelationIsFlipped  :: Maybe Bool
    , ifcobjJSONcrud               :: Maybe JSONCruds
    , ifcobjJSONexpr               :: Maybe JSONexpr
    , ifcobjJSONsubinterfaces      :: Maybe JSONSubInterface
    } deriving (Generic, Show)
data JSONSubInterface = JSONSubInterface
  { subJSONboxHeader          :: Maybe JSONBoxHeader
  , subJSONifcObjects         :: Maybe [JSONObjectDef]
  , subJSONrefSubInterfaceId  :: Maybe Text
  , subJSONrefIsLinkTo        :: Maybe Bool
  } deriving (Generic, Show)
data JSONBoxHeader = JSONBoxHeader
  { bhJSONtype                :: Text
  , bhJSONkeyVals             :: [JSONTemplateKeyValue]
  } deriving (Generic, Show)
data JSONTemplateKeyValue = JSONTemplateKeyValue
  { tkvJSONkey                :: Text
  , tkvJSONvalue              :: Maybe Text
  } deriving (Generic, Show)
data JSONCruds = JSONCruds
  { crudJSONread              :: Bool
  , crudJSONcreate            :: Bool
  , crudJSONupdate            :: Bool
  , crudJSONdelete            :: Bool
  } deriving (Generic, Show)
data JSONexpr = JSONexpr
  { exprJSONsrcConceptId      :: Text
  , exprJSONtgtConceptId      :: Text
  , exprJSONisUni             :: Bool
  , exprJSONisTot             :: Bool
  , exprJSONisIdent           :: Bool
  , exprJSONquery             :: Text
  } deriving (Generic, Show)

instance ToJSON JSONSubInterface where
  toJSON = amp2Jason
instance ToJSON Interfaces where
  toJSON = amp2Jason
instance ToJSON JSONInterface where
  toJSON = amp2Jason
instance ToJSON JSONObjectDef where
  toJSON = amp2Jason
instance ToJSON JSONCruds where
  toJSON = amp2Jason
instance ToJSON JSONexpr where
  toJSON = amp2Jason
instance ToJSON JSONBoxHeader where
  toJSON = amp2Jason
instance ToJSON JSONTemplateKeyValue where
  toJSON = amp2Jason
instance JSON FSpec Interfaces where
 fromAmpersand env fSpec _ = Interfaces (map (fromAmpersand env fSpec) (interfaceS fSpec ++ interfaceG fSpec))

instance JSON SubInterface JSONSubInterface where
 fromAmpersand env fSpec si = 
   case si of 
     Box{} -> JSONSubInterface
       { subJSONboxHeader          = Just . fromAmpersand env fSpec . siHeader $ si
       , subJSONifcObjects         = Just . map (fromAmpersand env fSpec) . siObjs $ si
       , subJSONrefSubInterfaceId  = Nothing
       , subJSONrefIsLinkTo        = Nothing
       }
     InterfaceRef{} -> JSONSubInterface
       { subJSONboxHeader          = Nothing
       , subJSONifcObjects         = Nothing
       , subJSONrefSubInterfaceId  = Just . escapeIdentifier . siIfcId $ si
       , subJSONrefIsLinkTo        = Just . siIsLink $ si
       }
instance JSON HTMLTemplateUsage JSONBoxHeader where
  fromAmpersand env fSpec header = JSONBoxHeader
       { bhJSONtype = btType header
       , bhJSONkeyVals = map (fromAmpersand env fSpec) $ btKeys header
       }
instance JSON TemplateKeyValue JSONTemplateKeyValue where
  fromAmpersand _ _ x = JSONTemplateKeyValue
       { tkvJSONkey = tkkey x
       , tkvJSONvalue = tkval x
       }

instance JSON Interface JSONInterface where
 fromAmpersand env fSpec interface = JSONInterface
  { ifcJSONid                 = escapeIdentifier . ifcname $ interface
  , ifcJSONlabel              = ifcname interface
  , ifcJSONifcObject          = fromAmpersand env fSpec (BxExpr $ ifcObj interface)
  , ifcJSONisAPI              = ifcIsAPI interface
  }

instance JSON Cruds JSONCruds where
 fromAmpersand _ _ crud = JSONCruds
  { crudJSONread              = crudR crud
  , crudJSONcreate            = crudC crud
  , crudJSONupdate            = crudU crud
  , crudJSONdelete            = crudD crud
  }
  
instance JSON ObjectDef JSONexpr where
 fromAmpersand env fSpec object =
    JSONexpr
        { exprJSONsrcConceptId = idWithoutType srcConcept
        , exprJSONtgtConceptId = idWithoutType tgtConcept
        , exprJSONisUni        = isUni normalizedInterfaceExp
        , exprJSONisTot        = isTot normalizedInterfaceExp
        , exprJSONisIdent      = isIdent normalizedInterfaceExp
        , exprJSONquery        = query
        }
      where
        query = broadQueryWithPlaceholder fSpec object{objExpression=normalizedInterfaceExp}
        normalizedInterfaceExp = conjNF env $ objExpression object
        (srcConcept, tgtConcept) =
          case getExpressionRelation normalizedInterfaceExp of
            Just (src, _ , tgt, _) ->
              (src, tgt)
            Nothing -> (source normalizedInterfaceExp, target normalizedInterfaceExp) -- fall back to typechecker type
 
instance JSON BoxItem JSONObjectDef where
 fromAmpersand env fSpec obj =
   case obj of 
     BxExpr object' -> JSONObjectDef
      { ifcobjJSONtype               = "ObjExpression"
      , ifcobjJSONid                 = escapeIdentifier . name $ object
      , ifcobjJSONlabel              = name object
      , ifcobjJSONviewId             = fmap name viewToUse
      , ifcobjJSONNormalizationSteps = Just $ showPrf showA.cfProof.objExpression $ object 
      , ifcobjJSONrelation           = fmap (showRel . fst) mEditableDecl
      , ifcobjJSONrelationIsFlipped  = fmap            snd  mEditableDecl
      , ifcobjJSONcrud               = Just $ fromAmpersand env fSpec (objcrud object)
      , ifcobjJSONexpr               = Just $ fromAmpersand env fSpec object
      , ifcobjJSONsubinterfaces      = fmap  (fromAmpersand env fSpec) (objmsub object)
      , ifcobjJSONtxt                = Nothing
      }
      where
        viewToUse = case objmView object of
                    Just nm -> Just $ lookupView fSpec nm
                    Nothing -> getDefaultViewForConcept fSpec tgtConcept
        normalizedInterfaceExp = conjNF env $ objExpression object
        (tgtConcept, mEditableDecl) =
          case getExpressionRelation normalizedInterfaceExp of
            Just (_ , decl, tgt, isFlipped') ->
              (tgt, Just (decl, isFlipped'))
            Nothing -> (target normalizedInterfaceExp, Nothing) -- fall back to typechecker type
        object = substituteReferenceObjectDef fSpec object'
     BxTxt object -> JSONObjectDef
      { ifcobjJSONtype               = "ObjText"
      , ifcobjJSONid                 = escapeIdentifier . name $ object
      , ifcobjJSONlabel              = name object
      , ifcobjJSONviewId             = Nothing
      , ifcobjJSONNormalizationSteps = Nothing
      , ifcobjJSONrelation           = Nothing
      , ifcobjJSONrelationIsFlipped  = Nothing
      , ifcobjJSONcrud               = Nothing
      , ifcobjJSONexpr               = Nothing
      , ifcobjJSONsubinterfaces      = Nothing
      , ifcobjJSONtxt                = Just $ objtxt object
      }
