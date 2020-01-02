{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ampersand.FSpec.MetaModels
  ( MetaModel(..)
  , mkGrindInfo
  , GrindInfo
  , grind
  , addSemanticModel
  , pCtx2Fspec
  )

where
import           Ampersand.ADL1
import           Ampersand.ADL1.P2A_Converters
import           Ampersand.Basics
import           Ampersand.FSpec.FSpec
import           Ampersand.FSpec.ShowMeatGrinder
import           Ampersand.FSpec.ToFSpec.ADL2FSpec
import           Ampersand.FSpec.Transformers 
import           Ampersand.Input
import           Ampersand.Misc.HasClasses
import qualified RIO.List as L
import qualified RIO.NonEmpty as NE

parser :: (HasLogFunc env, HasFSpecGenOpts env) => MetaModel -> RIO env (Guarded P_Context)
parser FormalAmpersand = parseFormalAmpersand
parser PrototypeContext   = parsePrototypeContext 

pCtx2Fspec :: (HasFSpecGenOpts env) => env -> P_Context -> Guarded FSpec
pCtx2Fspec env c = makeFSpec env <$> pCtx2aCtx env c


mkGrindInfo :: (HasFSpecGenOpts env, HasLogFunc env) => MetaModel -> RIO env GrindInfo
mkGrindInfo metamodel = do
    env <- ask 
    build env <$> parser metamodel
  where
    build :: (HasFSpecGenOpts env) =>
        env -> Guarded P_Context -> GrindInfo
    build env pCtx = GrindInfo
            { metaModel    = metamodel
            , pModel       = case pCtx of
                  Errors errs -> fatal . unlines $
                          ("The ADL scripts of "++name metamodel++" cannot be parsed:")
                        : (L.intersperse (replicate 30 '=') . fmap show . NE.toList $ errs)
                  Checked x [] -> x
                  Checked _ ws -> fatal . unlines $
                          ("The ADL scripts of "++name metamodel++" are not free of warnings:")
                        : (L.intersperse (replicate 30 '=') . fmap show $ ws)
            , fModel       = 
                case join $ pCtx2Fspec env <$> pCtx of
                  Errors errs -> fatal . unlines $
                          ("The ADL scripts of "++name metamodel++" cannot be parsed:")
                        : (L.intersperse (replicate 30 '=') . fmap show . NE.toList $ errs)
                  Checked x [] -> x
                  Checked _ ws -> fatal . unlines $
                          ("The ADL scripts of "++name metamodel++" are not free of warnings:")
                        : (L.intersperse (replicate 30 '=') . fmap show $ ws)
            , transformers = case metamodel of
                                FormalAmpersand -> transformersFormalAmpersand
                                PrototypeContext   -> transformersPrototypeContext
            }


