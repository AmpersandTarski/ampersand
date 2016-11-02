module Ampersand.FSpec.ToFSpec.CreateFspec
  (createMulti)

where
import Prelude hiding (putStrLn, writeFile) -- make sure everything is UTF8
import Ampersand.Basics
import Ampersand.Misc
import Ampersand.ADL1
import Ampersand.ADL1.P2A_Converters
import Ampersand.FSpec.FSpec
import Ampersand.FSpec.ShowMeatGrinder
import Ampersand.Input
import Ampersand.Input.ADL1.CtxError
import Ampersand.FSpec.ToFSpec.ADL2FSpec
import System.FilePath
import Control.Monad

-- | create an FSpec, based on the provided command-line options.
--   Without the command-line switch "--meta-tables", 
--   Ampersand compiles its script (userP_Ctx) straightforwardly in first order relation algebra.
--   This is useful for simple scripts and the compilation process is easy to understand.
--
--   With "--meta-tables" switched on, Ampersand does more.
--   This switch is useful for higher order Ampersand,
--   in which the user can work with the rules, relations and concepts of the model inside the model.
--   Besides the user script, userP_Ctx, Ampersand creates its own metamodel, rapP_Ctx, which is generated from "AST.adl"
--   This metamodel is populated with the result of grinding userP_Ctx, being populationPctx.
--   Grinding means to analyse the script down to the binary relations that constitute the metamodel.
--   The combination of model and populated metamodel results in the Guarded FSpec,
--   which is the result of createMulti.
createMulti :: Options  -- ^The options derived from the command line
            -> IO(Guarded MultiFSpecs)
createMulti opts = do 
  gSystemP_Ctx <- parseSystemContext opts
  whenCheckedIO (parseADL opts (fileName opts)) $ \userP_Ctx ->
    do let systemP_Ctx = case gSystemP_Ctx of
                          Errors err -> fatal 36 $ "Errors found while parsing SystemContext!"++show err
                          Checked ctx -> ctx
       let gFSpec = pCtx2Fspec . merge . pure $ [userP_Ctx,systemP_Ctx]  -- the FSpec resulting from the user's souceFile
       when (genMetaFile opts) (dumpMetaFile gFSpec)
       if genMetaTables opts || genRap
       then do fAmpP_Ctx <- parseMeta opts             -- the P_Context of the formalAmpersand metamodel
               let gGrinded :: Guarded P_Context
                   gGrinded = addGens <$> fAmpP_Ctx <*> join (grind <$> gFSpec) -- the user's sourcefile grinded, i.e. a P_Context containing population in terms of formalAmpersand.
               let metaPopFSpec = pCtx2Fspec gGrinded
               return $ mkMulti <$> (Just <$> metaPopFSpec) <*> combineAll [pure userP_Ctx, gGrinded, fAmpP_Ctx]
       else    return $ mkMulti <$> pure Nothing <*> gFSpec
   where
    -- The gens from FromalAmpersand must be available in the result of grinded 
    addGens :: P_Context -> P_Context -> P_Context
    addGens fa grinded = grinded{ctx_gs=gs fa++gs grinded}
     where
      gs pCtx = ctx_gs pCtx ++ concatMap pt_gns (ctx_pats pCtx)
    genRap = genRapPopulationOnly opts
    mkMulti :: Maybe FSpec -> FSpec -> MultiFSpecs
    mkMulti y x = MultiFSpecs
               { userFSpec = x
               , metaFSpec = y
               }
    dumpMetaFile :: Guarded FSpec -> IO()
    dumpMetaFile a = case a of
              Checked fSpec -> let (filePath,metaContents) = makeMetaPopulationFile fSpec 
                               in writeMetaFile (filePath,metaContents)
              _ -> return ()
    writeMetaFile :: (FilePath,String) -> IO ()
    writeMetaFile (filePath,metaContents) = do
        verboseLn opts ("Generating meta file in path "++dirOutput opts)
        writeFile (dirOutput opts </> filePath) metaContents      
        verboseLn opts ("\""++filePath++"\" written")

    combineAll :: [Guarded P_Context] -> Guarded FSpec
    combineAll = pCtx2Fspec . merge . sequenceA
         
    pCtx2Fspec :: Guarded P_Context -> Guarded FSpec
    pCtx2Fspec c = makeFSpec opts <$> join (pCtx2aCtx opts <$> c)
    merge :: Guarded [P_Context] -> Guarded P_Context
    merge ctxs = f <$> ctxs
      where
       f []     = fatal 77 $ "merge must not be applied to an empty list"
       f (c:cs) = foldr mergeContexts c cs
    grind :: FSpec -> Guarded P_Context
    grind fSpec = f <$> uncurry parseCtx (makeMetaPopulationFile fSpec)
      where
       f (a,[]) = a
       f _      = fatal 83 "Meatgrinder returns included file. That isn't anticipated."
