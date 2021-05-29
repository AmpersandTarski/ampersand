{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Ampersand.Input.Xslx.XLSX 
  (parseXlsxFile)
where
import           Ampersand.Basics hiding (view, (^.))
import           Ampersand.Core.ParseTree
import           Ampersand.Core.ShowPStruct  -- Just for debugging purposes
import           Ampersand.Input.ADL1.CtxError
import           Ampersand.Misc.HasClasses
import           Ampersand.Prototype.StaticFiles_Generated
import           Codec.Xlsx
import           Control.Lens hiding (both) -- ((^?),ix)
import           Data.Tuple.Extra(both,swap)
import qualified RIO.List as L
import qualified RIO.ByteString as B
import qualified RIO.ByteString.Lazy as BL
import           RIO.Char
import qualified RIO.Map as Map
import qualified RIO.Text as T
import qualified RIO.NonEmpty as NE
import qualified RIO.Set as Set

parseXlsxFile :: (HasFSpecGenOpts env) => 
    Maybe FileKind -> FilePath -> RIO env (Guarded P_Context)
parseXlsxFile mFk file =
  do env <- ask
     bytestr <- 
        case mFk of
          Just fileKind 
             -> case getStaticFileContent fileKind file of
                      Just cont -> return cont
                      Nothing -> fatal ("Statically included "<> tshow fileKind<> " files. \n  Cannot find `"<>T.pack file<>"`.")
          Nothing
             -> liftIO $ B.readFile file
     return . xlsx2pContext env . toXlsx . BL.fromStrict $ bytestr
 where
  xlsx2pContext :: (HasFSpecGenOpts env) 
      => env -> Xlsx -> Guarded P_Context
  xlsx2pContext env xlsx = Checked pop []
    where 
      pop = mkContextOfPops
          . concatMap (toPops env file)
          . concatMap theSheetCellsForTable 
          $ (xlsx ^. xlSheets)

-- | To enable roundtrip testing, all data can be exported.
-- For this purpose mkContextOfPopsOnly exports the population only
mkContextOfPops :: [P_Population] -> P_Context
mkContextOfPops populations = enrichedContext
  where
  --The result of mkContextOfPops is a P_Context enriched with the relations in genericRelations
  --The population is reorganized in genericPopulations to accommodate the particular ISA-graph.
    pCtx = PCtx{ ctx_nm     = ""
               , ctx_pos    = []
               , ctx_lang   = Nothing
               , ctx_markup = Nothing
               , ctx_pats   = []
               , ctx_rs     = []
               , ctx_ds     = []
               , ctx_cs     = []
               , ctx_ks     = []
               , ctx_rrules = []
               , ctx_reprs  = []
               , ctx_vs     = []
               , ctx_gs     = []
               , ctx_ifcs   = []
               , ctx_ps     = []
               , ctx_pops   = populations
               , ctx_metas  = []
               }
    enrichedContext :: P_Context
    enrichedContext
     = pCtx{ ctx_ds     = mergeRels (genericRelations<>declaredRelations)
           , ctx_pops   = genericPopulations
           }
    declaredRelations ::  [P_Relation]   -- relations declared in the user's script
    popRelations ::       [P_Relation]   -- relations that are "annotated" by the user in Excel-sheets.
                                         -- popRelations are derived from P_Populations only.
    declaredRelations = mergeRels (ctx_ds pCtx<>concatMap pt_dcs (ctx_pats pCtx))
    -- | To derive relations from populations, we derive the signature from the population's signature directly.
    --   Multiplicity properties are added to constrain the population without introducing violations.
    popRelations 
     = [ computeProps rel
       | pop@P_RelPopu{p_src = src, p_tgt = tgt}<-ctx_pops pCtx<>[pop |pat<-ctx_pats pCtx, pop<-pt_pop pat]
       , Just src'<-[src], Just tgt'<-[tgt]
       , rel<-[ P_Relation{ dec_nm     = name pop
                     , dec_sign   = P_Sign src' tgt'
                     , dec_prps   = mempty
                     , dec_pragma = mempty
                     , dec_Mean   = mempty
                     , pos        = origin pop
                     }]
       , signatur rel `notElem` map signatur declaredRelations
       ]
       where
          computeProps :: P_Relation -> P_Relation
          computeProps rel
           = rel{dec_prps = Set.fromList ([ Uni | isUni popR]<>[ Tot | isTot ]<>[ Inj | isInj popR ]<>[ Sur | isSur ])}
              where
               sgn  = dec_sign rel
               s = pSrc sgn; t = pTgt sgn
               popu :: P_Concept -> Set.Set PAtomValue
               popu c = (Set.fromList . concatMap p_popas) [ pop | pop@P_CptPopu{}<-pops, name c==name pop ]
               popR :: Set.Set PAtomPair
               popR = (Set.fromList . concatMap p_popps )
                      [pop
                      | pop@P_RelPopu {p_src = src, p_tgt = tgt} <- pops
                      , name rel == name pop
                      , Just src' <- [src]
                      , src' == s
                      , Just tgt' <- [tgt]
                      , tgt' == t
                      ]
               domR = Set.fromList . map ppLeft  . Set.toList $ popR
               codR = Set.fromList . map ppRight . Set.toList $ popR
               equal f (a,b) = f a == f b
               isUni :: Set.Set PAtomPair -> Bool
               isUni x = null . Set.filter (not . equal ppRight) . Set.filter (equal ppLeft) $ cartesianProduct x x
               isTot = popu s `Set.isSubsetOf` domR
               isInj :: Set.Set PAtomPair -> Bool
               isInj x = null . Set.filter (not . equal ppLeft) . Set.filter (equal ppRight) $ cartesianProduct x x
               isSur = popu t `Set.isSubsetOf` codR
               cartesianProduct :: -- Should be implemented as Set.cartesianProduct, but isn't. See https://github.com/commercialhaskell/rio/issues/177
                                   (Ord a, Ord b) => Set a -> Set b -> Set (a, b)
               cartesianProduct xs ys = Set.fromList $ liftA2 (,) (toList xs) (toList ys)
    genericRelations ::   [P_Relation]   -- generalization of popRelations due to CLASSIFY statements
    genericPopulations :: [P_Population] -- generalization of popRelations due to CLASSIFY statements
    -- | To derive relations from populations, we derive the signature from the population's signature directly.
    --   Multiplicity properties are added to constrain the population without introducing violations.
    (genericRelations, genericPopulations)
     = recur [] popRelations pops invGen
       where
        recur :: [P_Concept]->[P_Relation]->[P_Population]->[(P_Concept,Set.Set P_Concept)]->([P_Relation], [P_Population])
        recur     seen         unseenrels    unseenpops      ((g,specs):invGens)
         = if g `elem` seen then fatal ("Concept "<>name g<>" has caused a cycle error.") else
           recur (g:seen) (genericRels<>remainder) (genericPops<>remainPop) invGens
           where
            sameNameTargetRels :: [NE.NonEmpty P_Relation]
            sameNameTargetRels = eqCl (\r->(name r,targt r)) unseenrels
            genericRels ::    [P_Relation]
            remainingRels :: [[P_Relation]]
            (genericRels, remainingRels)
             = L.unzip
               [ ( headrel{ dec_sign = P_Sign g (targt (NE.head sRel))
                          , dec_prps = let test prop = prop `elem` foldr Set.intersection Set.empty (fmap dec_prps sRel)
                                       in Set.fromList ([Uni |test Uni]<>[Tot |test Tot]<>[Inj |test Inj]<>[Sur |test Sur])
                          }  -- the generic relation that summarizes sRel
            --   , [ rel| rel<-sRel, sourc rel `elem` specs ]                    -- the specific (and therefore obsolete) relations
                 , [ rel| rel<-NE.toList sRel, sourc rel `notElem` specs ]                 -- the remaining relations
                 )
               | sRel<-sameNameTargetRels
               , specs `Set.isSubsetOf` (Set.fromList . NE.toList $ fmap sourc sRel)
               , headrel<-[NE.head sRel]
               ]
            remainder :: [P_Relation]
            remainder
             = concat (remainingRels<>fmap NE.toList
                       [ sRel | sRel<-sameNameTargetRels
                       , not (specs `Set.isSubsetOf` (Set.fromList . NE.toList $ fmap sourc sRel))]
                      )
            sameNameTargetPops :: [NE.NonEmpty P_Population]
            sameNameTargetPops = eqCl (\r->(name r,tgtPop r)) unseenpops
            genericPops ::    [P_Population]
            remainingPops :: [[P_Population]]
            (genericPops, remainingPops)
             = L.unzip
               [ ( headPop{p_src=Just g}                   -- the generic relation that summarizes sRel
            --   , [ pop| pop<-sPop, srcPop pop `elem` specs ]    -- the specific (and therefore obsolete) populations
                 , [ pop| pop<-NE.toList sPop, srcPop pop `notElem` specs ] -- the remaining relations
                 )
               | sPop<-sameNameTargetPops
               , specs `Set.isSubsetOf` (Set.fromList . NE.toList $ fmap srcPop sPop)
               , headPop@P_RelPopu{}<-[NE.head sPop] -- Restrict to @P_RelPopu{} because field name p_src is being used
               ]
            remainPop :: [P_Population]
            remainPop
             = concat (remainingPops<>fmap NE.toList
                       [ sPop | sPop<-sameNameTargetPops
                       , not (specs `Set.isSubsetOf` (Set.fromList . NE.toList $ fmap srcPop sPop))]
                      )
        recur _ rels popus [] = (rels,popus)
        srcPop, tgtPop :: P_Population -> P_Concept -- get the source concept of a P_Population.
        srcPop pop@P_CptPopu{} = PCpt (name pop)
        srcPop pop@P_RelPopu{p_src = src} = case src of Just s -> s; _ -> fatal ("srcPop ("<>showP pop<>") is mistaken.")
        tgtPop pop@P_CptPopu{} = PCpt (name pop)
        tgtPop pop@P_RelPopu{p_tgt = tgt} = case tgt of Just t -> t; _ -> fatal ("tgtPop ("<>showP pop<>") is mistaken.")

    sourc, targt :: P_Relation -> P_Concept -- get the source concept of a P_Relation.
    sourc = pSrc . dec_sign
    targt = pTgt . dec_sign
    invGen :: [(P_Concept,Set.Set P_Concept)]  -- each pair contains a concept with all of its specializations
    invGen = [ (fst (NE.head cl), Set.fromList spcs)
             | cl<-eqCl fst [ (g,specific gen) | gen<-ctx_gs pCtx, g<-NE.toList (generics gen)]
             , g<-[fst (NE.head cl)], spcs<-[[snd c | c<-NE.toList cl, snd c/=g]], not (null spcs)
             ]
    signatur :: P_Relation -> (Text, P_Sign)
    signatur rel =(name rel, dec_sign rel)
    concepts = L.nub $
            [ PCpt (name pop) | pop@P_CptPopu{}<-ctx_pops pCtx] <>
            [ src' | P_RelPopu{p_src = src}<-ctx_pops pCtx, Just src'<-[src]] <>
            [ tgt' | P_RelPopu{p_tgt = tgt}<-ctx_pops pCtx, Just tgt'<-[tgt]] <>
            map sourc declaredRelations<> map targt declaredRelations<>
            concat [specific gen: NE.toList (generics gen)| gen<-ctx_gs pCtx]
    pops = computeConceptPopulations (ctx_pops pCtx<>[p |pat<-ctx_pats pCtx, p<-pt_pop pat])   -- All populations defined in this context, from POPULATION statements as well as from Relation declarations.
    computeConceptPopulations :: [P_Population] -> [P_Population]
    computeConceptPopulations pps -- I feel this computation should be done in P2A_Converters.hs, so every A_structure has compliant populations.
     = [ P_CptPopu{pos = OriginUnknown, p_cpt = c, p_popas = L.nub $
                       [ atom | cpt@P_CptPopu{}<-pps, PCpt (name cpt) == c, atom<-p_popas cpt]<>
                       [ ppLeft pair
                       | pop@P_RelPopu{p_src = src}<-pps, Just src'<-[src], src' == c
                       , pair<-p_popps pop]<>
                       [ ppRight pair
                       | pop@P_RelPopu{p_tgt = tgt}<-pps, Just tgt'<-[tgt], tgt' == c
                       , pair<-p_popps pop]}
       | c<-concepts
       ] <>
       [ rpop{p_popps=concatMap p_popps cl}
       | cl<-eqCl (\pop->(name pop,p_src pop,p_tgt pop)) [ pop | pop@P_RelPopu{}<-pps], rpop<-[NE.head cl]
       ]


data SheetCellsForTable 
       = Mapping{ theSheetName :: Text
                , theCellMap   :: CellMap
                , headerRowNrs :: [Int]
                , popRowNrs    :: [Int]
                , colNrs       :: [Int]
                , debugInfo :: [Text]
                }
instance Show SheetCellsForTable where  --for debugging only
  show x 
   = T.unpack . T.unlines $
      [ "Sheet       : "<>theSheetName x
      , "headerRowNrs: "<>tshow (headerRowNrs x)
      , "popRowNrs   : "<>tshow (popRowNrs x)
      , "colNrs      : "<>tshow (colNrs x)
      ] <> debugInfo x 
toPops :: (HasFSpecGenOpts env) => env -> FilePath -> SheetCellsForTable -> [P_Population]
toPops env file x = map popForColumn (colNrs x)
  where
    popForColumn :: Int -> P_Population
    popForColumn i =
      if i  == sourceCol  
      then  P_CptPopu { pos = popOrigin
                      , p_cpt = mkPConcept sourceConceptName 
                      , p_popas = concat [ case value(row,i) of
                                             Nothing -> []
                                             Just cv -> cellToAtomValues mSourceConceptDelimiter cv popOrigin
                                         | row <- popRowNrs x
                                         ] 
                      }
      else  P_RelPopu { pos = popOrigin
                      , p_src = src
                      , p_tgt = trg
                      , p_nmdr = PNamedRel popOrigin relName Nothing -- The P-to-A converter must assign the type.
                      , p_popps = thePairs
                      }
     where                             
       src, trg :: Maybe P_Concept
       (src,trg) = case mTargetConceptName of
                  Just tCptName -> both (fmap mkPConcept) $ (if isFlipped' then swap else id) (Just sourceConceptName, Just tCptName)
                  Nothing -> (Nothing,Nothing)
          
       popOrigin :: Origin
       popOrigin = originOfCell (relNamesRow, targetCol)
       (relNamesRow,conceptNamesRow) = case headerRowNrs x of
                                         [] -> fatal "headerRowNrs x is empty"
                                         [rnr] -> (rnr,fatal "headerRowNrs x has only one element")
                                         rnr:cnr:_ -> (rnr,cnr)
       sourceCol       = case colNrs x of
                           [] -> fatal "colNrs x is empty"
                           c:_ -> c
       targetCol       = i 
       sourceConceptName :: Text
       mSourceConceptDelimiter :: Maybe Char
       (sourceConceptName, mSourceConceptDelimiter)
          = case value (conceptNamesRow,sourceCol) of
                Just (CellText t) -> 
                   fromMaybe (fatal "No valid source conceptname found. This should have been checked before")
                             (conceptNameWithOptionalDelimiter t)
                _ -> fatal "No valid source conceptname found. This should have been checked before"
       mTargetConceptName :: Maybe Text
       mTargetConceptDelimiter :: Maybe Char
       (mTargetConceptName, mTargetConceptDelimiter)
          = case value (conceptNamesRow,targetCol) of
                Just (CellText t) -> let (nm,mDel) 
                                           = fromMaybe
                                                (fatal "No valid source conceptname found. This should have been checked before")
                                                (conceptNameWithOptionalDelimiter t)
                                     in (Just nm, mDel)
                _ -> (Nothing, Nothing)
       relName :: Text
       isFlipped' :: Bool
       (relName,isFlipped') 
          = case value (relNamesRow,targetCol) of
                Just (CellText t) -> 
                    case T.uncons . T.reverse . trim $ t of
                      Nothing -> (mempty, False)
                      Just ('~',rest) -> (T.reverse rest, True )
                      Just (h,tl)     -> (T.reverse $ T.cons h tl, False)
                _ -> fatal ("No valid relation name found. This should have been checked before" <>tshow (relNamesRow,targetCol))
       thePairs :: [PAtomPair]
       thePairs =  concat . mapMaybe pairsAtRow . popRowNrs $ x
       pairsAtRow :: Int -> Maybe [PAtomPair]
       pairsAtRow r = case (value (r,sourceCol)
                          ,value (r,targetCol)
                          ) of
                       (Just s,Just t) -> Just $ 
                                            (if isFlipped' then map flp else id)
                                                [mkPair origTrg s' t'
                                                | s' <- cellToAtomValues mSourceConceptDelimiter s origSrc
                                                , t' <- cellToAtomValues mTargetConceptDelimiter t origTrg
                                                ]
                       _               -> Nothing
            where origSrc = XLSXLoc file (theSheetName x) (r,sourceCol)
                  origTrg = XLSXLoc file (theSheetName x) (r,targetCol)
       -- | Read a cel, If it is text, it could represent multiple values. This is 
       --   the case if the header cell contains a delimiter. 
       cellToAtomValues 
            :: Maybe Char -- ^ the delimiter, if there is any, used as seperator for multiple values in the cell 
            -> CellValue  -- ^ The value that is read from the cell
            -> Origin     -- ^ the origin of the value.
            -> [PAtomValue]  
       cellToAtomValues mDelimiter cv orig
         = case cv of
             CellText t   -> map (XlsxString orig) 
                           . filter (not . T.null)
                           . unDelimit mDelimiter 
                           . handleSpaces $ t
             CellDouble d -> [XlsxDouble orig d]
             CellBool b -> [ComnBool orig b] 
             CellRich ts -> map (XlsxString orig) 
                          . filter (not . T.null)
                          . unDelimit mDelimiter 
                          . handleSpaces . T.concat . map _richTextRunText $ ts
             CellError e -> fatal . T.intercalate "\n  " $
                                    [ "Error reading cell at:"
                                    , tshow orig
                                    , tshow e]
       unDelimit :: Maybe Char -> Text -> [Text]
       unDelimit mDelimiter xs = 
         case mDelimiter of
           Nothing -> [xs]
           (Just delimiter) -> map trim $ T.split (== delimiter) xs
       handleSpaces = if view trimXLSXCellsL env then trim else id     
    originOfCell :: (Int,Int) -- (row number,col number)
                 -> Origin
    originOfCell (r,c) 
      = XLSXLoc file (theSheetName x) (r,c) 

    value :: (Int,Int) -> Maybe CellValue
    value k = theCellMap x ^? ix k . cellValue . _Just


theSheetCellsForTable :: (Text,Worksheet) -> [SheetCellsForTable]
theSheetCellsForTable (sheetName,ws) 
  =  catMaybes [theMapping i | i <- [0..length tableStarters - 1]]
  where
    tableStarters :: [(Int,Int)]
    tableStarters = filter isStartOfTable $ Map.keys (ws  ^. wsCells)  
      where isStartOfTable :: (Int,Int) -> Bool
            isStartOfTable (rowNr,colNr)
              | colNr /= 1 = False
              | rowNr == 1 = isBracketed' (rowNr,colNr) 
              | otherwise  =           isBracketed'  (rowNr     ,colNr)  
                             && (not . isBracketed') (rowNr - 1, colNr)             
              
    value :: (Int,Int) -> Maybe CellValue
    value k = (ws  ^. wsCells) ^? ix k . cellValue . _Just
    isBracketed' :: (Int,Int) -> Bool
    isBracketed' k = 
       case value k of
         Just (CellText t) -> isBracketed t
         _                 -> False 
        
    theMapping :: Int -> Maybe SheetCellsForTable
    theMapping indexInTableStarters 
     | length okHeaderRows /= nrOfHeaderRows = Nothing  -- Because there are not enough header rows
     | otherwise
     =  Just Mapping { theSheetName = sheetName
                     , theCellMap   = ws  ^. wsCells
                     , headerRowNrs = okHeaderRows
                     , popRowNrs    = populationRows
                     , colNrs       = theCols
                     , debugInfo = [ "indexInTableStarters: "<>tshow indexInTableStarters
                                   , "maxRowOfWorksheet   : "<>tshow maxRowOfWorksheet
                                   , "maxColOfWorksheet   : "<>tshow maxColOfWorksheet
                                   , "startOfTable        : "<>tshow startOfTable
                                   , "firstPopRowNr       : "<>tshow firstPopRowNr
                                   , "lastPopRowNr        : "<>tshow lastPopRowNr
                                   , "[(row,isProperRow)] : "<>T.concat [tshow (r,isProperRow r) | r<-[firstPopRowNr..lastPopRowNr] ]
                                   , "theCols             : "<>tshow theCols
                                   ] 
                     }
     where
       startOfTable = tableStarters `L.genericIndex` indexInTableStarters 
       firstHeaderRowNr = fst startOfTable
       firstColumNr = snd startOfTable
       relationNameRowNr = firstHeaderRowNr
       conceptNameRowNr  = firstHeaderRowNr+1
       nrOfHeaderRows = 2
       maxRowOfWorksheet :: Int
       maxRowOfWorksheet = case L.maximumMaybe (map fst (Map.keys (ws  ^. wsCells))) of
                             Nothing -> fatal "Maximum of an empty list is not defined!"
                             Just m -> m
       maxColOfWorksheet = case L.maximumMaybe (map snd (Map.keys (ws  ^. wsCells))) of
                             Nothing -> fatal "Maximum of an empty list is not defined!"
                             Just m -> m
       firstPopRowNr = firstHeaderRowNr + nrOfHeaderRows
       lastPopRowNr = ((map fst tableStarters<>[maxRowOfWorksheet+1]) `L.genericIndex` (indexInTableStarters+1))-1
       okHeaderRows = filter isProperRow [firstHeaderRowNr,firstHeaderRowNr+nrOfHeaderRows-1]
       populationRows = filter isProperRow [firstPopRowNr..lastPopRowNr]
       isProperRow :: Int -> Bool
       isProperRow rowNr
          | rowNr == relationNameRowNr = True -- The first row was recognized as tableStarter
          | rowNr == conceptNameRowNr  = isProperConceptName(rowNr,firstColumNr)
          | otherwise                  = notEmpty (rowNr,firstColumNr)
       notEmpty k
          = case value k of
            Just (CellText t)   -> (not . T.null . trim) t
            Just (CellDouble _) -> True
            Just (CellBool _)   -> True
            Just (CellRich _)   -> True
            Just (CellError e)  -> fatal $ "Error reading cell "<>tshow e
            Nothing -> False
       theCols = filter isProperCol [1..maxColOfWorksheet]
       isProperCol :: Int -> Bool
       isProperCol colNr
          | colNr == 1 = isProperConceptName (conceptNameRowNr,colNr)
          | otherwise  = isProperConceptName (conceptNameRowNr,colNr) && isProperRelName(relationNameRowNr,colNr)
       isProperConceptName k 
         = case value k of
            Just (CellText t) -> isJust . conceptNameWithOptionalDelimiter $ t
            _ -> False
       isProperRelName k 
         = case value k of
            Just (CellText t) -> (not . T.null . trim) t -- && (isLower . T.head . trim) t
            _ -> False
               
conceptNameWithOptionalDelimiter :: Text -> Maybe ( Text     {- Conceptname -} 
                                                    , Maybe Char {- Delimiter   -}
                                             )
-- Cases:  1) "[" <> Conceptname <> delimiter <> "]"
--         2) Conceptname
--         3) none of above
--  Where Conceptname is any string starting with an uppercase character
conceptNameWithOptionalDelimiter t'
  | isBracketed t   = 
       let mid = T.dropEnd 1 . T.drop 1 $ t
       in case T.uncons . T.reverse $ mid of 
            Nothing -> Nothing 
            Just (d,revInit) -> 
                       let nm = T.reverse revInit
                       in if isDelimiter d && isConceptName (T.reverse nm)
                          then Just (nm , Just d)
                          else Nothing
  | isConceptName t = Just (t, Nothing)
  | otherwise       = Nothing
  where t = trim t'
isDelimiter :: Char -> Bool
isDelimiter = isPunctuation
isConceptName :: Text -> Bool
isConceptName t = case T.uncons t of
                    Nothing  -> False
                    (Just (h,_)) -> isUpper h

-- | trim is used to remove leading and trailing spaces
trim :: Text -> Text
trim = T.reverse . trim' . T.reverse . trim'
  where 
    trim' :: Text -> Text
    trim' t = case uncons t of
               Just (' ',t') -> trim' t'
               _  -> t 
isBracketed :: Text -> Bool
isBracketed t =
    case T.uncons (trim t) of
      Just ('[',tl) -> case T.uncons (T.reverse tl) of
                         Just (']',_) -> True
                         _ -> False
      _ -> False
