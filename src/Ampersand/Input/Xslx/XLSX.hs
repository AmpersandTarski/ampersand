{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Ampersand.Input.Xslx.XLSX 
  (parseXlsxFile)
where
import           Ampersand.Basics hiding (view, (^.))
import           Ampersand.Core.ParseTree
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

parseXlsxFile :: (HasFSpecGenOpts env) => 
    Maybe FileKind -> FilePath -> RIO env (Guarded [P_Population])
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
      => env -> Xlsx -> Guarded [P_Population]
  xlsx2pContext env xlsx = Checked pop []
    where 
      pop = concatMap (toPops env file)
          . concatMap theSheetCellsForTable 
          $ (xlsx ^. xlSheets)

data SheetCellsForTable 
       = Mapping{ theSheetName :: Text
                , theCellMap   :: CellMap
                , headerRowNrs :: (Int,Int) -- Two rows denoting the header of the Mapping
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
       mbSign :: Maybe P_Sign
       mbSign = P_Sign <$> src <*> trg 
       popOrigin :: Origin
       popOrigin = originOfCell (relNamesRow, targetCol)
       (relNamesRow,conceptNamesRow) = headerRowNrs x
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
  = mapMaybe theMapping [0..length tableStarters - 1]
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
    theMapping indexInTableStarters =
       case okHeaderRows of
         Nothing -> Nothing
         Just headers -> Just 
             Mapping { theSheetName = sheetName
                     , theCellMap   = ws  ^. wsCells
                     , headerRowNrs = headers
                     , popRowNrs    = populationRows
                     , colNrs       = theCols
                     , debugInfo = [ "indexInTableStarters: "<>tshow indexInTableStarters
                                   , "maxRowOfWorksheet   : "<>tshow maxRowOfWorksheet
                                   , "maxColOfWorksheet   : "<>tshow maxColOfWorksheet
                                   , "startOfTable        : "<>tshow startOfTable
                                   , "firstPopRowNr       : "<>tshow firstPopRowNr
                                   , "lastPopRowNr        : "<>tshow lastPopRowNr
                                   , "[(row,isProperRow)] : "<>T.concat [tshow (r,isProperRow r) | r<- [firstPopRowNr..lastPopRowNr]]
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
       okHeaderRows = case filter isProperRow [firstHeaderRowNr,firstHeaderRowNr+nrOfHeaderRows-1] of
                        [a,b] -> Just (a,b)
                        _     -> Nothing
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
