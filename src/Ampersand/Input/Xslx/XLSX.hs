{-# LANGUAGE OverloadedStrings, DuplicateRecordFields,OverloadedLabels #-}
module Ampersand.Input.Xslx.XLSX 
  (parseXlsxFile)
where
import           Ampersand.ADL1
import           Ampersand.Basics hiding (view, (^.))
import           Ampersand.Input.ADL1.CtxError
import           Ampersand.Misc
import           Ampersand.Prototype.StaticFiles_Generated (getStaticFileContent, FileKind)
import           Codec.Xlsx
import           Control.Lens -- ((^?),ix)
import qualified RIO.List as L
import qualified RIO.ByteString.Lazy as BL
import           RIO.Char
import qualified RIO.Map as Map
import qualified RIO.Text as T
import           Data.Tuple

parseXlsxFile :: (HasExcellOutputOptions env) => 
    Maybe FileKind -> FilePath -> RIO env (Guarded [P_Population])
parseXlsxFile mFk file =
  do env <- ask
     bytestr <- 
        case mFk of
          Just fileKind 
             -> case getStaticFileContent fileKind file of
                      Just cont -> return $ fromString cont
                      Nothing -> fatal ("Statically included "++ show fileKind++ " files. \n  Cannot find `"++file++"`.")
          Nothing
             -> liftIO $ BL.readFile file
     return . xlsx2pContext env . toXlsx $ bytestr
 where
  xlsx2pContext :: (HasExcellOutputOptions env ) 
      => env -> Xlsx -> Guarded [P_Population]
  xlsx2pContext env xlsx = Checked pop []
    where 
      pop = concatMap (toPops env file)
          . concatMap theSheetCellsForTable 
          $ (xlsx ^. xlSheets)

data SheetCellsForTable 
       = Mapping{ theSheetName :: String
                , theCellMap   :: CellMap
                , headerRowNrs :: [Int]
                , popRowNrs    :: [Int]
                , colNrs       :: [Int]
                , debugInfo :: [String]
                }
instance Show SheetCellsForTable where  --for debugging only
  show x 
   = unlines $
      [ "Sheet       : "++theSheetName x
      , "headerRowNrs: "++show (headerRowNrs x)
      , "popRowNrs   : "++show (popRowNrs x)
      , "colNrs      : "++show (colNrs x)
      ] ++ debugInfo x 
toPops :: (HasExcellOutputOptions env) => env -> FilePath -> SheetCellsForTable -> [P_Population]
toPops env file x = map popForColumn (colNrs x)
  where
    popForColumn :: Int -> P_Population
    popForColumn i =
      if i  == sourceCol  
      then  P_CptPopu { pos = popOrigin
                      , p_cnme = sourceConceptName 
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
       src, trg :: Maybe String
       (src,trg) = case mTargetConceptName of
                  Just tCptName -> (if isFlipped' then swap else id) (Just sourceConceptName, Just tCptName)
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
       sourceConceptName :: String
       mSourceConceptDelimiter :: Maybe Char
       (sourceConceptName, mSourceConceptDelimiter)
          = case value (conceptNamesRow,sourceCol) of
                Just (CellText t) -> 
                   fromMaybe (fatal "No valid source conceptname found. This should have been checked before")
                             (conceptNameWithOptionalDelimiter . trim $ t)
                _ -> fatal "No valid source conceptname found. This should have been checked before"
       mTargetConceptName :: Maybe String
       mTargetConceptDelimiter :: Maybe Char
       (mTargetConceptName, mTargetConceptDelimiter)
          = case value (conceptNamesRow,targetCol) of
                Just (CellText t) -> let (nm,mDel) 
                                           = fromMaybe
                                                (fatal "No valid source conceptname found. This should have been checked before")
                                                (conceptNameWithOptionalDelimiter . trim $ t)
                                     in (Just nm, mDel)
                _ -> (Nothing, Nothing)
       relName :: String
       isFlipped' :: Bool
       (relName,isFlipped') 
          = case value (relNamesRow,targetCol) of
                Just (CellText t) -> 
                    case T.unpack . T.reverse . trim $ t of
                      '~':rest -> (reverse rest, True )
                      xs       -> (reverse xs  , False)
                _ -> fatal ("No valid relation name found. This should have been checked before" ++show (relNamesRow,targetCol))
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
       cellToAtomValues :: Maybe Char -> CellValue -> Origin -> [PAtomValue]  -- The value in a cell can contain the delimeter of the row
       cellToAtomValues mDelimiter cv orig
         = case cv of
             CellText t   -> map (XlsxString orig . T.unpack) 
                           . filter (not . T.null)
                           . unDelimit mDelimiter 
                           . handleSpaces $ t
             CellDouble d -> [XlsxDouble orig d]
             CellBool b -> [ComnBool orig b] 
             CellRich ts -> map (XlsxString orig . T.unpack) 
                          . filter (not . T.null)
                          . unDelimit mDelimiter 
                          . handleSpaces . T.concat . map _richTextRunText $ ts
             CellError e -> fatal . L.intercalate "\n  " $
                                    [ "Error reading cell at:"
                                    , show orig
                                    , show e]
       unDelimit :: Maybe Char -> T.Text -> [T.Text]
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


theSheetCellsForTable :: (T.Text,Worksheet) -> [SheetCellsForTable]
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
     =  Just Mapping { theSheetName = T.unpack sheetName
                     , theCellMap   = ws  ^. wsCells
                     , headerRowNrs = okHeaderRows
                     , popRowNrs    = populationRows
                     , colNrs       = theCols
                     , debugInfo = [ "indexInTableStarters"++": "++show indexInTableStarters
                                   , "maxRowOfWorksheet"++": "++show maxRowOfWorksheet
                                   , "maxColOfWorksheet"++": "++show maxColOfWorksheet
                                   , "startOfTable     "++": "++show startOfTable
                                   , "firstPopRowNr    "++": "++show firstPopRowNr
                                   , "lastPopRowNr     "++": "++show lastPopRowNr
                                   , "[(row,isProperRow)] "++": "++concatMap show [(r,isProperRow r) | r<- [firstPopRowNr..lastPopRowNr]]
                                   , "theCols          "++": "++show theCols
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
                             Nothing -> fatal "Maximum of an empty list is undefined!"
                             Just m -> m
       maxColOfWorksheet = case L.maximumMaybe (map snd (Map.keys (ws  ^. wsCells))) of
                             Nothing -> fatal "Maximum of an empty list is undefined!"
                             Just m -> m
       firstPopRowNr = firstHeaderRowNr + nrOfHeaderRows
       lastPopRowNr = ((map fst tableStarters++[maxRowOfWorksheet+1]) `L.genericIndex` (indexInTableStarters+1))-1
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
            Just (CellError e)  -> fatal $ "Error reading cell "++show e
            Nothing -> False
       theCols = filter isProperCol [1..maxColOfWorksheet]
       isProperCol :: Int -> Bool
       isProperCol colNr
          | colNr == 1 = isProperConceptName (conceptNameRowNr,colNr)
          | otherwise  = isProperConceptName (conceptNameRowNr,colNr) && isProperRelName(relationNameRowNr,colNr)
       isProperConceptName k 
         = case value k of
            Just (CellText t) -> isJust . conceptNameWithOptionalDelimiter . trim $ t
            _ -> False
       isProperRelName k 
         = case value k of
            Just (CellText t) -> (not . T.null . trim) t -- && (isLower . T.head . trim) t
            _ -> False
               
conceptNameWithOptionalDelimiter :: T.Text -> Maybe ( String     {- Conceptname -} 
                                                    , Maybe Char {- Delimiter   -}
                                             )
-- Cases:  1) "[" ++ Conceptname ++ delimiter ++ "]"
--         2) Conceptname
--         3) none of above
--  Where Conceptname is any string starting with an uppercase character
conceptNameWithOptionalDelimiter t
  | isBracketed t   = 
       let mid = T.dropEnd 1 . T.drop 1 $ t
       in case T.uncons . T.reverse $ mid of 
            Nothing -> Nothing 
            Just (d,revInit) -> 
                       let nm = T.reverse revInit
                       in if isDelimiter d && isConceptName (T.reverse nm)
                          then Just (T.unpack nm , Just d)
                          else Nothing
  | isConceptName t = Just (T.unpack t, Nothing)
  | otherwise       = Nothing
           
isDelimiter :: Char -> Bool
isDelimiter = isPunctuation
isConceptName :: T.Text -> Bool
isConceptName t = case T.uncons t of
                    Nothing  -> False
                    (Just (h,_)) -> isUpper h

-- | trim is used to remove leading and trailing spaces
trim :: T.Text -> T.Text
trim = T.reverse . trim' . T.reverse . trim'
  where 
    trim' :: T.Text -> T.Text
    trim' t = case uncons t of
               Just (' ',t') -> trim' t'
               _  -> t 
isBracketed :: T.Text -> Bool
isBracketed t =
    case T.uncons (trim t) of
      Just ('[',tl) -> case T.uncons (T.reverse tl) of
                         Just (']',_) -> True
                         _ -> False
      _ -> False
