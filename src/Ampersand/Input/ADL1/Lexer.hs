{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Ampersand.Input.ADL1.Lexer
    ( keywords
    , operators
    , symbols
    , lexer
    -- LexerMessage
    , LexerError(..)
    , LexerErrorInfo(..)
    , LexerWarning(..)
    , LexerWarningInfo(..)
    , keepOneTabWarning
    , showLexerErrorInfo
    , showLexerWarningInfo
    -- LexerToken
    , Token(..)
    , Lexeme(..)
    , lexemeText
    , initPos
    , FilePos(..)
) where

import           Ampersand.Basics
import           Ampersand.Core.ParseTree
import           Ampersand.Input.ADL1.LexerMessage
import           Ampersand.Input.ADL1.LexerMonad
import           Ampersand.Input.ADL1.LexerToken
import           RIO.Char hiding(isSymbol)
import qualified RIO.List as L
import qualified RIO.Char.Partial as Partial (chr)
import qualified RIO.Set as Set
import qualified RIO.Text as T
import           RIO.Time
import           Numeric

-- | Retrieves a list of keywords accepted by the Ampersand language
keywords :: [String] -- ^ The keywords
keywords  = L.nub $
                [ "CONTEXT", "ENDCONTEXT"
                , "IN" 
                ] ++
                [map toUpper $ show x | x::Lang <- [minBound..]
                ] ++
                [ "INCLUDE"
                , "META"
                , "PATTERN", "ENDPATTERN"
                , "CONCEPT"
                -- Keywords for Relation-statements
                , "RELATION", "PRAGMA", "MEANING"
                ] ++
                [map toUpper $ show x | x::Prop <-[minBound..]
                ] ++ 
                [ "POPULATION", "CONTAINS"
                -- Keywords for rules
                , "RULE", "MESSAGE", "VIOLATION", "TXT"
                ] ++
                [map toUpper $ show x | x::SrcOrTgt <-[minBound..]
                ] ++
                [ "I", "V", "ONE"
                , "ROLE", "MAINTAINS"
                -- Keywords for purposes
                , "PURPOSE", "REF"
                ] ++
                [map toUpper $ show x | x::PandocFormat <-[minBound..]
                ] ++ 
                -- Keywords for interfaces
                [ "INTERFACE", "FOR", "LINKTO", "API"
                , "BOX"
                -- Keywords for identitys
                , "IDENT"
                -- Keywords for views
                , "VIEW", "ENDVIEW", "DEFAULT"
                , "TEMPLATE", "HTML"
                -- Keywords for generalisations:
                , "CLASSIFY", "ISA", "IS"
                -- Keywords for TType:
                , "REPRESENT", "TYPE"
                ]++
                [map toUpper $ show tt | tt::TType <- [minBound..]
                                       , tt /= TypeOfOne 
                ]++
                -- Keywords for values of atoms:
                [ "TRUE", "FALSE" --for booleans
                -- Experimental stuff:
                , "SERVICE"
                -- Depreciated keywords:
                ]

-- | Retrieves a list of operators accepted by the Ampersand language
operators :: [String] -- ^ The operators
operators = [ "|-", "-", "->", "<-", "=", "~", "+", "*", ";", "!", "#",
              "::", ":", "\\/", "/\\", "\\", "/", "<>" , "..", "."]

-- | Retrieves the list of symbols accepted by the Ampersand language
symbols :: String -- ^ The list of symbol characters / [Char]
symbols = "()[],{}<>"

-- | Runs the lexer
lexer :: FilePath   -- ^ The file name, used for error messages
      -> String     -- ^ The content of the file
      -> Either LexerError ([Token], [LexerWarning]) -- ^ Either an error or a list of tokens and warnings
lexer file input = runLexerMonad file (mainLexer (initPos file) input)

-----------------------------------------------------------
-- Help functions
-----------------------------------------------------------

skipLine :: String -> String
skipLine = dropWhile (/= '\n')

-----------------------------------------------------------
-- Lexer definition
-----------------------------------------------------------

type Lexer = FilePos -> String  -> LexerMonad [Token]

mainLexer :: Lexer

-----------------------------------------------------------
-- Removing unnecessary text artifacts (comment, spaces,...)
-----------------------------------------------------------

mainLexer _ [] =  return []

mainLexer p ('-':'-':s) = mainLexer p (skipLine s) --TODO: Test if we should increase line number and reset the column number

mainLexer p (c:s) | isSpace c = let (spc,next) = span isSpaceNoTab s
                                    isSpaceNoTab x = isSpace x && (not .  isTab) x
                                    isTab = ('\t' ==)
                                in  do when (isTab c) (lexerWarning TabCharacter p)
                                       mainLexer (foldl' updatePos p (c:spc)) next

mainLexer p ('{':'-':s) = lexNestComment mainLexer (addPos 2 p) s
mainLexer p ('{':'+':s) = lexMarkup mainLexer (addPos 2 p) s
mainLexer p ('"':ss) =
    let (s,swidth,rest) = scanString ss
    in case rest of
         ('"':xs) -> returnToken (LexString s) p mainLexer (addPos (swidth+2) p) xs
         _        -> lexerError (NonTerminatedString s) p
         

-----------------------------------------------------------
-- looking for keywords - operators - special chars
-----------------------------------------------------------

-- Special case for < since it's the beginning of operators but also a symbol when alone
mainLexer p ('<':d:s) = if isOperator ['<',d]
                        then returnToken (LexOperator ['<',d]) p mainLexer (addPos 2 p) s
                        else returnToken (LexSymbol    '<')    p mainLexer (addPos 1 p) (d:s)

mainLexer p cs@(c:s)
     | isIdStart c || isUpper c
         = let (name', p', s')    = scanIdent (addPos 1 p) s
               name''               = c:name'
               tokt   | iskw name'' = LexKeyword name''
                      | otherwise = if isIdStart c
                                    then LexVarId name''
                                    else LexConId name''
           in returnToken tokt p mainLexer p' s'
     | isOperatorBegin c
         = let (name', s') = getOp cs
           in returnToken (LexOperator name') p mainLexer (foldl' updatePos p name') s'
     | isSymbol c = returnToken (LexSymbol c) p mainLexer (addPos 1 p) s
     | isDigit c
         = case  getDateTime cs of
            Just (Right (tk,_,width,s')) -> returnToken tk p mainLexer (addPos width p) s'
            Just (Left msg) -> lexerError msg p
            Nothing 
              -> case getDate cs of
                  Just (tk,_,width,s') -> returnToken tk p mainLexer (addPos width p) s'
                  Nothing -> let (tk,_,width,s') = getNumber cs
                             in  returnToken tk p mainLexer (addPos width p) s'
       -- Ignore unexpected characters in the beginning of the file because of the UTF-8 BOM marker.
       -- TODO: Find out the right way of handling the BOM marker.
     | beginFile p = do { lexerWarning UtfChar p; mainLexer p s }
     | otherwise  = lexerError (UnexpectedChar c) p
    where beginFile (FilePos _ 1 1) = True
          beginFile _ = False

-----------------------------------------------------------
-----------------------------------------------------------
-- Supporting functions for MainLexer
-----------------------------------------------------------
-----------------------------------------------------------

-----------------------------------------------------------
-- Check on keywords - operators - special chars
-----------------------------------------------------------

locatein :: Ord a => [a] -> a -> Bool
locatein es e = e `elem` Set.fromList es

iskw :: String -> Bool
iskw = locatein keywords

isSymbol :: Char -> Bool
isSymbol = locatein symbols

isOperator :: String -> Bool
isOperator  = locatein operators

isOperatorBegin :: Char -> Bool
isOperatorBegin  = locatein (mapMaybe head operators)
   where head :: [a] -> Maybe a
         head (a:_) = Just a
         head []    = Nothing
isIdStart :: Char -> Bool
isIdStart c = isLower c || c == '_'

isIdChar :: Char -> Bool
isIdChar c =  isAlphaNum c || c == '_'

-- Finds the longest prefix of cs occurring in keywordsops
getOp :: String -> (String, String)
getOp cs = findOper operators cs ""
    where findOper :: [String] -> String -> String -> (String,String)
          findOper []  _     _  = ("", cs)
          findOper _   []    op = (op,[])
          findOper ops (c:rest) op =
              if null found then (op, c:rest)
              else findOper found rest (op ++ [c])
              where found = [s' | o:s'<-ops, c==o]

-- scan ident receives a file position and the resting contents, returning the scanned identifier, the file location and the resting contents.
scanIdent :: FilePos -> String -> (String, FilePos, String)
scanIdent p s = let (nm,rest) = span isIdChar s
                in (nm,addPos (length nm) p,rest)


-----------------------------------------------------------
-- String clean-up functions / comments
-----------------------------------------------------------

lexNestComment :: Lexer -> Lexer
lexNestComment c p ('-':'}':s) = c (addPos 2 p) s
lexNestComment c p ('{':'-':s) = lexNestComment (lexNestComment c) (addPos 2 p) s
lexNestComment c p (x:s)       = lexNestComment c (updatePos p x) s
lexNestComment _ p []          = lexerError UnterminatedComment p

lexMarkup :: Lexer -> Lexer
lexMarkup = lexMarkup' ""
 where 
       lexMarkup' str _ p ('+':'}':s) = returnToken (LexMarkup str) p mainLexer (addPos 2 p)  s
       lexMarkup' str c p (x:s)       = lexMarkup' (str++[x]) c (updatePos p x) s
       lexMarkup' _   _ p []          = lexerError UnterminatedMarkup p

-----------------------------------------------------------
-- iso 8601 date / time
-----------------------------------------------------------
-- Returns tuple with the parsed lexeme, the UTCTime, the amount of read characters and the rest of the text
getDateTime :: String -> Maybe (Either LexerErrorInfo (Lexeme, UTCTime, Int, String) )
getDateTime cs = 
  case getDate cs of
   Nothing -> Nothing
   Just (_,day,ld,rd) -> 
      case getTime rd of
        Nothing -> case rd of
                    'T':_ -> Just . Left $ ProblematicISO8601DateTime
                    _     -> Nothing
        Just (timeOfDay, tzoneOffset,lt,rt) -> 
            let ucttime = addUTCTime tzoneOffset (UTCTime day timeOfDay)
            in Just . Right $ 
                    ( LexDateTime ucttime
                    , ucttime
                    , ld + lt
                    , rt
                    )
getTime :: String -> Maybe (DiffTime, NominalDiffTime, Int, String)
getTime cs =
  case cs of
   'T':h1:h2:':':m1:m2:rest 
    -> if all isDigit [h1,h2,m1,m2]
       then let (_,Left hours,_,_) = getNumber [h1,h2]
                (_,Left minutes,_,_) = getNumber [m1,m2]
                (seconds,ls,rs) = getSeconds rest
            in case getTZD rs of
                 Nothing -> Nothing
                 Just (offset,lo,ro)
                   -> if hours < 24 && minutes < 60 && seconds < 60
                      then Just (fromRational . toRational $
                                   ( fromIntegral hours*60
                                    +fromIntegral minutes
                                   )*60+ seconds
                                ,offset
                                ,1+5+ls+lo
                                ,ro)
                      else Nothing
       else Nothing
   _ -> Nothing
   
getSeconds :: String -> (Float,Int,String)
getSeconds cs =
 case cs of
  (':':s1:s2:rest) ->  
     if all isDigit [s1,s2]
     then let (fraction,lf,rf) = getFraction (s1:s2:rest)
          in (fraction,1+lf,rf)
     else (0,0,cs)
  _ -> (0,0,cs)
getFraction :: String -> (Float,Int,String)
getFraction cs = 
  case readFloat cs of
   [(a,str)] -> (a, length cs - length str, str) --TODO: Make more efficient.
   _ -> (0,0,cs)
   


getTZD :: String -> Maybe (NominalDiffTime, Int,String)
getTZD cs = 
 case cs of
  'Z':rest -> Just (0,1,rest)
  '+':h1:h2:':':m1:m2:rest -> mkOffset [h1,h2] [m1,m2] rest (+)
  '-':h1:h2:':':m1:m2:rest -> mkOffset [h1,h2] [m1,m2] rest (-)
  _ -> Nothing
  where
   mkOffset :: String -> String -> String -> (Int -> Int -> Int) -> Maybe (NominalDiffTime, Int,String)
   mkOffset hs ms rest op = 
    let (_,Left hours  ,_,_) = getNumber hs
        (_,Left minutes,_,_) = getNumber ms
        total = hours*60+minutes
    in if hours <= 24 && minutes < 60
       then Just (fromRational . toRational $ 0 `op` total
                 ,6,rest) 
       else Nothing 
getDate :: String -> Maybe (Lexeme, Day, Int, String)
getDate cs =
  case cs of
   y1:y2:y3:y4:'-':m1:m2:'-':d1:d2:rest ->
      if all isDigit [y1,y2,y3,y4,m1,m2,d1,d2] 
      then case fromGregorianValid (toInteger year) month day of
             Nothing -> Nothing 
             Just d -> Just (LexDate d, d, 10, rest)
      else Nothing
     where (_,Left year ,_,_) = getNumber [y1,y2,y3,y4]
           (_,Left month,_,_) = getNumber [m1,m2]
           (_,Left day  ,_,_) = getNumber [d1,d2]
   _  -> Nothing
     
-----------------------------------------------------------
-- Numbers
-----------------------------------------------------------
-- Returns tuple with the parsed lexeme, the integer, the amount of read characters and the rest of the text
getNumber :: String -> (Lexeme, Either Int Double, Int, String)
getNumber str =
  case readDec str of
    [(_,'.':_)] -> case readFloat str of
                           [(flt,rest)] -> (LexFloat flt, Right flt, length str - length rest,rest)
                           _            -> fatal "Unexpected: can read decimal, but not float???"
    [(dec,rest)]  -> (LexDecimal dec , Left dec, length str - length rest,rest)
    _  -> fatal $ "No number to read!\n  " <> T.take 40 (T.pack str)
--getNumber :: String -> (Lexeme, (Either Int Double), Int, String)
--getNumber [] = fatal "getNumber"
--getNumber cs@(c:s)
--  | c /= '0'         = num10
--  | null s           = const0
--  | hs `elem` "xX"   = num16
--  | hs `elem` "oO"   = num8
--  | otherwise        = num10
--  where (hs:ts) = s
--        const0 = (LexDecimal 0, Left 0, 1, s)
--        num10 :: (Lexeme, (Either Int Double), Int, String)
--        num10  = let (n, rs) = span isDigit cs
--                     (isWhole,readed,rest) = 
--                        case rs of
--                          '.':cs' -> let (n',rs') = span isDigit cs'
--                                         wholeNumberString = n++"."++n'
--                                     in (all (=='0') n',wholeNumberString,rs')
--                          _       -> (True,n, rs)
--                     nrInt = read n
--                 in if isWhole
--                    then (LexDecimal nrInt , Left nrInt, length readed,rest)
--                    else let x = fst.head.readFloat $ readed
--                         in (LexFloat x, Right x, length readed,rest)
--        num16   = readIntNum isHexaDigit  16 LexHex
--        num8    = readIntNum isOctDigit 8  LexOctal
--        readIntNum :: (Char -> Bool) -> Int -> (Int -> Lexeme) -> (Lexeme, Either Int a , Int, String)
--        readIntNum p base lx
--          = let (n, rs) = span p ts
--            in  if null n
--                then const0
--                else let nr = readn base n
--                     in (lx nr, Left nr, 2 + length n, rs)

-----------------------------------------------------------
-- characters / strings
-----------------------------------------------------------
scanString :: String -> (String, Int, String)
scanString = scanUpto ['"']

-- | scan to some given character. The end char is scanned away too
scanUpto :: String  -- non-empty list of ending characters
         -> String 
         -> (String, Int, String)
scanUpto echrs s = 
 case s of
   xs       -> let (ch,cw,cr) = getchar echrs xs
                   (str,w,r)  = scanUpto echrs cr
               in maybe ("",0,xs) (\c -> (c:str,cw+w,r)) ch

getchar :: String  -- non-empty list of ending characters
        -> String  -- string to get the character from
        -> (Maybe Char, Int, String)
getchar echrs s =
  case s of
   []          -> (Nothing,0,[])
   ('\n':_ )   -> (Nothing,0,s)
   ('\t':_ )   -> (Nothing,0,s)
   ('\\':'&':xs) -> let (str,w,r) = getchar echrs xs -- Special case is required because an escaped & is equal to empty string in Haskell
                    in (str,w+2,r)
   ('\\':xs)   -> let (c,l,r) = getEscChar xs
                  in (c,l+1,r)
   (x:xs)      
    | x `elem` echrs   -> (Nothing,0,s)
    | otherwise -> (Just x,1,xs)

getEscChar :: String -> (Maybe Char, Int, String)
getEscChar [] = (Nothing,0,[])
getEscChar s@(x:xs) | isDigit x = case readDec s of
                                    [(val,rest)]
                                      | val >= 0 && val <= ord (maxBound :: Char) -> (Just (Partial.chr val),length s - length rest, rest)
                                      | otherwise -> (Nothing, 1, rest)
                                    _  -> fatal $ "Impossible! first char is a digit.. "<>T.take 40 (T.pack s)
                    | x `elem` ['\"','\''] = (Just x,2,xs)
                    | otherwise = case x `lookup` cntrChars of
                                 Nothing -> (Nothing,0,s)
                                 Just c  -> (Just c,1,xs)
  where cntrChars = [('a','\a'),('b','\b'),('f','\f'),('n','\n'),('r','\r'),('t','\t')
                    ,('v','\v'),('\\','\\')]

-----------------------------------------------------------
-- Token creation function
-----------------------------------------------------------

returnToken :: Lexeme -> FilePos -> Lexer -> Lexer
returnToken lx fpos continue posi input = do
    let token = Tok lx fpos
    tokens <- continue posi input
    return (token:tokens)
