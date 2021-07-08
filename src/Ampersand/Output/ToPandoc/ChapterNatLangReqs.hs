{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Ampersand.Output.ToPandoc.ChapterNatLangReqs (
      chpNatLangReqs
 ) where

import           Ampersand.Output.ToPandoc.SharedAmongChapters
import           RIO.Char hiding (Space)
import qualified RIO.List as L
import qualified RIO.NonEmpty as NE
import qualified RIO.Set as Set
import qualified RIO.Text as T
import qualified RIO.Text.Partial as T'(splitOn)

chpNatLangReqs :: (HasDirOutput env, HasDocumentOpts env) 
   => env -> Int -> FSpec -> Blocks
chpNatLangReqs env lev fSpec =
      --  *** Header ***
   xDefBlck env fSpec SharedLang
   <> --  *** Intro  ***
    case outputLang' of
        Dutch   -> para
                     (  "Dit hoofdstuk beschrijft functionele eisen ten behoeve van "
                     <> (singleQuoted.str.name) fSpec
                     <> " in natuurlijke taal. "
                     <> "Het hoofdstuk bevat definities en afspraken. "
                     <> "Hiermee wordt beoogd dat verschillende belanghebbenden hun afspraken op dezelfde manier kunnen begrijpen. "
                     <> "Alle definities en afspraken zijn genummerd omwille van de traceerbaarheid. "
                     )
        English -> para
                     (  "This chapter describes functional requirements for "
                     <> (singleQuoted.str.name) fSpec
                     <> " in natural language. "
                     <> "It contains definitions and agreements. "
                     <> "The purpose of this chapter is to create shared understanding among stakeholders. "
                     <> "All definitions and agreements have been numbered for the sake of traceability. "
                     )
   <> --  *** Requirements ***
   (mconcat . map printOneTheme . orderingByTheme env) fSpec
   <> --  *** Legal Refs ***
     if genLegalRefs then legalRefs else mempty

  where
  -- shorthand for easy localizing    
  l :: LocalizedStr -> Text
  l = localize outputLang'
  outputLang' = outputLang env fSpec
  genLegalRefs = view genLegalRefsL env
  legalRefs :: Blocks
  legalRefs =  header (lev+2) sectionTitle
            <> legacyTable caption'
                     [(AlignLeft,1/4),(AlignLeft,3/4)]
                     [plain lawHeader, plain articleHeader]  --headers
                     [ [(para.str.aOlLaw) art  , (para.str.unscanRef.aOlArt) art]
                     | art <-(L.sort . L.nub . concatMap getArticlesOfLaw.getRefs) fSpec  ]

         where (sectionTitle, lawHeader, articleHeader, caption') =
                 case outputLang' of
                   Dutch   -> ("Referentietabel", "Wet", "Artikel", "Referentietabel van de wetsartikelen")
                   English -> ("Reference table", "Law", "Article", "Reference table of articles of law")
               getRefs ::FSpec ->  [LawRef]
               getRefs = concatMap (mapMaybe toLawRef . explRefIds) . purposesOf fSpec outputLang'


  -- | printOneTheme tells the story in natural language of a single theme.
  -- For this to work out, Ampersand authors should take care in composing PURPOSEs.
  printOneTheme :: ThemeContent -> Blocks
  printOneTheme tc 
    | isNothing (patOfTheme tc) &&
        null (cptsOfTheme tc) &&
        null (dclsOfTheme tc) &&
        null (rulesOfTheme tc) = mempty
    | otherwise =
             --  *** Header of the theme: ***
            xDefBlck env fSpec (XRefSharedLangTheme (patOfTheme tc))
          <> --  *** Purpose of the theme: ***
             (case patOfTheme tc of
                 Nothing  -> 
                   (para.str.l) 
                     (NL "Deze paragraaf beschrijft de relaties en concepten die niet in voorgaande secties zijn beschreven."
                     ,EN "This paragraph shows remaining artifacts that have not been described in previous paragraphs."
                     )
                 Just pat -> 
                   case purposesOf fSpec outputLang' pat of
                     []    -> printIntro    (cptsOfTheme tc)
                     purps -> purposes2Blocks env purps
             )
          <> (mconcat . map (\x->printConceptDef env l x<>printConceptPurp x). cptsOfTheme ) tc
          <> (mconcat . map printRel     . dclsOfTheme ) tc
          <> (mconcat . map printRule    . rulesOfTheme) tc
      where
-- The following paragraph produces an introduction of one theme (i.e. pattern or process).
       printIntro :: [Numbered CptCont] -> Blocks
       printIntro [] = mempty
       printIntro nCpts
         = case patOfTheme tc of
             Nothing  -> mempty
             Just pat -> 
                 para ((str.l) (NL "In het volgende wordt de taal geïntroduceerd ten behoeve van "
                                 ,EN "The sequel introduces the language of ")
                              <> (str.name) pat <> ".")
              <>
{-
                ( case nCpts of
                   [] 
                     -> fatal "Unexpected. There should be at least one concept to introduce."
                   [x]
                     -> para(   (str.l) (NL "Nu volgt de definitie van het begrip "
                                        ,EN "At this point, the definition of ")
                             <> (showCpt x) 
                             <> (str.l) (NL "."
                                        ,EN " is given.")
                            )
                   _ 
                     -> para(   (str.l) (NL "Nu volgen definities van de begrippen "
                                        ,EN "At this point, the definitions of ")
                             <> commaPandocAnd outputLang' (map showCpt (sortWith theNr nCpts)) 
                             <> (str.l) (NL "."
                                        ,EN " are given.")
                             )
               )<>
-}
               ( case filter hasMultipleDefs nCpts of
                   []  -> mempty
                   [x] -> para(  (str.l) (NL "Het begrip "
                                         ,EN "Concept ")
                              <> showCpt x 
                              <> (str.l) (NL " heeft meerdere definities."
                                         ,EN " is multiple defined.")
                              )
                   multipleDefineds
                       -> para(  (str.l) (NL "De begrippen "
                                         ,EN "Concepts ")
                              <> commaPandocAnd outputLang' (map showCpt multipleDefineds) 
                              <> (str.l) (NL " hebben meerdere definities."
                                         ,EN " are multiple defined.")
                              )
               )
         where
           showCpt :: Numbered CptCont -> Inlines
           showCpt = emph.text.name.cCpt.theLoad
           hasMultipleDefs :: Numbered CptCont -> Bool
           hasMultipleDefs x = 
              case cCptDefs (theLoad x) of
                (_:_:_) -> True
                _       -> False       


  printRel :: Numbered DeclCont -> Blocks
  printRel nDcl =
         (printPurposes . cDclPurps . theLoad) nDcl
      <> definitionList 
            [(   (str.l) (NL "Relatie ", EN "Relation ")
              <> (text . tshow . theNr $ nDcl) <> ": " <> xDefInln env fSpec (XRefSharedLangRelation dcl)
             , 
                [printMeaning outputLang' dcl]
              <>(case Set.elems $ properties dcl of
                    []  -> mempty
                    ps  -> [plain (   (str.l) (NL "Deze relatie is ",EN "This relation is " )
                                   <> (commaPandocAnd outputLang' (map (str . propFullName False outputLang') ps)<>"."
                                      )
                                  )
                           ]
                )    
             )   
            ]
      <> case samples of
            []  -> mempty
            [_] -> plain ((str.l) (NL "Een frase die hiermee gemaakt kan worden is bijvoorbeeld:"
                                  ,EN "A phrase that can be formed is for instance:")
                         )
            _   -> plain ((str.l) (NL "Frasen die hiermee gemaakt kunnen worden zijn bijvoorbeeld:"
                                  ,EN "Phrases that can be made are for instance:")
                         )
      <> case samples of
            []  -> mempty
            _   -> bulletList . map (plain . mkPhrase dcl) $ samples
    where dcl = cDcl . theLoad $ nDcl
          
          samples = take 3 . Set.elems . cDclPairs . theLoad $ nDcl
  printRule :: Numbered RuleCont -> Blocks
  printRule nRul =
         (printPurposes . cRulPurps . theLoad) nRul
      <> definitionList 
            [(   str (l (NL "Afspraak ", EN "Agreement "))
              <> (text . tshow . theNr $ nRul) <>": "
              <> xDefInln env fSpec (XRefSharedLangRule rul)<>"."

             , case (cRulMeanings . theLoad) nRul of
                 [] -> [plain $
                            (str.l) (NL "Deze regel ",EN "The rule ")
                         <> (str.l) (NL " is ongedocumenteerd.",EN " is undocumented.")
                       ]
                 ms -> fmap (printMarkup . ameaMrk) ms
             )
            ]
     where rul = cRul . theLoad $ nRul
  mkPhrase :: Relation -> AAtomPair -> Inlines
  mkPhrase decl pair -- srcAtom tgtAtom
   | T.null (prL<>prM<>prR)
                   =    (atomShow . upCap) srcAtom
                     <> (pragmaShow.l) (NL " correspondeert met ", EN " corresponds to ")
                     <> atomShow tgtAtom
                     <> (pragmaShow.l) (NL " in de relatie ",EN " in relation ")
                     <> atomShow (name decl)
                     <> "."
   | otherwise
                  =    (if T.null prL then mempty
                         else pragmaShow (upCap prL) <> " ")
                     <> atomShow srcAtom <> " "
                     <> (if T.null prM then mempty
                         else pragmaShow prM <> " ")
                     <> atomShow tgtAtom
                     <> (if T.null prR then mempty
                         else " " <> pragmaShow prR)
                     <> "."
   where srcAtom = showValADL (apLeft pair)
         tgtAtom = showValADL (apRight pair)
         prL = decprL decl
         prM = decprM decl
         prR = decprR decl
         atomShow = str
         pragmaShow = emph . str
                   
newtype LawRef = LawRef { lawRef :: Text}
data ArticleOfLaw = ArticleOfLaw { aOlLaw :: Text
                                 , aOlArt :: [Either Text Int]
                                 } deriving Eq
toLawRef:: Text -> Maybe LawRef
toLawRef txt = if T.null txt then Nothing else Just (LawRef txt)
wordsOf :: LawRef -> NE.NonEmpty Text
wordsOf ref = case T.words . lawRef $ ref of
                [] -> fatal "text in LaWRef must not be empty."
                h:tl -> h NE.:| tl
-- the article is everything but the law (and we also drop any trailing commas)
getArticlesOfLaw :: LawRef -> [ArticleOfLaw]
getArticlesOfLaw ref = map buildLA . T'.splitOn ", " . T.unwords . NE.init . wordsOf $ ref
                             
   where
     buildLA :: Text -> ArticleOfLaw
     buildLA art = ArticleOfLaw ((NE.last . wordsOf) ref) (scanRef art)
       where
    -- group string in number and text sequences, so "Art 12" appears after "Art 2" when sorting (unlike in normal lexicographic string sort)
         scanRef :: Text -> [Either Text Int]
         scanRef txt = case T.uncons txt of
           Nothing -> mempty
           Just (c,_) | isDigit c -> scanRefInt txt
                      | otherwise -> scanRefTxt txt
         scanRefTxt :: Text -> [Either Text Int]
         scanRefTxt txt = case T.uncons txt of
           Nothing -> mempty
           Just _ -> let (txt', rest) = T.break isDigit txt'
                           in  Left txt' : scanRefInt rest

         scanRefInt :: Text -> [Either Text Int]
         scanRefInt txt = case T.uncons txt of
           Nothing -> mempty
           Just _  -> Right 
                        (case readMaybe (T.unpack digits) of
                          Nothing  -> fatal $ "Impossible: This cannot be interpreted as digits: "<> digits
                          Just x -> x
                        ) : scanRefTxt rest
                  where (digits, rest) = T.span isDigit txt

instance Ord ArticleOfLaw where
 compare a b =
   case compare (aOlLaw a) (aOlLaw b) of
     EQ   -> compare (aOlArt a) (aOlArt b)
     ord' -> ord'

unscanRef :: [Either Text Int] -> Text
unscanRef = T.concat . fmap (either id tshow)

             
             
