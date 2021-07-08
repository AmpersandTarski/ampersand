{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Ampersand.Output.ToPandoc.SharedAmongChapters
    ( module Text.Pandoc.Builder
    , module Text.Pandoc
    , module Ampersand.ADL1
    , module Ampersand.Basics
    , module Ampersand.Core.ShowAStruct
    , module Ampersand.FSpec
    , module Ampersand.Graphic.Graphics
    , module Ampersand.Misc.HasClasses
    , module Ampersand.Output.PandocAux
    , module Ampersand.Classes
    , Chapter(..)
    , chaptersInDoc
    , Xreferenceble(..)
    , CustomSection(..)
    , pandocEqnArray
    , pandocEquationWithLabel
    , Purpose(..)
    , printMeaning
    , printMarkup
    , printPurposes
    , purposes2Blocks
    , meaning2Blocks
    , violation2Inlines
    , isMissing
    , lclForLang
    , dpRule'
    , ThemeContent(..), orderingByTheme
    , Numbered(..), RuleCont(..),DeclCont(..),CptCont(..)
    , plainText
    , showPredLogic
    , legacyTable
    , printConceptDef
    , printConceptPurp
    )
where
import           Ampersand.ADL1 hiding (Meta)
import           Ampersand.Basics hiding (Reader,Identity,toList,link)
import           Ampersand.Classes
import           Ampersand.Core.ShowAStruct
import           Ampersand.FSpec
import           Ampersand.Graphic.Graphics
import           Ampersand.Misc.HasClasses
import           Ampersand.Output.PandocAux
import           Ampersand.Output.PredLogic
import           Data.Hashable
import           Data.Typeable (typeOf)
import qualified RIO.List as L
import qualified RIO.NonEmpty as NE
import qualified RIO.Set as Set
import qualified RIO.Text as T
import           RIO.Time
import           Text.Pandoc hiding (trace,Verbosity,getVerbosity)
import           Text.Pandoc.Builder hiding (caption)
import           System.FilePath ( (</>) )
-- | Define the order of the chapters in the document.
chaptersInDoc :: (HasDocumentOpts env) => env -> [Chapter]
chaptersInDoc = view chaptersL

data CustomSection
             = XRefSharedLangRelation Relation
             | XRefSharedLangRule Rule
             | XRefSharedLangTheme (Maybe Pattern)
             | XRefDataAnalysisRule Rule
             | XRefConceptualAnalysisPattern Pattern
             | XRefConceptualAnalysisRelation Relation
             | XRefConceptualAnalysisRule Rule
             | XRefConceptualAnalysisExpression Rule

------ Symbolic referencing to a chapter/section. ---------------------------------

-- | Things that can be referenced in a document. 
class Typeable a => Xreferenceble a where
  xDefBlck :: (HasDirOutput env, HasDocumentOpts env) => env -> FSpec -> a -> Blocks
  xDefBlck _ _ a = fatal ("A "<>tshow (typeOf a)<>" cannot be labeld in <Blocks>.") --you should use xDefInln instead.
  -- ^ function that defines the target Blocks of something that can be referenced.

  xDefInln :: (HasOutputLanguage env) => env -> FSpec -> a -> Inlines
  xDefInln _ _ a = fatal ("A "<>tshow (typeOf a)<>" cannot be labeld in an <Inlines>.") --you should use xDefBlck instead.
  -- ^ function that defines the target Inlines of something that can e referenced.

  hyperLinkTo :: a -> Inlines
  -- ^ function that returns a link to something that can be referenced.
  xSafeLabel :: a -> Text -- The full string that is used as ID for referencing
  {-# MINIMAL xSafeLabel, hyperLinkTo, (xDefBlck | xDefInln) #-}

instance Xreferenceble Chapter where
  xSafeLabel a = tshow Sec<>tshow a
  hyperLinkTo = codeGen'
  xDefBlck env fSpec a = headerWith (xSafeLabel a,[],[]) 1 (chptTitle (outputLang env fSpec) a)

instance Xreferenceble Picture where
  xSafeLabel a = tshow Fig<>caption a
  hyperLinkTo = codeGen'
  xDefBlck env _ a = para $ imageWith (xSafeLabel a, [], []) (T.pack src) (xSafeLabel a)(text (caption a))
   where
    dirOutput = view dirOutputL env
    src  = dirOutput </> imagePathRelativeToDirOutput env a
instance Xreferenceble CustomSection where
  xSafeLabel a =
       (tshow . xrefPrefix . refStuff $ a)
     <> tshow (chapterOfSection x)
     <> typeOfSection x
     <> "-"
     <> (tshow . hash . nameOfThing $ x) -- Hash, to make sure there are no fancy characters. 
    where
      x = refStuff a
  hyperLinkTo = codeGen'
  xDefBlck env fSpec a = either id (fatal ("You should use xDefInln for:\n  "<>tshow (refStuff a))) (hyperTarget env fSpec a)
  xDefInln env fSpec a = either (fatal ("You should use xDefBlck for:\n  "<>tshow (refStuff a))) id (hyperTarget env fSpec a)

hyperTarget :: (HasOutputLanguage env) => env -> FSpec -> CustomSection -> Either Blocks Inlines
hyperTarget env fSpec a =
    case a of
      XRefConceptualAnalysisPattern{} -> Left . hdr $ (text.l) (NL "Thema: ",EN "Theme: ")      <> (singleQuoted . str . nameOfThing . refStuff $ a)
      XRefSharedLangTheme (Just pat) -> (Left . hdr . text . name) pat
      XRefSharedLangTheme Nothing    -> (Left . hdr . text . l) (NL "Overig",EN "Remaining")
      XRefSharedLangRelation d        -> Right $ spanWith (xSafeLabel a,[],[]) (str . showRel $ d)
                                      --   Left $ divWith (xSafeLabel a,[],[]) 
                                      --                  (   (para . str $ showRel d)
                                      --                    <>codeBlockWith 
                                      --                         ("", ["adl"],[("caption",showRel d)]) 
                                      --                         ( "Deze RELATIE moet nog verder worden uitgewerkt in de Haskell code")
                                      --                  )
      XRefSharedLangRule r            -> Right $ spanWith (xSafeLabel a,[],[]) (str . tshow . name $ r)
                                      --   Left $ divWith (xSafeLabel a,[],[])
                                      --                  (   (para . text $ name r)
                                      --                  --  <>codeBlockWith 
                                      --                  --       ("", ["adl"],[("caption",name r)]) 
                                      --                  --       ( "Deze REGEL moet nog verder worden uitgewerkt in de Haskell code")        
                                      --                    <>printMeaning (outputLang env fSpec) r
                                      --                  )
      XRefConceptualAnalysisRelation _d
            -> Right $ spanWith (xSafeLabel a,[],[])
                                (    (text.l) (NL "Relatie ",EN "Relation ")
                               --   <> (str . show . numberOf fSpec $ d)
                                )
      XRefConceptualAnalysisRule _r
            -> Right $ spanWith (xSafeLabel a,[],[])
                                (    (text.l) (NL "Regel ",EN "Rule ")
                               --   <> (str . show . numberOf fSpec $ r)
                                )
      XRefConceptualAnalysisExpression _r
            -> Right $ spanWith (xSafeLabel a,[],[])
                                (    (text.l) (NL "Regel ",EN "Rule ")
                               --   <> (str . show . numberOf fSpec $ r)
                                )
      _ ->  fatal ("hyperTarget not yet defined for "<>tshow (refStuff a))
   where
    hdr = headerWith (xSafeLabel a, [], []) 2
    -- shorthand for easy localizing    
    l :: LocalizedStr -> Text
    l = localize (outputLang env fSpec)
codeGen' :: Xreferenceble a => a -> Inlines
codeGen' a =
  cite [Citation { citationId = xSafeLabel a
                 , citationPrefix = [Space]
                 , citationSuffix = [Space]
                 , citationHash = 0
                 , citationNoteNum = 0
                 , citationMode = NormalCitation
                 }
       ] $ code (xSafeLabel a)

pandocEqnArray :: [Inlines] -> Blocks
pandocEqnArray [] = mempty
pandocEqnArray xs
 = orderedList (map para xs)

data CrossrefType = Dfn | Agr | Eq | Sec | Tbl | Fig -- Special prefixes that make pandoc-crossref work. 
instance Show CrossrefType where
  show x = case x of
            Dfn -> "lst:"
            Agr -> "agr:"
            Eq  -> "eq:"
            Sec -> "sec:"
            Tbl -> "tbl:"
            Fig -> "fig:"
pandocEquationWithLabel :: (HasOutputLanguage env) => env -> FSpec -> CustomSection -> Inlines -> Blocks
pandocEquationWithLabel env fSpec xref x =
  para (strong (xDefInln env fSpec xref) <> x)

data RefStuff =
  RefStuff { typeOfSection    :: Text
           , chapterOfSection :: Chapter
           , nameOfThing      :: Text
           , xrefPrefix       :: CrossrefType
           } deriving Show
refStuff :: CustomSection -> RefStuff
refStuff x  =
   case x of
     XRefSharedLangRelation d
       -> RefStuff { typeOfSection    = relation
                   , chapterOfSection = SharedLang
                   , nameOfThing      = showRel d
                   , xrefPrefix       = Dfn
                   }
     XRefDataAnalysisRule r
       -> RefStuff { typeOfSection    = rule
                   , chapterOfSection = DataAnalysis
                   , nameOfThing      = name r
                   , xrefPrefix       = Agr
                   }
     XRefSharedLangRule r
       -> RefStuff { typeOfSection    = rule
                   , chapterOfSection = SharedLang
                   , nameOfThing      = name r
                   , xrefPrefix       = Agr
                   }
     XRefConceptualAnalysisPattern p
       -> RefStuff { typeOfSection    = pattern'
                   , chapterOfSection = ConceptualAnalysis
                   , nameOfThing      = name p
                   , xrefPrefix       = Sec
                   }
     XRefConceptualAnalysisRelation d
       -> RefStuff { typeOfSection    = relation
                   , chapterOfSection = ConceptualAnalysis
                   , nameOfThing      = showRel d
                   , xrefPrefix       = Eq
                   }
     XRefConceptualAnalysisRule r
       -> RefStuff { typeOfSection    = rule
                   , chapterOfSection = ConceptualAnalysis
                   , nameOfThing      = name r
                   , xrefPrefix       = Eq
                   }
     XRefConceptualAnalysisExpression r
       -> RefStuff { typeOfSection    = expression
                   , chapterOfSection = ConceptualAnalysis
                   , nameOfThing      = name r
                   , xrefPrefix       = Eq
                   }
     XRefSharedLangTheme mt
       -> RefStuff { typeOfSection    = theme
                   , chapterOfSection = SharedLang
                   , nameOfThing      = maybe ":overig" name mt
                   , xrefPrefix       = Sec
                   }
  where (relation , rule  , expression , pattern' , theme) =
          ("relation","rule" ,"expression","pattern","theme")



data ThemeContent =
       Thm { themeNr ::      Int
           , patOfTheme ::   Maybe Pattern -- A theme is about either a pattern or about everything outside patterns
           , rulesOfTheme :: [Numbered RuleCont] -- The (numbered) rules of that theme
           , dclsOfTheme ::  [Numbered DeclCont] -- The (numbered) relations that are used in a rule of this theme, but not in any rule of a previous theme.
           , cptsOfTheme ::  [Numbered CptCont]  -- The (numbered) concepts that are used in a rule of this theme, but not in any rule of a previous theme.
           }
data Numbered t =
 Nr { theNr ::   Int
    , theLoad :: t
    }
instance Named t => Named (Numbered t) where
 name = name . theLoad
data RuleCont = CRul { cRul ::  Rule
                     , cRulPurps :: [Purpose]
                     , cRulMeanings :: [Meaning]
                     }
data DeclCont = CDcl { cDcl ::  Relation
                     , cDclPurps :: [Purpose]
                     , cDclMeanings :: [Meaning]
                     , cDclPairs :: AAtomPairs
                     }
data CptCont  = CCpt { cCpt ::  A_Concept
                     , cCptDefs :: [AConceptDef]
                     , cCptPurps :: [Purpose]
                     }
instance Named RuleCont where
  name = name . cRul
instance Named DeclCont where
  name = name . cDcl
instance Named CptCont where
  name = name . cCpt

-- | orderingByTheme collects materials from the fSpec to distribute over themes.
--   It ensures that all rules, relations and concepts from the context are included in the specification.
--   The principle is that every rule, relation, or concept that is defined in a pattern is documented in the corresponding theme.
--   Everything that is defined outside themes is documented in the last theme.
--   As a consequence, something that is declared in several patterns occurs in the corresponding themes and may be seen as a double occurrence.
--   However, that may be the intention of the Ampersand modeler.
--   The story: materials from the patterns are gathered in ruless, conceptss, and relationss.
--   Numbering of each item is done recursively by `numbered`, while keeping the structure intact.
--   Finally, the theme content is constructed.
orderingByTheme :: HasOutputLanguage env => env -> FSpec -> [ThemeContent]
orderingByTheme env fSpec
 = [ Thm { themeNr      = i
         , patOfTheme   = Just pat
         , rulesOfTheme = fmap rul2rulCont nrules
         , dclsOfTheme  = fmap dcl2dclCont nrelations
         , cptsOfTheme  = fmap cpt2cptCont nconcepts
         }
   | (pat, i, nrules, nrelations, nconcepts)<-L.zip5 (vpatterns fSpec) [0..] (NE.init nruless) (NE.init nrelationss) (NE.init nconceptss) ] <>
   [ Thm { themeNr      = length (vpatterns fSpec)
         , patOfTheme   = Nothing
         , rulesOfTheme = fmap rul2rulCont (NE.last nruless)
         , dclsOfTheme  = fmap dcl2dclCont (NE.last nrelationss)
         , cptsOfTheme  = fmap cpt2cptCont (NE.last nconceptss)
         } ]
   where
     nruless     :: NonEmpty [Numbered Rule]
     nconceptss  :: NonEmpty [Numbered AConceptDef]
     nrelationss :: NonEmpty [Numbered Relation]
     nruless      = transformNonEmpty (numbering 0 (map Set.toList ruless    <>[Set.toList (ctxrs aCtx)]))
     nconceptss   = transformNonEmpty (numbering 0 (               conceptss <>[ctxcdsOutPats aCtx]            ))
     nrelationss  = transformNonEmpty (numbering 0 (map Set.toList relationss<>[Set.toList (ctxds aCtx)]))
     transformNonEmpty :: [a] -> NonEmpty a
     transformNonEmpty x = case NE.nonEmpty x of Just ne -> ne; Nothing -> fatal "onbereikbare code"
     aCtx = originalContext fSpec
     ruless     :: [Rules]
     conceptss  :: [[AConceptDef]]
     relationss :: [Relations]
     (ruless, conceptss, relationss)
      = L.unzip3 [ (ptrls pat, ptcds pat, ptdcs pat) | pat<-vpatterns fSpec ]
     numbering :: Int -> [[a]] -> [[Numbered a]]
     numbering n (xs:xss) = [ Nr i x | (x,i)<-zip xs [n..]]: numbering (n+length xs) xss
     numbering _ _ = []

     rul2rulCont :: Numbered Rule -> Numbered RuleCont
     rul2rulCont (Nr n rul)
       = Nr n CRul { cRul      = rul
                   , cRulPurps = purposesOf fSpec (outputLang env fSpec) rul
                   , cRulMeanings = meanings rul
                   }
     dcl2dclCont :: Numbered Relation -> Numbered DeclCont
     dcl2dclCont (Nr n dcl)
       = Nr n CDcl { cDcl      = dcl
                   , cDclPurps = purposesOf fSpec (outputLang env fSpec) dcl
                   , cDclMeanings = meanings dcl
                   , cDclPairs = pairsInExpr fSpec (EDcD dcl)
                   }

     cpt2cptCont :: Numbered AConceptDef -> Numbered CptCont
     cpt2cptCont (Nr n cpt)
       = Nr n CCpt { cCpt      = c
                   , cCptDefs  = [cpt]
                   , cCptPurps = purposesOf fSpec (outputLang env fSpec) c
                   } where c = PlainConcept (acdcpt cpt NE.:| [])

dpRule' :: (HasDocumentOpts env) =>
            env -> FSpec -> [Rule] -> Int -> A_Concepts -> Relations
            -> ([(Inlines, [Blocks])], Int, A_Concepts, Relations)
dpRule' env fSpec = dpR
 where
   l lstr = text $ localize (outputLang env fSpec) lstr
   dpR [] n seenConcs seenRelations = ([], n, seenConcs, seenRelations)
   dpR (r:rs) n seenConcs seenRelations
     = ( ( l (NL "Regel: ",EN "Rule: ") <> (text.name) r
         , [theBlocks]
          ): dpNext
       , n'
       , seenCs
       , seenDs
       )
       where
        theBlocks :: Blocks
        theBlocks =
            purposes2Blocks env (purposesOf fSpec (outputLang env fSpec) r) -- Als eerste de uitleg van de betreffende regel..
         <> purposes2Blocks env [p | d<-Set.elems nds, p<-purposesOf fSpec (outputLang env fSpec) d]  -- Dan de uitleg van de betreffende relaties
         <> case (Set.elems . Set.map EDcD $ nds, outputLang env fSpec) of
             ([] ,_)       -> mempty
             ([d],Dutch)   -> plain ("Om dit te formaliseren is een " <> (if isFunction d then "functie"  else "relatie" ) <> " nodig:")
             ([d],English) -> plain ("In order to formalize this, a " <> (if isFunction d then "function" else "relation") <> " is introduced:")
             (_  ,Dutch)   -> plain ("Om te komen tot de formalisatie van " <> hyperLinkTo (XRefSharedLangRule r)
                                    <>  " (" <> (singleQuoted.str.name) r  <> ") "
                                    <> str (" zijn de volgende "<>count Dutch (length nds) "in deze paragraaf geformaliseerde relatie"<>" nodig."))
             (_  ,English) -> plain ("To arrive at the formalization of "   <> hyperLinkTo (XRefSharedLangRule r) <> str (", the following "<>count English (length nds) "relation"<>" are introduced."))
         <> (bulletList . map (plain . showRef) . Set.elems $ nds)
         <> (if null nds
             then case Set.elems rds of
                       []   -> mempty
                       [rd] -> plain (  l (NL "Om dit te formalizeren maken we gebruik van relatie "
                                          ,EN "We use relation ")
                                     <> showRef rd
                                     <> l (NL ".", EN " to formalize this.")
                                     )
                       _    ->    plain (  l (NL "Dit formaliseren we door gebruik te maken van de volgende relaties: "
                                             ,EN "We formalize this using relations "))
                               <> (bulletList  . map (plain . showRef) . Set.elems $ rds)
             else case Set.elems rds of
                       []   -> mempty
                       [rd] -> plain (  l (NL "Daarnaast gebruiken we relatie ", EN "Beside that, we use relation ")
                                      <> showRef rd
                                      <> l (NL " om ", EN " to formalize ")
                                      <> hyperLinkTo (XRefSharedLangRule r)
                                      <> l (NL " te formaliseren: ", EN ": ")
                                     )
                       _    -> plain (   l (NL " Om ", EN " To formalize ")
                                      <> hyperLinkTo (XRefSharedLangRule r)
                                      <> l (NL " te formaliseren, gebruiken we daarnaast ook de relaties: "
                                           ,EN " we also use relations ")
                                     ) <>
                               (bulletList  . map (plain . showRef) . Set.elems $ rds)
           )
         <> plain (if isSignal r
                   then l ( NL "Activiteiten, die door deze regel zijn gedefinieerd, zijn afgerond zodra: "
                          , EN "Activities that are defined by this rule are finished when: ")
                   else l (NL "De regel luidt: ", EN "This means: ")
                  )
         <> pandocEquationWithLabel env fSpec (XRefConceptualAnalysisExpression r) (showMath r)
         <> (if length nds<=1
             then mempty
             else plain (  l (NL "Dit komt overeen met ", EN "This corresponds to ")
                        <> hyperLinkTo (XRefSharedLangRule r)
                        <> " (" <> (singleQuoted.str.name) r  <> ")."

                        )
            )
        showRef :: Relation -> Inlines
        showRef dcl = hyperLinkTo (XRefConceptualAnalysisRelation dcl) <> "(" <> (str . showRel) dcl <> ")"

        ncs = concs r Set.\\ seenConcs            -- newly seen concepts
        cds = [(c,cd) | c<-Set.elems ncs, cd<-conceptDefs fSpec, name cd==name c]    -- ... and their definitions
        ds  = bindedRelationsIn r
        nds = ds Set.\\ seenRelations     -- newly seen relations
        rds = ds `Set.intersection` seenRelations  -- previously seen relations
        ( dpNext, n', seenCs,  seenDs ) = dpR rs (n+length cds+length nds+1) (ncs `Set.union` seenConcs) (nds `Set.union` seenRelations)

printMeaning :: HasMeaning a => Lang -> a -> Blocks
printMeaning lang = maybe mempty (printMarkup . ameaMrk) . meaning lang

printPurposes :: [Purpose] -> Blocks
printPurposes = mconcat . map (printMarkup . explMarkup)

printMarkup :: Markup -> Blocks
printMarkup = amPandoc

meaning2Blocks :: Meaning -> Blocks
meaning2Blocks
 = printMarkup . ameaMrk

purposes2Blocks :: (HasDocumentOpts env) => env -> [Purpose] -> Blocks
purposes2Blocks env ps
 = maybe mempty amPandoc (concatMarkup . map markup' $ ps)
    where   -- The reference information, if available for this purpose, is put
        markup' purp = Markup { amLang= amLang . explMarkup $ purp
                    , amPandoc= insertAfterFirstInline (ref purp) $ amPandoc . explMarkup $ purp
                    }
        ref :: Purpose -> [Inline]
        ref purp = [RawInline
                      (Text.Pandoc.Builder.Format "latex")
                      (texOnlyMarginNote
                        (T.intercalate "; " (explRefIds purp) <> "\n"))
                   | view fspecFormatL env `elem` [Fpdf, Flatex]
                        && (not . null . explRefIds) purp
                   ]
concatMarkup :: [Markup] -> Maybe Markup
concatMarkup es
 = case eqCl amLang es of
    []   -> Nothing
    [cl] -> Just Markup { amLang   = amLang (NE.head cl)
                        , amPandoc = mconcat (map amPandoc es)
                        }
    cls  -> fatal ("don't call concatMarkup with different languages and formats\n   "<>
                   T.intercalate "\n   " (map (tshow . amLang . NE.head) cls)
                  )

-- Insert an inline after the first inline in the list of blocks, if possible.
insertAfterFirstInline :: [Inline] -> Blocks -> Blocks
insertAfterFirstInline inlines =  fromList . insertAfterFirstInline' . toList
  where
    insertAfterFirstInline' (            Plain (inl:inls):pblocks)        =             Plain (inl : (inlines<>inls)) : pblocks
    insertAfterFirstInline' (            Para (inl:inls):pblocks)         =             Para (inl : (inlines<>inls)) : pblocks
    insertAfterFirstInline' (BlockQuote (Para (inl:inls):pblocks):blocks) = BlockQuote (Para (inl : (inlines<>inls)) : pblocks):blocks
    insertAfterFirstInline' blocks                                        = Plain inlines : blocks

isMissing :: Maybe Purpose -> Bool
isMissing = maybe True (not . explUserdefd)

lclForLang :: Lang -> TimeLocale
lclForLang lang = defaultTimeLocale { months =
         case lang of
           Dutch   -> [ ("januari","jan"),("februari","feb"),("maart","mrt"),("april","apr")
                      , ("mei","mei"),("juni","jun"),("juli","jul"),("augustus","aug")
                      , ("september","sep"),("oktober","okt"),("november","nov"),("december","dec")]
           English -> [ ("January","Jan"),("February","Feb"),("March","Mar"),("April","Apr")
                      , ("May","May"),("June","Jun"),("July","Jul"),("August","Aug")
                      , ("September","Sep"),("October","Oct"),("November","Nov"),("December","Dec")]
           }

plainText :: Text -> Blocks
plainText = plain . text

violation2Inlines :: (HasOutputLanguage env) => env -> FSpec -> PairView Expression -> Inlines
violation2Inlines env fSpec _ = (text.l) (NL "<meldingstekst moet hier nog worden gegenereerd>"
                                        ,EN "<violation message should be printed here>"
                                        )
  where
    l = localize (outputLang env fSpec)

-- Some helper function to cope with changes in Pandoc. In the newer versions of Pandoc,
-- tables have gotten more possibilities. For the time being, we do not use them. Maybe later.
legacyTable
      :: Inlines               -- ^ Caption
      -> [(Alignment, Double)] -- ^ Column alignments and fractional widths
      -> [Blocks]              -- ^ Headers
      -> [[Blocks]]            -- ^ Rows
      -> Blocks
legacyTable caption' cellspecs headers rows =
  table tCaption tColSpec tHead tBodies tFooter
    where
      tCaption :: Caption
      tCaption
        | null caption' = emptyCaption
        | otherwise = Caption (Just . toList $ caption') []
      tColSpec :: [ColSpec]
      tColSpec = map toColSpec cellspecs
        where toColSpec :: (Alignment, Double) -> ColSpec
              toColSpec (a, d) = (a, ColWidth d)
      tHead :: TableHead
      tHead = TableHead nullAttr (zipWith toRow (map fst cellspecs) headers)
        where toRow :: Alignment -> Blocks -> Row
              toRow a bs = Row nullAttr (map (toCell a . singleton) $ toList bs)
      toCell :: Alignment -> Blocks -> Cell
      toCell a b = Cell nullAttr a (RowSpan 1) (ColSpan 1) (toList b)
      tBodies :: [TableBody]
      tBodies = map toBodyRow rows
         where toBodyRow :: [Blocks] -> TableBody
               toBodyRow bs = TableBody nullAttr (RowHeadColumns 0) [] [Row nullAttr $ zipWith toCell (map fst cellspecs) bs]
      tFooter :: TableFoot
      tFooter = TableFoot nullAttr []

-- | This function is used in the conceptual analysis chapter as wel as the natural language chapter. To avoid
--   code duplication, it has been placed in this shared module.
printConceptDef :: (HasDocumentOpts env) =>
    env -> (LocalizedStr -> Text) -> Numbered CptCont -> Blocks
printConceptDef env l nCpt
        = case (nubByContent . cCptDefs . theLoad) nCpt of
           []   -> mempty  -- There is no definition of the concept
           [cd] -> printCDef cd Nothing
           cds  -> mconcat
                  [printCDef cd (Just $ T.snoc "." suffx)
                  |(cd,suffx) <- zip cds ['a' ..]  -- There are multiple definitions. Which one is the correct one?
                  ]
        where
         fspecFormat = view fspecFormatL env
         nubByContent = L.nubBy (\x y -> fun x == fun y) -- fixes https://github.com/AmpersandTarski/Ampersand/issues/617
           where fun = amPandoc . ameaMrk . acddef2
         printCDef :: AConceptDef -- the definition to print
                -> Maybe Text -- when multiple definitions exist of a single concept, this is to distinguish
                -> Blocks
         printCDef cDef suffx
           = definitionList
              [(   str (l (NL"Definitie " ,EN "Definition "))
                <> ( if fspecFormat `elem` [Fpdf, Flatex]
                     then (str . tshow .theNr) nCpt
                     else (str . name) cDef
                   )
                <> str (fromMaybe "" suffx) <> ":"
               , [meaning2Blocks (acddef2 cDef)]
               )
              ]


-- | This function is used in the conceptual analysis chapter as wel as the natural language chapter. To avoid
--   code duplication, it has been placed in this shared module.
printConceptPurp :: Numbered CptCont -> Blocks
printConceptPurp = printPurposes . cCptPurps . theLoad
