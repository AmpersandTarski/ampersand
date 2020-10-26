{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module           Ampersand.FSpec.ShowHS
    (ShowHS(..),ShowHSName(..),fSpec2Haskell,haskellIdentifier)
where
import           Ampersand.Basics
import           Ampersand.ADL1
import           Ampersand.Core.ShowAStruct  (AStruct(..))  -- for traceability, we generate comments in the Haskell code.
import           Ampersand.FSpec.FSpec
import           Ampersand.Misc.HasClasses
import           RIO.Char                  (isAlphaNum)
import           Data.Hashable
import qualified RIO.List as L
import qualified RIO.NonEmpty as NE
import qualified RIO.Set as Set
import qualified RIO.Text as T
import           RIO.Time
import           Text.Pandoc hiding (Meta)

fSpec2Haskell :: (HasFSpecGenOpts env) =>
       env -> UTCTime -> FSpec -> Text
fSpec2Haskell env now fSpec
    = T.intercalate "\n" 
        [ "{-# OPTIONS_GHC -Wall #-}"
        , "{-Generated code by "<>ampersandVersionStr<>" at "<>tshow now<>"-}"
        , "module Main"
        , "where"
        , ""
        , "import Ampersand"
        , "import Text.Pandoc hiding (Meta)"
        , ""
        , "main :: IO ()"
        , "main = do env <- getOptions"
        , "          say (showHS env \"\\n  \" fSpec_"<>T.pack (baseName env)<>")"
        , ""
        , "fSpec_"<>T.pack (baseName env)<>" :: FSpec"
        , "fSpec_"<>T.pack (baseName env)<>" =\n  "<>showHS env "\n  " fSpec
        ]
wrap :: Text->Text->(Text->a->Text)->[a]->Text
wrap initStr indent f xs
 = initStr<>
   case xs of
     []  -> "[]"
     [x] -> "[ "<>f (indent<>"  ") x<>" ]"
     _   -> "[ "<>T.intercalate (indent<>", ") [f (indent<>"  ") x | x<-xs]<>indent<>"]"

class ShowHSName a where
 showHSName :: a -> Text

class ShowHS a where
 showHS :: (HasFSpecGenOpts env) => env -> Text -> a -> Text

instance ShowHSName a => ShowHSName [a] where
 showHSName xs = "["<>T.intercalate "," (map showHSName xs)<>"]"

instance ShowHS a => ShowHS [a] where
 showHS env indent = wrap "" (indent<>" ") (showHS env)

instance ShowHS a => ShowHS (NE.NonEmpty a) where
 showHS env indent = wrap "" (indent<>" ") (showHS env) . NE.toList

instance ShowHSName a => ShowHSName (Maybe a) where
 showHSName Nothing  = "Nothing"
 showHSName (Just x) = showHSName x

instance ShowHS a => ShowHS (Maybe a) where
 showHS _ _ Nothing  = "Nothing"
 showHS env indent (Just x) = "Just (" <> showHS env indent x <> ")"

instance (ShowHSName a , ShowHSName b) => ShowHSName (a,b) where
 showHSName (a,b) = "( "<>showHSName a<>" , "<>showHSName b<>" )"
-- | The following is used to showHS env for signs: (Concept, Concept)
--   instance (ShowHS a , ShowHS b) => ShowHS (a,b) where
--    showHS env indent (a,b) = "("<>showHS env (indent<>" ") a<>","<>showHS env (indent<>" ") b<>")"

instance ShowHSName PlugSQL where
 showHSName plug = haskellIdentifier ("plug_"<>name plug)

instance ShowHS PlugSQL where
 showHS env indent plug
   = case plug of
       TblSQL{} -> T.intercalate indent
                   ["let " <> T.intercalate (indent<>"    ")
                                          [showHSName f<>indent<>"     = "<>showHS env (indent<>"       ") f 
                                          | f<-NE.toList $ plugAttributes plug] <>indent<>"in"
                   ,"TblSQL { sqlname    = " <> (tshow.name) plug
                   ,"       , attributes = ["<>T.intercalate ", " (map showHSName (attributes plug))<>"]"
                   ,"       , cLkpTbl    = [ "<>T.intercalate (indent<>"                      , ") ["("<>showHSName c<>", "<>showHSName cn<>")" | (c,cn)<-cLkpTbl plug] <> "]"
                   ,"       , dLkpTbl    = [ "<>T.intercalate (indent<>"                      , ") 
                                                ( map (showHS env (indent<>"                        ")) . dLkpTbl $ plug) 
                                              <> "]"
                   ,"       }"
                   ]
       BinSQL{} -> T.intercalate indent
                   ["let " <> T.intercalate (indent<>"    ")
                                          [showHSName f<>indent<>"     = "<>showHS env (indent<>"       ") f 
                                          | f<-NE.toList $ plugAttributes plug] <>indent<>"in"
                   ,"BinSQL { sqlname = " <> (tshow.name) plug
                   ,"       , cLkpTbl = [ "<>T.intercalate (indent<>"                   , ") ["("<>showHSName c<>", "<>showHSName cn<>")" | (c,cn)<-cLkpTbl plug] <> "]"
                   ,"       , dLkpTbl    = [ "<>T.intercalate (indent<>"                      , ") 
                                                ( map (showHS env (indent<>"                        ")) . dLkpTbl $ plug) 
                                              <> "]"
                   ,"       }"
                   ]

instance ShowHS RelStore where
 showHS _ indent store
   = T.intercalate indent
       [  "Relstore { rsDcl           = " <> showHSName (rsDcl store)
       ,  "         , rsStoredFlipped = " <> tshow (rsStoredFlipped store)
       ,  "         , rsSrcAtt        = " <> showHSName (rsSrcAtt store)
       ,  "         , rsTrgAtt        = " <> showHSName (rsTrgAtt store)
       ,  "         }"
       ]

instance ShowHSName SqlAttribute where
 showHSName sqAtt = haskellIdentifier ("sqlAtt_"<>attName sqAtt)

instance ShowHS SqlAttribute where
 showHS env indent sqAtt
   = T.intercalate indentA
       [  "Att { attName    = " <> tshow (attName sqAtt)
       ,      ", attExpr    = " <> showHS env indentB (attExpr sqAtt)
       ,      ", attType    = " <> showHS env "" (attType sqAtt)
       ,      ", attUse     = " <> showHS env "" (attUse sqAtt)
       ,      ", attNull    = " <> tshow (attNull sqAtt)
       ,      ", attDBNull  = " <> tshow (attDBNull sqAtt)
       ,      ", attUniq    = " <> tshow (attUniq sqAtt)
       ,      ", attFlipped = " <> tshow (attFlipped sqAtt)
       ,      "}"
       ] where indentA = indent <>"    "         -- adding the width of "Att "
               indentB = indentA<>"            " -- adding the width of ", attExpr = "

instance ShowHS SqlAttributeUsage where
 showHS _ _ (PrimaryKey aCpt) = "PrimaryKey "<>showHSName aCpt
 showHS _ _ (ForeignKey aCpt) = "ForeignKey "<>showHSName aCpt
 showHS _ _ PlainAttr         = "PlainAttr "

instance ShowHS TType where
 showHS _ indent tt = indent <> tshow tt

instance ShowHSName Quad where
 showHSName q
   = haskellIdentifier ("quad_"<>(showHSName.qDcl) q<>"_"<>(name.qRule) q)

instance ShowHS Quad where
 showHS _ indent q
   = T.intercalate indent
            [ "Quad{ qDcl     = " <> showHSName (qDcl q)
            , "    , qRule    = " <> showHSName (qRule q)
            , wrap "    , qConjuncts = " newindent (const showHSName) (NE.toList $ qConjuncts q)
            , "    }"
            ]
    where
      newindent = indent <> "                 "

instance ShowHS DnfClause where
 showHS env indent dnf
   = T.intercalate indent
       [ wrap "Dnf " (indent<>"    ") (\_->showHS env (indent<>"      ")) (antcs dnf)
       , wrap "    " (indent<>"    ") (\_->showHS env (indent<>"      ")) (conss dnf)
       ]

instance ShowHSName Conjunct where
 showHSName x = haskellIdentifier (rc_id x)

instance ShowHS Conjunct where
 showHS env indent x
   = T.intercalate (indent <>"    ")
       [   "Cjct{ rc_id         = " <> tshow (rc_id x)
       ,       ", rc_orgRules   = " <> "[ "<>T.intercalate ", " (NE.toList . fmap showHSName $ rc_orgRules x)<>"]"
       ,       ", rc_conjunct   = " <> showHS env indentA (rc_conjunct x)
       , wrap  ", rc_dnfClauses = " indentA (\_->showHS env (indentA<>"  ")) (rc_dnfClauses x)
       ,       "}"
       ]
     where indentA = indent <>"                    "

instance ShowHSName FSpec where
 showHSName fSpec = haskellIdentifier ("fSpc_"<>name fSpec)

instance ShowHS FSpec where
 showHS env indent fSpec
  = T.intercalate (indent <>"     ")
        [ "FSpec{ fsName        = " <> tshow (name fSpec)
        , wrap ", fspos         = " indentA (showHS env) (fspos fSpec)
        , wrap ", plugInfos     = " indentA (\_->showHS env (indentA<>"  ")) (plugInfos  fSpec)
        ,      ", interfaceS    = interfaceS'"
        ,      ", interfaceG    = interfaceG'"
        ,      ", fRoleRuls     = " <>showHS env indentA (fRoleRuls fSpec)
        , wrap ", fRoles        = " indentA (showHS env)    [rol | (rol,_) <- fRoles fSpec]
        , wrap ", vrules        = " indentA (const showHSName) (Set.elems $ vrules fSpec)
        , wrap ", grules        = " indentA (const showHSName) (Set.elems $ grules fSpec)
        , wrap ", invariants    = " indentA (const showHSName) (Set.elems $ invariants fSpec)
        , wrap ", fallRules     = " indentA (const showHSName) (Set.elems $ fallRules fSpec)
        , wrap ", allUsedDecls  = " indentA (const showHSName) (Set.elems $ allUsedDecls fSpec)
        , wrap ", vrels         = " indentA (const showHSName) (Set.elems $ vrels fSpec)
        , wrap ", allConcepts   = " indentA (const showHSName) (Set.elems $ allConcepts fSpec)
        , wrap ", vIndices      = " indentA (const showHSName) (vIndices fSpec)
        , wrap ", vviews        = " indentA (const showHSName) (vviews fSpec)
        , wrap ", vgens         = " indentA (showHS env)    (vgens fSpec)
        , wrap ", fsisa         = " indentA (const showHSName) (fsisa fSpec)
        , wrap ", allConjuncts  = " indentA (const showHSName) (allConjuncts fSpec)
        , wrap ", vquads        = " indentA (const showHSName) (vquads fSpec)
        , wrap ", vpatterns     = " indentA (const showHSName) (vpatterns fSpec)
        , wrap ", conceptDefs   = " indentA (showHS env)    (conceptDefs fSpec)
        , wrap ", fSexpls       = " indentA (showHS env)    (fSexpls fSpec)
        ,      ", metas         = allMetas"
        , wrap ", allViolations = " indentA showViolatedRule (allViolations fSpec)
        , wrap ", allExprs      = " indentA (showHS env)    (Set.toList $ allExprs fSpec)
        , "}"
        ] <>
    indent<>"where"<>
     "\n -- ***Interfaces Specified in Ampersand script***: "<>
    indent<>" interfaceS' = "<>(if null (interfaceS fSpec) then "[]" else
                              "[ "<>T.intercalate (indentB<>"  , ") (map showHSName (interfaceS fSpec))<>indentB<>"  ]")<>
     "\n -- ***Activities Generated by the Ampersand compiler ***: " <>
    indent<>" interfaceG' = "<>(if null (interfaceG fSpec) then "[]" else
                              "[ "<>T.intercalate (indentB<>", ") (map showHSName (interfaceG fSpec))<>indentB<>"]")<>
    indent<>" allMetas = "<>(if null (metas fSpec) then "[]" else
                              "[ "<>T.intercalate (indentB<>", ") (map (showHS env (indent <> "         ")) (metas fSpec))<>indentB<>"]") <>

-- WHY?  staan hier verschillende lijstjes met interfaces?
-- BECAUSE! Een Ampersand engineer besteedt veel tijd om vanuit een kennismodel (lees: een graaf met concepten en relaties)
--          alle interfaces met de hand te verzinnen.
--          Je kunt natuurlijk ook een interfaces-generator aan het werk zetten, die een aantal interfaces klaarzet bij wijze
--          van steiger (scaffold). Dat bespaart een hoop werk. De functie interfaceG is zo'n generator.
--          Door de gegenereerde interfaces af te drukken, kun je dus heel snel Ampersand sourcecode maken met correct-vertaalbare interfaces.
--          Heb je eenmaal een goed werkend pakket interfaces, dan wil je wellicht alleen de door jezelf gespecificeerde interfaces
--          gebruiken. Dat gebeurt in interfaceS.

    (if null  (interfaceS fSpec) then ""  else
     "\n -- *** User defined interfaces (total: "<>(tshow.length.interfaceS) fSpec<>" interfaces) ***: "<>
     T.concat [indent<>" "<>showHSName s<>indent<>"  = "<>showHS env (indent<>"    ") s | s<-interfaceS fSpec]<>"\n"
    )<>
    (if null (interfaceG fSpec ) then "" else
     "\n -- *** Generated interfaces (total: "<>(tshow.length.interfaceG) fSpec<>" interfaces) ***: "<>
     T.concat [indent<>" "<>showHSName x<>indent<>"  = "<>showHS env (indent<>"    ") x |x<-interfaceG fSpec ]<>"\n"
    )<>
    (let ds = vrels fSpec `Set.union` allUsedDecls fSpec `Set.union` (Set.fromList . map qDcl . vquads) fSpec in
     if null ds then "" else
     "\n -- *** Declared relations (in total: "<>(tshow.length) ds<>" relations) ***: "<>
     T.concat [indent<>" "<>showHSName x<>indent<>"  = "<>showHS env (indent<>"    ") x |x<-Set.elems ds]<>"\n"
    ) <>
    (if null (vIndices fSpec)     then "" else
     "\n -- *** Indices (total: "<>(tshow.length.vIndices) fSpec<>" indices) ***: "<>
     T.concat [indent<>" "<>showHSName x<>indent<>"  = "<>showHS env (indent<>"    ") x |x<-vIndices fSpec]<>"\n"
    ) <>
    (if null (vviews fSpec)     then "" else
     "\n -- *** Views (total: "<>(tshow.length.vviews) fSpec<>" views) ***: "<>
     T.concat [indent<>" "<>showHSName x<>indent<>"  = "<>showHS env (indent<>"    ") x |x<-vviews fSpec]<>"\n"
    ) <>
    (if null (vrules   fSpec ) then "" else
     "\n -- *** User defined rules (total: "<>(tshow.length.vrules) fSpec<>" rules) ***: "<>
     T.concat [indent<>" "<>showHSName x<>indent<>"  = "<>showHS env (indent<>"    ") x |x<-Set.elems $ vrules     fSpec ]<>"\n"
    )<>
    (if null (grules   fSpec ) then "" else
     "\n -- *** Generated rules (total: "<>(tshow.length.grules) fSpec<>" rules) ***: "<>
     T.concat [indent<>" "<>showHSName x<>indent<>"  = "<>showHS env (indent<>"    ") x |x<-Set.elems $ grules     fSpec ]<>"\n"
    )<>
    (if null (allConjuncts fSpec ) then "" else
     "\n -- *** Conjuncts (total: "<>(tshow.length.allConjuncts) fSpec<>" conjuncts) ***: "<>
     T.concat [indent<>" "<>showHSName x<>indent<>"  = "<>showHS env (indent<>"    ") x |x<-allConjuncts     fSpec ]<>"\n"
    )<>
    (if null (vquads fSpec ) then "" else
     "\n -- *** Quads (total: "<>(tshow.length.vquads) fSpec<>" quads) ***: "<>
     T.concat [indent<>" "<>showHSName x<>indent<>"  = "<>showHS env (indent<>"    ") x |x<-vquads     fSpec ]<>"\n"
    )<>
    (if null (plugInfos fSpec ) then "" else
     "\n -- *** PlugInfos (total: "<>(tshow.length.plugInfos) fSpec<>" plugInfos) ***: "<>
     T.concat [indent<>" "<>showHSName p<>indent<>"  = "<>showHS env (indent<>"    ") p |InternalPlug p<-L.sortBy (compare `on` name) (plugInfos fSpec) ]<>"\n"
    )<>
    (if null (vpatterns fSpec) then "" else
     "\n -- *** Patterns (total: "<>(tshow.length.vpatterns) fSpec<>" patterns) ***: "<>
     T.concat [indent<>" "<>showHSName x<>indent<>"  = "<>showHS env (indent<>"    ") x |x<-vpatterns fSpec]<>"\n"
    )<>
--       (if null (conceptDefs fSpec) then "" else
--        "\n -- *** ConceptDefs (total: "<>(tshow.length.conceptDefs) fSpec<>" conceptDefs) ***: "<>
--        concat [indent<>" "<>showHSName cd<>indent<>"  = "<>showHS env (indent<>"    ") cd | c<-concs fSpec, cd<-concDefs fSpec c]<>"\n"
--       )<>
    (if null (allConcepts fSpec) then "" else
     "\n -- *** Concepts (total: "<>(tshow.length.allConcepts) fSpec<>" concepts) ***: "<>
     T.concat [indent<>" "<>showHSName x<>indent<>"  = "<>showHS env (indent<>"    ") x
          <> indent<>"    "<>showAtomsOfConcept x |x<-L.sortBy (comparing showHSName) (Set.toList $ allConcepts fSpec)]<>"\n"
    )
        where indentA = indent <>"                       "
              indentB = indent <>"             "
              showAtomsOfConcept c =
                           "-- atoms: [ "<> T.intercalate indentC strs<>"]"
                  where
                    strs = map showVal . L.sort . Set.elems . atomsInCptIncludingSmaller fSpec $ c
                      where showVal val= "`"<>showValADL val<>"`" 
                    indentC = if sum (map T.length strs) > 300
                              then indent <> "    --        , "
                              else ", "
              showViolatedRule :: Text -> (Rule,AAtomPairs) -> Text
              showViolatedRule indent' (r,ps)
                 = T.intercalate indent'
                     [        " ( "<>showHSName r<>" -- This is "<>(if isSignal r then "a process rule." else "an invariant")<>
                      indent'<>" , "<> wrap "" (indent'<>"   ") 
                                               (let showPair _ p = "( "<> (tshow.showValADL.apLeft) p<>", "<>(tshow.showValADL.apRight) p<>")"
                                                in showPair
                                               ) (Set.elems ps)<>
                      indent'<>" )"
                     ]

instance ShowHS Meta where
 showHS f i (Meta pos' nm val) = "Meta ("<>showHS f i pos' <> ") " <> " " <> tshow nm <> " " <> tshow val

instance ShowHSName PlugInfo where
 showHSName (InternalPlug p) = haskellIdentifier ("ipl_"<>name p)-- TODO
 
instance ShowHS PlugInfo where
 showHS _ _ (InternalPlug p)
  = "InternalPlug "<>showHSName p

instance ShowHS Role where
 showHS _ ind r = ind<>
                  (case r of
                     Role str -> "Role "<>tshow str
                     Service str -> "Service "<>tshow str
                  ) 
 
instance ShowHS P_RoleRule where
 showHS env ind rs
  = "Maintain "<>tshow (mRoles rs)<>" "<>tshow (mRules rs)<>" "<>showHS env (ind<>"    ") (origin rs)
instance ShowHS (Role,Rule) where
 showHS _ _ (rol,rul)
  = "("<>tshow rol<>", "<>showHSName rul<>")"

instance ShowHSName Pattern where
 showHSName pat = haskellIdentifier ("pat_"<>name pat)

instance ShowHS Pattern where
 showHS env indent pat
  = T.intercalate indentA
     [ "A_Pat { ptnm  = "<>tshow (name pat)
     , ", ptpos = "<>showHS env "" (ptpos pat)
     , ", ptend = "<>showHS env "" (ptend pat)
     , ", ptrls = [" <>T.intercalate ", " [showHSName r | r<-Set.elems $ ptrls pat] <> T.concat [" {- no rules -} "        | Set.null (ptrls pat)] <>"]"
     , wrap ", ptgns = " indentB (showHS env) (ptgns pat)
     , ", ptdcs = [ " <>T.intercalate (indentB<>", ") [showHSName d | d<-Set.elems $ ptdcs pat] <> T.concat [" {- no relations -} " | null (ptdcs pat)] <>indentB<>"]"
     , wrap ", ptups = " indentB (showHS env) (ptups pat)
     , wrap ", ptids = " indentB (showHS env) (ptids pat)
     , wrap ", ptvds = " indentB (showHS env) (ptvds pat)
     , wrap ", ptxps = " indentB (showHS env) (ptxps pat)
     , "}"
     ] where indentA = indent <>"      "     -- adding the width of "A_Pat "
             indentB = indentA<>"          " -- adding the width of ", ptrls = "

instance ShowHS PPurpose where
 showHS env _ expl =
    "PRef2 ("<>showHS env "" (origin     expl)<>") "<>
          "("<>showHS env "" (pexObj     expl)<>") "<>
          "("<>showHS env "" (pexMarkup  expl)<>") "
             <>tshow (T.intercalate ";" (pexRefIDs expl))<>" "

instance ShowHS PRef2Obj where
 showHS _ _ peObj
  = case peObj of
         PRef2ConceptDef str                    -> "PRef2ConceptDef " <>tshow str
         PRef2Relation (PNamedRel _ nm mSgn) -> "PRef2Relation "<>tshow nm<>maybe "" tshow mSgn
         PRef2Rule str                          -> "PRef2Rule "       <>tshow str
         PRef2IdentityDef str                   -> "PRef2IdentityDef "<>tshow str
         PRef2ViewDef str                       -> "PRef2ViewDef "    <>tshow str
         PRef2Pattern str                       -> "PRef2Pattern "    <>tshow str
         PRef2Interface str                     -> "PRef2Interface "  <>tshow str
         PRef2Context str                       -> "PRef2Context "    <>tshow str

instance ShowHS Purpose where
 showHS env _ expla =
    "Expl "<>"("<>showHS env "" (explPos expla)<>") "
           <>"("<>showHS env "" (explObj expla)<>") "
                <>showHS env "" (explMarkup  expla)<>" "
                <>tshow (explUserdefd expla)<>" "
                <>tshow (explRefIds expla)<>" "

instance ShowHS ExplObj where
 showHS env i peObj = case peObj of
          ExplConcept cpt    -> "ExplConcept "    <>showHS env i cpt
          ExplRelation rel   -> "ExplRelation "   <>showHSName rel
          ExplRule str       -> "ExplRule "       <>tshow str
          ExplIdentityDef str-> "ExplIdentityDef "<>tshow str
          ExplViewDef str    -> "ExplViewDef "    <>tshow str
          ExplPattern str    -> "ExplPattern "    <>tshow str
          ExplInterface str  -> "ExplInterface "  <>tshow str
          ExplContext str    -> "ExplContext "    <>tshow str

instance ShowHS P_Markup where
 showHS _ indent m
   = T.intercalate indent
     ["P_Markup{ mLang   = "<> tshow (mLang m)
     ,"        , mFormat = "<> tshow (mFormat m)
     ,"        , mString = "<> tshow (mString m)
     ,"        }"
     ]

instance ShowHS Markup where
 showHS _ indent m
   = T.intercalate indent
     ["Markup{ amLang   = "<> tshow (amLang m)
     ,"        , amPandoc = "<> tshow (amPandoc m)
     ,"        }"
     ]

instance ShowHS (PairView Expression) where
  showHS env indent (PairView pvs) = "PairView "<>showHS env indent (NE.toList pvs)

instance ShowHS (PairViewSegment Expression) where
  showHS _     _ (PairViewText _ txt) = "PairViewText "<>tshow txt
  showHS env _ (PairViewExp _ srcOrTgt e) = "PairViewExp "<>tshow srcOrTgt<>" ("<>showHS env "" e<>")"

instance ShowHSName Rule where
 showHSName r = haskellIdentifier ("rule_"<> rrnm r)

instance ShowHS Rule where
 showHS env indent r
   = T.intercalate indent
     ["Ru{ rrnm   = " <> tshow (rrnm   r)
     ,"  , formalExpression  = -- " <> showA (formalExpression  r) <> indent<>"             " <> showHS env (indent<>"             ") (formalExpression  r)
     ,"  , rrfps  = " <> showHS env "" (rrfps  r)
     ,"  , rrmean = " <> showHS env (indent<>"             ") (rrmean r)
     ,"  , rrmsg  = " <> showHS env "" (rrmsg  r)
     ,"  , rrviol = " <> showHS env "" (rrviol r)
     ,"  , rrdcl  = " <> case rrdcl r of
                           Just (p,d) -> "Just ("<>showHSName p<>", "<>showHSName d<>" )"
                           Nothing    -> "Nothing"
     ,"  , rrpat  = " <> tshow (rrpat  r)
     ,"  , r_usr  = " <> tshow (r_usr  r)
     ,"  , isSignal = " <> tshow (isSignal  r)
     ,"  }"
     ]

instance ShowHS Meaning where
  showHS env indent (Meaning x) = "Meaning " <> showHS env (indent<>"        ") x

instance ShowHSName IdentityDef where
 showHSName identity = haskellIdentifier ("identity_"<>name identity)

instance ShowHS IdentityDef where
 showHS env indent identity
  = "Id ("<>showHS env "" (idPos identity)<>") "<>tshow (idLbl identity)<>" ("<>showHSName (idCpt identity)<>")"
    <>indent<>"  [ "<>T.intercalate (indent<>"  , ") (NE.toList . fmap (showHS env indent) $ identityAts identity)<>indent<>"  ]"

instance ShowHS IdentitySegment where
 showHS env indent (IdentityExp objDef) = "IdentityExp ("<> showHS env indent objDef <> ")"

instance ShowHSName ViewDef where
 showHSName vd = haskellIdentifier ("vdef_"<>name vd)

instance ShowHS ViewDef where
 showHS env indent vd
  = "ViewDef ("<>showHS env "" (vdpos vd)<>") "<>tshow (name vd)<>" "<>showHSName (vdcpt vd)
    <>indent<>"  [ "<>T.intercalate (indent<>"  , ") (showHS env indent <$> vdats vd)<>indent<>"  ]"

instance ShowHS ViewSegment where
  showHS env indent vs =
    "ViewSegment "<>showHS env indent (origin vs) <> " "
                  <>" "<>tshow (vsmlabel vs)
                  <>" "<>tshow (vsmSeqNr vs)<>" "
                  <>showHS env indent (vsmLoad vs)
-- showHSName vd = haskellIdentifier ("vdef_"<>name vd)

instance ShowHS ViewSegmentPayLoad where
 showHS _     _     (ViewText str)  = "ViewText "<>tshow str
 showHS env indent (ViewExp  expr) = showHS env (indent<>"            ") expr

instance ShowHS Population where
 showHS _ indent pop
  = case pop of
      ARelPopu{} -> "ARelPopu { popdcl = "<>showHSName (popdcl pop)
--TODOFIX
--          <>indent<>"         , popps  = [ "<>T.intercalate
--           (indent<>"                    , ") (map show (popps pop))
          <>indent<>"                    ]"
          <>indent<>"         }"
      ACptPopu{} -> "ACptPopu { popcpt = "<>showHSName (popcpt pop)
--TODOFIX
--          <>indent<>"         , popas  = [ "<>T.intercalate
--           (indent<>"                    , ") (map show (popas pop))
          <>indent<>"                    ]"
          <>indent<>"         }"

instance ShowHSName ObjectDef where
 showHSName obj = haskellIdentifier ("oDef_"<>name obj)

instance ShowHS ObjectDef where
 showHS env indent x
  = T.intercalate indent
        ["ObjectDef { objnm    = " <> tshow(name x)
        ,"       , objpos   = " <> showHS env "" (origin x)
        ,"       , objExpression   = " <> showHS env (indent<>"                ") (objExpression x)
        ,"       , objcrud  = " <> showHS env (indent<>"                ") (objcrud x)
        ,"       , objmView = " <> tshow(objmView x)
        ,"       , objmsub  = " <> showHS env (indent<>"                ") (objmsub x)
        ,"       }"
        ]
instance ShowHS BoxTxt where
 showHS env indent x
  = T.intercalate indent
        ["BoxTxt { objnm    = " <> tshow(name x)
        ,"       , objpos   = " <> showHS env "" (origin x)
        ,"       , objtxt   = " <> tshow(objtxt x)
        ,"       }"
        ]
instance ShowHS Cruds where
 showHS env indent x 
  = T.intercalate indent
      ["Cruds { crudOrig = "<> showHS env "" (crudOrig x)
      ,"      , crudC    = "<> tshow (crudC x)
      ,"      , crudR    = "<> tshow (crudR x)
      ,"      , crudU    = "<> tshow (crudU x)
      ,"      , crudD    = "<> tshow (crudD x)
      ,"      }"
      ]  
instance ShowHSName Interface where
 showHSName obj = haskellIdentifier ("ifc_"<>name obj)

instance ShowHS Interface where
 showHS env indent ifc
  = T.intercalate indent
        [ "Ifc { ifcname   = " <> tshow(ifcname ifc)
        , "    , ifcRoles  = " <> tshow(ifcRoles ifc)
        , "    , ifcObj"<>indent<>"       = " <> showHS env (indent<>"         ") (ifcObj ifc)
        , wrap "    , ifcControls = " (indent<>"                  ") (const showHSName) (ifcControls ifc)
        , "    , ifcPos    = " <> showHS env "" (ifcPos ifc)
        , "    , ifcPrp    = " <> tshow(ifcPrp ifc)
        ]<>indent<>"    }"
instance ShowHS BoxItem where
 showHS env indent obj =
   case obj of
     (BxExpr e) -> "BxExpr ("<>showHS env indent e<>")"
     (BxTxt t) -> "BxTxt ("<>showHS env indent t<>")"
 
instance ShowHS SubInterface where
 showHS _     _     (InterfaceRef _ isLink n) = "InterfaceRef "<>tshow isLink <>" "<>tshow n
 showHS env indent (Box _ x cl objs) = "Box ("<>showHS env indent x<>") ("<>tshow cl<>")"<>indent<>"     ("<>showHS env (indent<>"     ") objs<>")"

instance ShowHS Expression where
 showHS env indent (EEqu (l,r)) = "EEqu ("<>showHS env (indent<>"      ") l<>indent<>"     ,"<>showHS env (indent<>"      ") r<>indent<>"     )"
 showHS env indent (EInc (l,r)) = "EInc ("<>showHS env (indent<>"      ") l<>indent<>"     ,"<>showHS env (indent<>"      ") r<>indent<>"     )"
 showHS env indent (EIsc (l,r)) = "EIsc ("<>showHS env (indent<>"      ") l<>indent<>"     ,"<>showHS env (indent<>"      ") r<>indent<>"     )"
 showHS env indent (EUni (l,r)) = "EUni ("<>showHS env (indent<>"      ") l<>indent<>"     ,"<>showHS env (indent<>"      ") r<>indent<>"     )"
 showHS env indent (EDif (l,r)) = "EDif ("<>showHS env (indent<>"      ") l<>indent<>"     ,"<>showHS env (indent<>"      ") r<>indent<>"     )"
 showHS env indent (ELrs (l,r)) = "ELrs ("<>showHS env (indent<>"      ") l<>indent<>"     ,"<>showHS env (indent<>"      ") r<>indent<>"     )"
 showHS env indent (ERrs (l,r)) = "ERrs ("<>showHS env (indent<>"      ") l<>indent<>"     ,"<>showHS env (indent<>"      ") r<>indent<>"     )"
 showHS env indent (EDia (l,r)) = "EDia ("<>showHS env (indent<>"      ") l<>indent<>"     ,"<>showHS env (indent<>"      ") r<>indent<>"     )"
 showHS env indent (ECps (l,r)) = "ECps ("<>showHS env (indent<>"      ") l<>indent<>"     ,"<>showHS env (indent<>"      ") r<>indent<>"     )"
 showHS env indent (ERad (l,r)) = "ERad ("<>showHS env (indent<>"      ") l<>indent<>"     ,"<>showHS env (indent<>"      ") r<>indent<>"     )"
 showHS env indent (EPrd (l,r)) = "EPrd ("<>showHS env (indent<>"      ") l<>indent<>"     ,"<>showHS env (indent<>"      ") r<>indent<>"     )"
 showHS env indent (EKl0 e    ) = "EKl0 ("<>showHS env (indent<>"      ") e<>")"
 showHS env indent (EKl1 e    ) = "EKl1 ("<>showHS env (indent<>"      ") e<>")"
 showHS env indent (EFlp e    ) = "EFlp ("<>showHS env (indent<>"      ") e<>")"
 showHS env indent (ECpl e    ) = "ECpl ("<>showHS env (indent<>"      ") e<>")"
 showHS env indent (EBrk e    ) = "EBrk ("<>showHS env (indent<>"      ") e<>")"
 showHS _    _      (EDcD dcl  ) = "EDcD "<>showHSName dcl
 showHS _    _      (EDcI c    ) = "EDcI "<>showHSName c
 showHS env _      (EEps i sgn) = "EEps ("<>showHS env "" i<>") ("<>showHS env "" sgn<>")"
 showHS env _      (EDcV sgn  ) = "EDcV ("<>showHS env "" sgn<>")"
 showHS _    _      (EMp1 a c  ) = "EMp1 " <>tshow a<>" "<>showHSName c

instance ShowHS Signature where
 showHS _ _ sgn = "Sign "<>showHSName (source sgn)<>" "<>showHSName (target sgn)

instance ShowHS AClassify where
 showHS _ _ gen =
   case gen of
     Isa{} -> "Isa "<>showHSName (genspc gen)<>" "<>showHSName (gengen gen)<>" "
     IsE{} -> "IsE "<>showHSName (genspc gen)<>" ["<>T.intercalate ", " (NE.toList . fmap showHSName $ genrhs gen)<>"] "

instance ShowHSName Relation where
 showHSName d | decusr d  = haskellIdentifier ("rel_"<>name d<>"_"<>name (source d)<>"_"<>name (target d)) -- user defined relations
              | otherwise = haskellIdentifier ("vio_"<>name d<>"_"<>name (source d)<>"_"<>name (target d)) -- relations generated per rule

instance ShowHS Relation where
 showHS env indent d
    = T.intercalate indent
                     ["Relation { decnm   = " <> tshow (decnm d)
                     ,"         , decsgn  = " <> showHS env "" (sign d)
                     ,"         , decprps = " <> showL(map (showHS env "") (Set.elems $ decprps d))
                     ,"         , decprps_calc = " <> case decprps_calc d of
                                                 Nothing -> "Nothing"
                                                 Just ps -> "Just "<>showL(map (showHS env "") (Set.elems ps))
                     ,"         , decprL  = " <> tshow (decprL d)
                     ,"         , decprM  = " <> tshow (decprM d)
                     ,"         , decprR  = " <> tshow (decprR d)
                     ,"         , decMean = " <> tshow (decMean d)
                     ,"         , decfpos = " <> showHS env "" (decfpos d)
                     ,"         , decusr  = " <> tshow (decusr d)
                     ,"         , decpat  = " <> tshow (decpat d)
                     ]<>"}"

--   instance ShowHSName ConceptDef where
--    showHSName cd = haskellIdentifier ("cDef_"<>cdcpt cd)

instance ShowHS ConceptDef where
 showHS env _ cd
  = " Cd ("<>showHS env "" (origin cd)<>") "<>tshow (cdcpt cd)<>" "<>tshow (cddef cd)<>" "<>tshow (cdref cd)<>" "<>tshow (cdfrom cd)--
instance ShowHSName A_Concept where
 showHSName ONE = haskellIdentifier "cptOne"
 showHSName c = haskellIdentifier ("cpt_"<>name c)
instance ShowHS A_Concept where
 showHS _ _ c = case c of
                    PlainConcept{} -> "PlainConcept "<>tshow (name c)
                    ONE -> "ONE"

instance ShowHSName Prop where
 showHSName Uni = "Uni"
 showHSName Inj = "Inj"
 showHSName Sur = "Sur"
 showHSName Tot = "Tot"
 showHSName Sym = "Sym"
 showHSName Asy = "Asy"
 showHSName Trn = "Trn"
 showHSName Rfx = "Rfx"
 showHSName Irf = "Irf"
 showHSName Prop = "Prop"

instance ShowHS Prop where
 showHS _ _ = showHSName

instance ShowHS FilePos where
 showHS _ _ = tshow

instance ShowHSName Origin where
 showHSName ori = "Orig"<>tshow x<>tshow (hash x)
   where x :: Text
         x = case ori of
              FileLoc l sym -> "FileLoc (" <> tshow l <> " " <> sym <> ")"
              Origin s      -> "Origin " <> tshow s
              PropertyRule str declOrig 
                            -> "PropertyRule of "<>str<>" "<>
                                  case declOrig of 
                                    FileLoc l sym -> "declared at FileLoc (" <> tshow l <> " " <> sym <> ")"
                                    _             -> fatal $ "This should be the origin of a Relation, but it doesn't seem like it is.\n"
                                                               <>tshow declOrig 
              OriginUnknown -> "OriginUnknown"
              XLSXLoc fPath sheet (a,b) -> "XLSXLoc "<>T.pack fPath<>" "<>sheet<>" "<>tshow(a,b)
              MeatGrinder   -> "MeatGrinder"
instance ShowHS Origin where
 showHS env indent (FileLoc l s)               = "FileLoc (" <> showHS env indent l <> " " <> s <> ")"
 showHS env indent (PropertyRule str declOrig) = "PropertyRule " <> tshow str <> " ("<>showHS env indent declOrig<>")"
 showHS _     _     (Origin s)                  = "Origin " <> tshow s
 showHS _     _     OriginUnknown               = "OriginUnknown"
 showHS _     _     (XLSXLoc fPath sheet (a,b)) = "XLSXLoc "<>T.pack fPath<>" "<>sheet<>" "<>tshow(a,b)  
 showHS _     _     MeatGrinder                 = "MeatGrinder"

instance ShowHS Block where
 showHS _ _   = tshow

instance ShowHS Inline where
 showHS _ _   = tshow
                  

-- \***********************************************************************
-- \*** hulpfuncties                                                    ***
-- \***********************************************************************

haskellIdentifier :: Text -> Text
haskellIdentifier cs = unCap (hsId cs)
 where
   hsId xs = case T.uncons xs of
     Nothing -> mempty
     Just ('_', cs') -> T.cons '_' $ hsId cs'
     Just (c,cs') 
        | isAlphaNum c -> T.cons c $ hsId cs'
        | otherwise    -> hsId cs'

showL :: [Text] -> Text
showL xs = "["<>T.intercalate "," xs<>"]"
