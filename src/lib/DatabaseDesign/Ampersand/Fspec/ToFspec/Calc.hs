{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Fspec.ToFspec.Calc
            ( deriveProofs 
            , lambda
            , checkMono
            , showProof, showPrf
          --  , testInterface
            )
where

   import DatabaseDesign.Ampersand.Basics         (fatalMsg,Collection (isc),Identified(..),eqCl)
   import Data.List
   import GHC.Exts (sortWith)
   import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree hiding (sortWith)
   import DatabaseDesign.Ampersand.ADL1
   import DatabaseDesign.Ampersand.ADL1.Expression                 (subst,isNeg)
   import DatabaseDesign.Ampersand.Classes
   import DatabaseDesign.Ampersand.Fspec.Fspec (Fspc(..),Clauses(..),Quad(..),ECArule(..),InsDel(..),PAclause(..), HornClause(..), horn2expr)
   import DatabaseDesign.Ampersand.Fspec.ShowADL (ShowADL(..))
   import DatabaseDesign.Ampersand.Fspec.ShowECA (showECA)
   import DatabaseDesign.Ampersand.Fspec.ToFspec.ADL2Fspec
   import DatabaseDesign.Ampersand.Fspec.ToFspec.NormalForms        (conjNF,disjNF,cfProof,dfProof,nfProof,simplify,normPA,exprIsc2list, exprUni2list, exprCps2list, exprRad2list) --,proofPA) -- proofPA may be used to test derivations of PAclauses.
   import DatabaseDesign.Ampersand.Misc            (Lang(..),Options(..),PandocFormat(ReST),string2Blocks)
   import Text.Pandoc
   
   fatal :: Int -> String -> a
   fatal = fatalMsg "Fspec.ToFspec.Calc"

   showClause :: Fspc -> Clauses -> String
   showClause _ cl
    = "\nRule: "++showADL (cl_rule cl) ++concat
       [if null hornClauses then "\nNo clauses" else
        "\nConjunct: "++showADL conj++
        concat ["\n   Clause: "++showADL clause | clause<-hornClauses]
       | (conj, hornClauses)<-cl_conjNF cl]

-- testInterface :: Fspc -> Interface -> String
-- Deze functie is bedoeld om te bedenken hoe interfaces moeten worden afgeleid uit een vers vertaalde ObjectDef.
-- Nadat deze goed werkt kunnen de bewijsgenerator en de codegenerator worden gemaakt.
--   testInterface :: Options -> Fspc -> Interface -> String
--   testInterface flags fSpec ifc
--    = "\nInterface "++ name ifc++"("++intercalate ", " [showADL r++":"++name (target r) | r<-rels]++")\n"++
--      " - The parameters correspond to editable fields in a user interface.\n   "++
--      showADL ifc++"\n"++
--      " - Invariants:\n   "++intercalate "\n   " [showADL rule    | rule<-invs]++"\n"++
--      " - Derivation of clauses for ECA-rules:"   ++
--      concat [showClause fSpec (allClauses flags rule) | rule<-invs]++"\n"++
--{-
--      " - ECA rules:"++concat  [ "\n\n   "++showECA fSpec "\n>     "  (eca{ecaAction=normPA (ecaAction eca)})
--                                 ++"\n------ Derivation ----->"++showProof (showECA fSpec "\n>     ") (proofPA (ecaAction eca))++"\n<------End Derivation --"
--                               | eca<-ecaRs]++"\n\n"++
---}
--      " - Visible relations:\n   "++intercalate "\n   " (spread 80 ", " [showADL r  | r<-vis])++"\n"
--    where
----        showQ i (rel, shs,conj,r)
----         = "\nQuad "++show i++":\nrelation: "++showADL rel++":\nshifts: "++concat ["\n"++showADLe s |s<-shs]++"\nconjunct: "++showADL conj++"\nrule: "++showADL r++""
----TODO: Deze code komt ook voor in ADL2Fspec.hs. Dat lijkt dubbelop, en derhalve niet goed.
--        rels = nub (recur (ifcObj ifc))
--         where recur obj = [editMph (objctx o) | o<-objatsLegacy obj, editable (objctx o)]++[r | o<-objatsLegacy obj, r<-recur o]
--        vis        = nub (rels++map (I . target) rels)
--   --     visible r  = r `elem` vis
--        invs       = [rule | rule<-invariants fSpec, (not.null) (map makeDeclaration (mors rule) `isc` vis)]
--   --     qs         = vquads fSpec
--   --     ecaRs      = assembleECAs qs
----        editable (ERel Rel{} _)  = True    --WHY?? Stef, welke functie is de juiste?? TODO deze functie staat ook in ADL2Fspec.hs, maar is daar ANDERS(!)...
----        editable _               = False
----        editMph (ERel r@Rel{} _) = r       --WHY?? Stef, welke functie is de juiste?? TODO deze functie staat ook in ADL2Fspec.hs, maar is daar ANDERS(!)...
----        editMph e                = fatal 64 $ "cannot determine an editable declaration in a composite expression: "++show e
--        -- De functie spread verspreidt strings over kolommen met een breedte van n.
--        -- Deze functie garandeert dat alle strings worden afgedrukt in de aangegeven volgorde.
--        -- Hij probeert daarbij zo weinig mogelijk regels te gebruiken,
--        -- en alleen de grens van n te overschrijden als een string zelf langer is dan n.
--        spread :: Int -> String -> [String] -> [String]
--        spread n str = f ""
--         where f stored []       = [stored | not (null stored)]
--               f [] (cs:css)     = f cs css
--               f stored (cs:css) | length stored > n = stored: f cs css
--                                 | length new   <= n = f new css 
--                                 | otherwise         = stored: f cs css
--                                   where new = stored++str++cs

   deriveProofs :: Options -> Fspc -> [Inline]
   deriveProofs flags fSpec
    = [ --"\nSignals for "++name fSpec++"\n--------------\n"++
        --proof (signals fSpec)++
        LineBreak
      , Str ("Transformation of user specified rules into ECA rules for "++name fSpec)
      , LineBreak, Str "--------------", LineBreak, Str "--------------", LineBreak
      , LineBreak, Str "First step: determine all (", Str (show (length qs)), Str ") quads", LineBreak
      , LineBreak, Str "-- first quad ------------", LineBreak
      ]
      ++
      intercalate [LineBreak,Str "-- next quad ------------",LineBreak]
        [ [            Str "When relation ", Str (showADL rel), Str " is changed,"
          , LineBreak, Str (showADL r)
          ]++ (if length (cl_conjNF ccrs)<=1 then [Space] else [Str " (", Str (show (length (cl_conjNF ccrs))), Str " conjuncts)"]) ++
          [ Str " must be restored.", LineBreak, Str "This quad has conjunct: ", Str (showADL conj)
          , Str " and ", Str (show (length hornClauses)), Str " Horn clauses."
          ]++
          [ inl
          | hc<-hornClauses, inl<-[LineBreak, Str "Horn clause ", Str (showADL hc)]]
        | Quad rel ccrs<-qs, let r=cl_rule ccrs , (conj,hornClauses)<-cl_conjNF ccrs ]
      ++
      [ LineBreak, Str "-- end ------------", LineBreak, LineBreak, Str "Second step: assemble horn clauses."
      , LineBreak, Str "-- first Horn clause ------------", LineBreak]
      ++
      intercalate [LineBreak,Str "-- next Horn clause ------------",LineBreak]
        [ [ Str "Horn clause ", Str (showADL hc)
          , LineBreak, Str "is derived from rule ", Str (showADL r)
          , LineBreak
          , Str ( case ms of
                  []    -> "no relations affect this clause"
                  [rel] -> "It can be called when relation " ++showADL rel++" is affected."
                  _     -> "It can be called when relations "++commaEng "or" [showADL rel | rel<-ms]++" are affected."
                )
          ]
          | (ms,hc,r)<-
              [ (nubBy sameDecl [ rel |(rel,_,_)<-cl],hc,r)
              | cl<-eqCl (\(_,_,hc)->hc) [(rel,hc,r) |Quad rel ccrs<-qs, let r=cl_rule ccrs, (_,hornClauses)<-cl_conjNF ccrs, hc<-hornClauses]
              , let (_,hc,r) = head cl
              ]
         ]
      ++
      [ LineBreak, Str "-- end ------------", LineBreak, LineBreak, Str "Third step: determine ECA rules"] ++
      [ if verboseP flags
        then Str " (Turn --verbose off if you want to see ECA rules only)"
        else Str " (Turn on --verbose if you want to see more detail)" ] ++
      [ LineBreak ]
      ++
      concat
        [ [ LineBreak,Str "-- ECA Rule ", Str (show (ecaNum ecarule)), Str " ---------", LineBreak
          , Str (showECA fSpec "\n  " ecarule{ecaAction=normPA (ecaAction ecarule)}) ]++
          concat [ [ LineBreak, Str "delta expression", LineBreak, Space, Str (showADL d)
                   , LineBreak, Str "derivation:"
                   , LineBreak, Space]++
                   (showProof showADL. nfProof showADL) d ++  -- nfProof produces its result in disjunctive normal form
                   [ LineBreak, Str "disjunctly normalized delta expression" 
                   , LineBreak, Str (showADL (disjNF d))
                   ]
                 | verboseP flags, e@Do{}<-[ecaAction ecarule], let d = paDelta e ]
        | ecarule <- ecaRs]
      ++
      [ LineBreak, Str "--------------", LineBreak, LineBreak, Str "Fourth step: cascade blocking rules"
      , LineBreak
      ]++
      intercalate []
        [ [LineBreak, Str ("-- Raw ECA rule "++show (ecaNum er)++"------------"), LineBreak, Str (showECA fSpec "\n  " er)]
        | er<- ecaRs]
      ++
      [ LineBreak, Str "--------------", LineBreak, LineBreak, Str "Fifth step: preEmpt the rules (= optimize)"
      , LineBreak
      ]++
{- TODO: readdress preEmpt. It is wrong
      intercalate []
        [ [LineBreak, Str ("-- Preempted ECA rule "++show (ecaNum er)++"------------"), LineBreak, Str (showECA fSpec "\n  " er)]
        | er<- preEmpt ecaRs]
      ++ -}
      [ Str "--------------", LineBreak, Str "Final step: derivations --------------", LineBreak]
      ++
      intercalate [LineBreak, Str "--------------", LineBreak]
         [ derivation rule
         | rule<-udefrules fSpec]++
      [Str ("Aantal Rules: "++(show.length.udefrules) fSpec)]
{-
      ++
      [ LineBreak, Str "--------------", LineBreak]
      ++ -- TODO: make an ontological analysis, which explains the delete behaviour.
      [ Str "Ontological analysis: ", LineBreak, Str "  "]
      ++
      intercalate [LineBreak, LineBreak, Str "  "] 
          [ [Str (name ifc), Str "("]
            ++ intercalate [Str ", "] 
                 [[Str (name a), Str "[", Str ((name.target.ctx) a), Str "]"]
                 |a<-attributes (ifcObj ifc)]
            ++ [Str "):", LineBreak, Str "  "]
          | ifc<-interfaceS fSpec]
      ++
      [ LineBreak, Str "--------------", LineBreak
      , Str "Analyzing interfaces:", LineBreak, Str "     "]
      ++
      intercalate [LineBreak, Str "     "] 
         [[Str (testInterface flags fSpec ifc)]
         | ifc<-take 1 (interfaceG fSpec)]
      ++
      [ LineBreak, Str "--------------", LineBreak]
      -}
      where 
  --     visible _  = True -- We take all quads into account.
       qs         = vquads fSpec  -- the quads that are derived for this fSpec specify horn clauses, meant to maintain rule r, to be called when relation rel is affected (rel is in r).
          --   assembleECAs :: (Relation Concept->Bool) -> [Quad] -> [ECArule]
       ecaRs      = assembleECAs qs  -- this creates all ECA rules from the available quads. They are still raw (unoptimized).

  --     relEqCls = eqCl fst4 [(rel,hornClauses,conj,cl_rule ccrs) | Quad rel ccrs<-qs, (conj,hornClauses)<-cl_conjNF ccrs]
--  This is what ADL2Fpsec has to say about computing ECA rules:
--       ecas
--        = [ ECA (On ev rel) delt act
--          | relEq <- relEqCls                 -- The material required for one relation
--          , let (rel,_,_,_) = head relEq      -- This is the relation
--          , let ERel delt _ = delta (sign rel)  -- delt is a placeholder for the pairs that have been inserted or deleted in rel.
--          , ev<-[Ins,Del]                     -- This determines the event: On ev rel
--          , let act = ALL [ CHC [ (if isTrue  clause' || isTrue  step   then Nop else
--                                   if isFalse clause'   then Blk else
----                                 if not (visible rel) then Blk else
--                                   let visible _ = True in genPAclause visible ev toExpr viols)
--                                   [(conj,causes)]  -- the motivation for these actions
--                                | hc@(Hc antcs conss) <- hornClauses
--                                , let clause  = horn2expr hc
--                                , let sgn     = sign clause
--                                , let clause' = conjNF (subst (rel, actSem Ins rel (delta (sign rel))) clause)
--                                , let step    = conjNF (notCpl sgn clause .\/. clause')
--                                , let viols   = conjNF (notCpl sgn clause')
--                                , let negs    = EUni [notCpl sgn f | f<-antcs] sgn
--                                , let poss    = EUni conss sgn
--                                , let frExpr  = if ev==Ins
--                                                then conjNF negs
--                                                else conjNF poss
--                                , rel `elem` mors frExpr
--                                , let toExpr = if ev==Ins
--                                               then conjNF poss
--                                               else conjNF (notCpl negs)
--                                ]
--                                [(conj,causes)]  -- to supply motivations on runtime
--                          | conjEq <- eqCl snd3 [(hornClauses,conj,rule) | (_,hornClauses,conj,rule)<-relEq]
--                          , let causes          = nub (map thd3 conjEq)
--                          , let (hornClauses,conj,_) = head conjEq
--                          ]
--                          [(conj,nub [r |(_,_,_,r)<-cl]) | cl<-eqCl thd4 relEq, let (_,_,conj,_) = head cl]  -- to supply motivations on runtime
--          ]
--       fst4 (w,_,_,_) = w
--       snd3 (_,y,_) = y
--       thd3 (_,_,z) = z
--       thd4 (_,_,z,_) = z
       --TODO: See ticket #105
       derivation :: Rule -> [Inline]
       derivation rule 
         = [Str (showADL rule)]++
           ( if exx'==e
             then [Str " is already in conjunctive normal form", LineBreak]
             else [LineBreak, Str "Convert into conjunctive normal form", LineBreak] ++ showProof showADL ((e,[],"<=>"):prf)
           )++
           [ LineBreak, Str "Violations are computed by (disjNF . ECpl . normexpr) rule:\n     " ]++
           (disjProof. notCpl (sign rule). rrexp) rule++[ LineBreak, LineBreak ] ++
           concat [ [LineBreak, Str "Conjunct: ", Space, Space, Space, Space, Space, Str (showADL conjunct)]++
                    concat [ [LineBreak, Str "This conjunct has ", Str (show (length hornClauses)), Str " clauses:"] | length hornClauses>1 ]++
                    concat [ [LineBreak, Str "   Clause: ", Str (showADL clause)] | clause<-hornClauses]++[ LineBreak]++
                    concat [ [LineBreak, Str "For each clause, let us analyse the insert- and delete events."] | length hornClauses>1 ]++
                    concat [ [LineBreak, Str "   Clause: ", Str (showADL clause), Str " may be affected by the following events:",LineBreak]++
                             concat [ [Str "event = ", Str (show ev), Space, Str (showADL rel), Str " means doing the following substitution", LineBreak ] ++
                                      [Str (showADL clause++"["++showADL rel++":="++showADL (actSem ev rel (delta (sign rel)))++"] = clause'"), LineBreak ] ++
                                      [Str ("clause' = "++showADL ex'), LineBreak ] ++
                                      concat [ [Str ("which has CNF: "++showADL ex'), LineBreak] | clause'/=ex'] ++
                                      [Str ("Computing the violations means to negate the conjunct: "++showADL (notClau)), LineBreak ] ++
                                      concat [ [Str ("which has CNF: "++showADL viols), LineBreak] | notClau/=viols] ++
                                      [Str "Now try to derive whether clause |- clause' is true... ", LineBreak, Str (showADL (notClau .\/. clause')), LineBreak, Str "<=>", LineBreak, Str (showADL step), LineBreak ]
                                    | decl <-nub (mors r)
                                    , ev<-[Ins,Del]
                                    , let rel     = makeRelation decl
                                    , let ex'     = subst (rel, actSem ev rel (delta (sign rel))) expr
                                    , let clause' = conjNF ex'
                                    , let notClau = notCpl sgn clause'
                                    , let step    = conjNF (notClau .\/. clause')
                                    , let viols   = conjNF (notClau)
                                    , let negs    = foldr (./\.) (vExpr sgn) antcs
                                    , let poss    = foldr (.\/.) (notCpl sgn (vExpr sgn)) conss
                                    , let frExpr  = if ev==Ins
                                                    then conjNF negs
                                                    else conjNF poss
                                    , decl `elem` mors frExpr
                        --            , let toExpr = if ev==Ins
                        --                           then conjNF poss
                        --                           else conjNF (notCpl negs)
                                    ]
                           | clause@(Hc antcs conss) <- hornClauses
                           , let expr = horn2expr clause, let sgn = sign expr ]
                  | let Clauses ts r = allClauses flags rule, (conjunct, hornClauses)<-ts] ++
           [LineBreak]
{-
           [ Str ("Available code fragments on rule "++name rule++":", LineBreak ]++
           intercalate [LineBreak] [showADL rule++ " yields\n"++intercalate "\n\n"
                                   [ [Str "event = ", Str (show ev), Space, Str (showADL rel), LineBreak ] ++
                                     [Str (showADL r++"["++showADL rel++":="++showADL (actSem ev rel (delta (sign rel)))++"] = r'"), LineBreak ] ++
                                     [Str "r'    = "] ++ conjProof r' ++ [LineBreak ] ++
                                     [Str "viols = r'-"] ++ disjProof (ECpl r') ++ [ LineBreak ] ++
                                     "violations, considering that the valuation of "++showADL rel++" has just been changed to "++showADL (actSem ev rel (delta (sign rel)))++
                                     "            "++conjProof (ECpl r) ++"\n"++
                                     "reaction? evaluate r |- r' ("++(showADL.conjNF) (notCpl sgn r .\/. r')++")"++
                                        conjProof (notCpl sgn r .\/. r')++"\n"++
                                     "delta: r-/\\r' = "++conjProof (EIsc[notCpl r,r'])++
                                     "\nNow compute a reaction\n(isTrue.conjNF) (notCpl sgn r .\/. r') = "++show ((isTrue.conjNF) (notCpl sgn r .\/. r'))++"\n"++
                                     (if null (lambda ev (ERel rel ) r)
                                      then "lambda "++showADL rel++" ("++showADL r++") = empty\n"
                                      else -- for debug purposes:
                                           -- "lambda "++show ev++" "++showADL rel++" ("++showADL r++") = \n"++(intercalate "\n\n".map showPr.lambda ev (ERel rel)) r++"\n"++
                                           -- "derivMono ("++showADL r++") "++show ev++" "++showADL rel++"\n = "++({-intercalate "\n". map -}showPr.derivMono r ev) rel++"\n"++
                                           -- "\nNow compute checkMono r ev rel = \n"++show (checkMono r ev rel)++"\n"++
                                           if (isTrue.conjNF) (notCpl sgn r .\/. r')
                                           then "A reaction is not required, because  r |- r'. Proof:"++conjProof (notCpl sgn r .\/. r')++"\n"
                                           else if checkMono r ev rel
                                           then "A reaction is not required, because  r |- r'. Proof:"{-++(showPr.derivMono r ev) rel-}++"NIET TYPECORRECT: (showPr.derivMono r ev) rel"++"\n"  --WHY? Stef, gaarne herstellen...Deze fout vond ik nadat ik het type van showProof had opgegeven.
                                           else let ERel _ _ = delta (sign rel) in
                                                "An appropriate reaction on this event is required."
                                           --     showECA fSpec "\n  " (ECA (On ev rel) delt (genPAclause visible Ins r viols conj [rule]) 0)
                                     )
                                   | rel<-nub [x |x<-mors r, not (isIdent x)] -- TODO: include proofs that allow: isIdent rel'
                                   , ev<-[Ins,Del]
                                   , r'<-[subst (rel, actSem ev rel (delta (sign rel))) r]
                        --        , viols<-[conjNF (ECpl r')]
                                   , True ]  -- (isTrue.conjNF) (notCpl sgn r .\/. r')
                                  | r<-[hc | cs<-[allClauses flags rule], (_,hornClauses)<-cl_conjNF cs, hc<-hornClauses]
                                  ]
-}
              where e = rrexp rule
                    prf = cfProof showADL e
                    (exx',_,_) = last prf
               --     conjProof = showProof showADL . cfProof showADL
                    disjProof = showProof showADL . dfProof showADL
--                    showPr    = showProof showADL  -- hoort bij de uitgecommentaarde code hierboven...
       --TODO: See ticket #105
       commaEng :: String -> [String] -> String
       commaEng  _  []       = ""
       commaEng  _  [x]      = x
       commaEng str [x,y]    = x++" "++str++" "++y
       commaEng str (x:y:ys) = x++", "++commaEng str (y:ys)

-- Stel we voeren een actie a uit, die een(1) van de volgende twee is:
--        {r} INS rel INTO expr {r'}       ofwel
--        {r} DEL rel FROM expr {r'}
-- Dan toetst checkMono of r|-r' waar is op grond van de afleiding uit derivMono.
-- Als dat waar is, betekent dat dat invariant r waar blijft wanneer actie a wordt uitgevoerd.
   checkMono :: Expression
             -> InsDel
             -> Relation
             -> Bool
   checkMono expr ev rel
     = case ruleType conclusion of
        Truth -> fatal 247 "derivMono came up with a Truth!"
        _     -> simplify expr == simplify (antecedent conclusion) &&
                 simplify (subst (rel, actSem ev rel (delta (sign rel))) expr) ==
                 simplify (consequent conclusion)
     where (conclusion,_,_) = last (derivMono expr ev rel)


   type Proof expr = [(expr,[String],String)]
   reversePrf :: Proof e -> Proof e
   reversePrf [] = []
   reversePrf [s] = [s]
   reversePrf ((r,cs,e'):prf@((r',_ ,_):_)) = init rp++[(r',cs,rev e'),(r,[],"")]
     where rp = reversePrf prf
           rev "==>" = "<=="
           rev "<==" = "==>"
           rev "-->" = "<--"
           rev "<--" = "-->"
           rev x = x

   showProof :: (expr->String) -> Proof expr -> [Inline]
   showProof shw [(expr,_,_)]        = [ LineBreak, Space, Space, Space, Space, Space, Space, Str (shw expr), LineBreak]
   showProof shw ((expr,ss,equ):prf) = [ LineBreak, Space, Space, Space, Space, Space, Space, Str (shw expr), LineBreak] ++
                                       (if null ss  then [ Space, Space, Space, Str equ ] else
                                        if null equ then [ Str (unwords ss) ] else
                                        [ Space, Space, Space, Str equ, Str (" { "++intercalate " and " ss++" }") ])++
                                       showProof shw prf
                                       --where e'= if null prf then "" else let (expr,_,_):_ = prf in showHS options "" expr 
   showProof _  []                   = []

   showPrf :: (expr->String) -> Proof expr -> [String]
   showPrf shw [(expr,_,_)]        = [ "    "++shw expr]
   showPrf shw ((expr,ss,equ):prf) = [ "    "++shw expr] ++
                                     (if null ss  then [ equ ] else
                                      if null equ then [ unwords ss ] else
                                      [ equ++" { "++intercalate " and " ss++" }" ])++
                                     showPrf shw prf
   showPrf _  []                   = []

-- derivMono provides a derivation to prove that (precondition) r is a subset of (postcondition) r'.
-- This is useful in proving that an action {expr} a {expr'} maintains its invariant, i.e. that  expr|-expr'  holds (proven by monotony properties)
-- Derivmono gives a derivation only.
   derivMono :: Expression -> InsDel -> Relation -> [(Rule, [String], String)]
   derivMono expr -- preconditie van actie a
             tOp  -- de actie (Ins of Del)
             rel' -- re relatie, zodat de actie bestaat uit INSERT rel' INTO expr of DELETE rel' FROM expr
    = f (head (lambda tOp (ERel rel' (sign expr)) expr++[[]])) (start tOp)
    where
     f :: [(Expression, [String], whatever)] 
        -> (Expression, Expression) 
        -> [(Rule, [String], String)]
     f [] (_,_) = []
     f [(e',_,_)] (neg',pos')
      = [(rule (subst (rel',neg') e') (subst (rel',pos') e'),[],"")]
     f ((e',["invert"],_): prf@((_,_,_):_)) (neg',pos')
      = (rule (subst (rel',neg') e') (subst (rel',pos') e'),["r |- s  <=>  s- |- r-"],"<=>"):
         f prf (pos',neg')
     f ((e1,_,_): prf@((e2,_,_):_)) (neg',pos')
      = (rule (subst (rel',neg') e1) (subst (rel',pos') e1),["Monotony of "++showOp e2],"==>"):
         f prf (neg',pos')
         
     start Ins  = (ERel rel' (sign rel'),ERel rel' (sign rel') .\/. delta (sign rel'))
     start Del  = (ERel rel' (sign rel') ./\. notCpl (sign rel') (delta (sign rel')),ERel rel' (sign rel'))

     rule :: Expression -> Expression -> Rule
     rule neg' pos' | isTrue neg' = Ru { rrnm  = ""
                                       , rrfps = Origin "rule generated for isTrue neg' by Calc"
                                       , rrexp = pos'
                                       , rrmean = AMeaning
                                                  [A_Markup Dutch   ReST (string2Blocks ReST "Waarom wordt deze regel hier aangemaakt? (In Calc.hs, regel 402)")
                                                  ,A_Markup English ReST (string2Blocks ReST "Why is this rule created? (In Calc.hs, line 403)")]  --TODO Stef, gaarne de explanations aanvullen/verwijderen. Dank! Han.
                                       , rrmsg = []
                                       , rrviol = Nothing
                                       , rrtyp = sign neg' {- (neg `meet` pos) -}
                                       , rrdcl = Nothing
                                       , r_env = ""
                                       , r_usr = Multiplicity
                                       , r_sgl = fatal 336 $ "erroneous reference to r_sgl in rule ("++showADL neg'++") |- ("++showADL pos'++")"
                                       , srrel = fatal 337 $ "erroneous reference to srrel in rule ("++showADL neg'++") |- ("++showADL pos'++")"
                                       }
                    | otherwise   = Ru { rrnm  = ""
                                       , rrfps = Origin "rule generated for not(isTrue neg') by Calc"
                                       , rrexp = neg' .|-. pos'
                                       , rrmean = AMeaning
                                                  [A_Markup Dutch   ReST (string2Blocks ReST "Waarom wordt deze regel hier aangemaakt? (In Calc.hs, regel 332)")
                                                  ,A_Markup English ReST (string2Blocks ReST "Why is this rule created? (In Calc.hs, line 333)")]  --TODO Stef, gaarne de explanations aanvullen/verwijderen. Dank! Han.
                                       , rrmsg = []
                                       , rrviol = Nothing
                                       , rrtyp = sign neg' {- (neg `meet` pos) -}
                                       , rrdcl = Nothing
                                       , r_env = ""
                                       , r_usr = Multiplicity
                                       , r_sgl = fatal 352 $ "illegal reference to r_sgl in rule ("++showADL neg'++") |- ("++showADL pos'++")"
                                       , srrel = fatal 353 $ "illegal reference to srrel in rule ("++showADL neg'++") |- ("++showADL pos'++")"
                                       }
     showOp expr' = case expr' of
                     EEqu{} -> "="
                     EImp{} -> "|-"
                     EIsc{} -> "/\\"
                     EUni{} -> "\\/"
                     EDif{} -> "-"
                     ELrs{} -> "/"
                     ERrs{} -> "\\"
                     ECps{} -> ";"
                     ERad{} -> "!"
                     EPrd{} -> "*"
                     EKl0{} -> "*"
                     EKl1{} -> "+"
                     EFlp{} -> "~"
                     ECpl{} -> "-"
                     _      -> ""

{- The purpose of function lambda is to generate a derivation.
Rewrite rules:
-r;-s -> -(r!s)
-}
   lambda :: InsDel -> Expression 
                    -> Expression 
                    -> [Proof Expression]
   lambda tOp' e' expr' = [reversePrf[(e'',text,op)
                          | (e'',_,text,op)<-prf]
                          | prf<-lam tOp' e' expr' ]
    where
     lam :: InsDel -> Expression -> Expression ->
            [[(Expression,Expression -> Expression,[String],String)]]
     lam tOp e3 expr =
          case expr of
             EIsc{}  | e3==expr             -> [[(e3,id,[],"")]]
                     | length (const' expr)>0 -> [(expr,\_->expr,      [derivtext tOp "mono" (inter' expr) expr],"<--") :prf
                                                 | prf<-lam tOp e3 (inter' expr)
                                                 ]
                     | and [isNeg f |f<-exprIsc2list expr] -> [(expr, deMorgan sgn, [derivtext tOp "equal" deMrg expr],"==") :prf | prf<-lam tOp e3 deMrg]
                     | or[null p |p<-fPrfs] -> []
                     | otherwise            -> [(expr,\_->expr,    [derivtext tOp "mono" (first lc) expr],"<--") : lc]
             EUni{}  | e3==expr             -> [[(e3,id,[],"")]]
                     | length (const' expr)>0 -> [(expr,\_->expr, [derivtext tOp "mono" (inter' expr) expr],"<--") :prf
                                                      | prf<-lam tOp e3 (inter' expr)
                                                      ]
                     | and [isNeg f |f<-exprUni2list expr] -> [(expr, deMorgan sgn, [derivtext tOp "equal" deMrg expr],"==") :prf | prf<-lam tOp e3 deMrg]
                     | or[null p |p<-fPrfs] -> []
                     | otherwise            -> [(expr,\_->expr,    [derivtext tOp "mono" (first lc) expr],"<--") : lc]
             ECps{}  | e3==expr             -> [[(e3,id,[],"")]]
                     | and [isNeg f |f<-exprCps2list expr] -> [(expr, deMorgan sgn, [derivtext tOp "equal" deMrg expr],"==")
                                               :prf
                                               | prf<-lam tOp e3 deMrg
                                               ] -- isNeg is nog niet helemaal correct.
                     | or[null p|p<-fPrfs] -> []
                     | otherwise            -> [(expr,\_->expr,    [derivtext tOp "mono" (first lc) expr],"<--"): lc]        
             ERad{}  | e3==expr             -> [[(e3,id,[],"")]]
                     | and [isNeg f |f<-exprRad2list expr] -> [(expr, deMorgan sgn, [derivtext tOp "equal" deMrg expr],"==") :prf | prf<-lam tOp e3 deMrg] -- isNeg is nog niet helemaal correct.
                     | or[null p |p<-fPrfs] -> []
                     | otherwise            -> [(expr,\_->expr,    [derivtext tOp "mono" (first lc) expr],"<--"): lc]
             EKl0 x _                         -> [(expr,\e->EKl0 e sgn,[derivtext tOp "mono" x expr],"<--") :prf   | prf<-lam tOp e3 x]
             EKl1 x _                         -> [(expr,\e->EKl1 e sgn,[derivtext tOp "mono" x expr],"<--") :prf   | prf<-lam tOp e3 x]
             ECpl x _                         -> [(expr,\e->ECpl e sgn,["invert"],"<--") :prf | prf<-lam (inv tOp) e3 x]
             EBrk x                           -> lam tOp e3 x
             _                                -> [[(e3,id,[],"")]]
           where
               sgn   = sign expr
               deMrg = deMorgan sgn expr
               fPrfs = case expr of
                        EUni{} -> [lam tOp e3 f |f<-exprUni2list expr, isVar f e3]
                        EIsc{} -> [lam tOp e3 f |f<-exprIsc2list expr, isVar f e3]
                        ECps{} -> [lam tOp e3 f |f<-exprCps2list expr, isVar f e3]
                        ERad{} -> [lam tOp e3 f |f<-exprRad2list expr, isVar f e3]
                        _      -> fatal 428 ("fPrfs  is not defined.Consult your dealer!")
               lc = longstcomn vars++concat (drop (length rc-1) (sortWith length rc))
               rc = remainders vars vars
               vars = map head fPrfs
               const' e@EUni{} = [f |f<-exprUni2list e, isConst f e3]
               const' e@EIsc{} = [f |f<-exprIsc2list e, isConst f e3]
               const' expr'' = fatal 440 $ "'const'("++ show expr''++")' is not defined.Consult your dealer!"
               inter' e@EUni{} = foldr (.\/.) (notCpl sgn (vExpr sgn)) [f |f<-exprUni2list e, isVar f e3]
               inter' e@EIsc{} = foldr (./\.) (vExpr sgn) [f |f<-exprIsc2list e, isVar f e3]
               inter' expr'' = fatal 443 $ "'inter'("++ show expr''++")' is not defined.Consult your dealer!"
 --      lam tOp e f       = []

  -- longstcomn determines the longest prefix common to all xs in xss.
     longstcomn :: (Eq a) => [[(a, b, c, d)]] -> [(a, b, c, d)]
     longstcomn xss | or [null xs | xs<-xss]      = []
                    | length (eqCl first xss)==1 = head [head prf | prf<-xss]: longstcomn [tail prf | prf<-xss]
                    | otherwise                  = []
    -- remainders determines the remainders.
     remainders :: (Eq a) => [[(a, b, c, d)]] -> [[(a, b, c, d)]] -> [[(a, b, c, d)]]
     remainders _ xss | or [null xs | xs<-xss]      = xss
                      | length (eqCl first xss)==1 = remainders xss [tail prf | prf<-xss]
                      | otherwise                  = xss
     isConst :: (ConceptStructure a, ConceptStructure b) => a->b->Bool
     isConst e f = null (mors e `isc` mors f)
     isVar :: (ConceptStructure a, ConceptStructure b) => a->b->Bool
     isVar e f   = not (isConst e f)
     derivtext :: InsDel -> String -> Expression -> Expression -> String
     derivtext tOp "invert" e'' expr = sh tOp++showADL e''++" means "++sh (inv tOp)++showADL expr++"."
     derivtext tOp "mono"    e'' expr = "("++showADL e''++"->"++showADL expr++") is monotonous, so "++sh tOp++showADL e''++" means "++sh tOp++showADL expr++"."
     derivtext _ str _ _ = str
     sh :: InsDel -> String
     sh Ins  = "insert into "
     sh Del  = "delete from "
     inv :: InsDel -> InsDel
     inv Ins = Del
     inv Del = Ins
     first :: [(a,b,c,d)] -> a
     first ((e'',_,_,_):_) = e''
     first _ = fatal 472 "wrong pattern in first"
     
   ruleType :: Rule -> RuleType
   ruleType r = case rrexp r of
                 EEqu{} -> Equivalence
                 EImp{} -> Implication
                 _      -> Truth


