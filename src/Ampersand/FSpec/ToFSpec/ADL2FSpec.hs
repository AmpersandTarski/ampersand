{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Ampersand.FSpec.ToFSpec.ADL2FSpec
   ( makeFSpec
   ) where
import           Ampersand.ADL1
import           Ampersand.Basics hiding (Identity)
import           Ampersand.Classes
import           Ampersand.Core.ShowAStruct
import           Ampersand.FSpec.Crud
import           Ampersand.FSpec.FSpec
import           Ampersand.FSpec.ToFSpec.ADL2Plug
import           Ampersand.FSpec.ToFSpec.Calc
import           Ampersand.FSpec.ToFSpec.NormalForms 
import           Ampersand.FSpec.ToFSpec.Populated 
import           Ampersand.Misc.HasClasses
import qualified RIO.List as L
import qualified RIO.NonEmpty as NE
import qualified RIO.NonEmpty.Partial as PARTIAL
import qualified RIO.Set as Set
import qualified RIO.Text as T

{- The FSpec-datastructure should contain all "difficult" computations. This data structure is used by all sorts of rendering-engines,
such as the code generator, the functional-specification generator, and future extentions. -}
makeFSpec :: (HasFSpecGenOpts env) =>
    env -> A_Context -> FSpec
makeFSpec env context
 =      FSpec { fsName       = name context
              , originalContext = context 
              , fspos        = ctxpos context
              , plugInfos    = allplugs
              , interfaceS   = fSpecAllInterfaces -- interfaces specified in the Ampersand script
              , roleInterfaces = fSpecRoleInterfaces
              , interfaceG   = [ifc | ifc<-interfaceGen, let ctxrel = objExpression (ifcObj ifc)
                                    , isIdent ctxrel && source ctxrel==ONE
                                      || ctxrel `notElem` map (objExpression.ifcObj) fSpecAllInterfaces
                                    , view genInterfacesL env]  -- generated interfaces
              , fDeriveProofs = deriveProofs env context 
              , fRoleRuls    = L.nub [(role',rule)   -- fRoleRuls says which roles maintain which rules.
                                   | rule <- Set.elems allrules
                                   , role' <- maintainersOf rule
                                   ]
              , fMaintains   = fMaintains'
              , fRoles       = zip ((L.sort . L.nub) (  concatMap (NE.toList . arRoles) (ctxrrules context)
                                                 <> concatMap ifcRoles               (ctxifcs context )
                                                 )
                                   ) [0..] 
              , fallRules    = allrules
              , vrules       = Set.filter      isUserDefined  allrules
              , grules       = Set.filter (not.isUserDefined) allrules
              , invariants   = Set.filter (not.isSignal)      allrules
              , signals      = Set.filter      isSignal       allrules
              , allConjuncts = allConjs
              , allConjsPerRule = fSpecAllConjsPerRule
              , allConjsPerDecl = fSpecAllConjsPerDecl
              , allConjsPerConcept = fSpecAllConjsPerConcept
              , vquads       = allQuads
              , allUsedDecls = bindedRelationsIn context
              , vrels        = calculatedDecls
              , allConcepts  = fSpecAllConcepts
              , cptTType     = representationOf contextinfo
              , fsisa        = L.nub . concatMap genericAndSpecifics . gens $ context
              , vpatterns    = patterns context
              , vgens        = gens context
              , vIndices     = identities context
              , vviews       = viewDefs context
              , lookupView   = lookupView'
              , getDefaultViewForConcept = getDefaultViewForConcept'
              , getAllViewsForConcept = getAllViewsForConcept'
              , conceptDefs  = ctxcds context
              , fSexpls      = ctxps context <> concatMap ptxps (patterns context)
              , metas        = ctxmetas context
              , crudInfo     = mkCrudInfo fSpecAllConcepts calculatedDecls fSpecAllInterfaces
              , atomsInCptIncludingSmaller = atomValuesOf contextinfo initialpopsDefinedInScript --TODO: Write in a nicer way, like `atomsBySmallestConcept`
              , atomsBySmallestConcept = \cpt -> Set.map apLeft 
                                               . pairsinexpr 
                                               . foldl' (.-.) (EDcI cpt) 
                                               . map (handleType cpt)
                                               . smallerConcepts (gens context) $ cpt
              , tableContents = tblcontents contextinfo initialpopsDefinedInScript
              , pairsInExpr  = pairsinexpr
              , applyViolText = apply_viol_text
              , allViolations  = [ (r,vs)
                                 | r <- Set.elems allrules
                                 , let vs = ruleviolations r, not (null vs) ]
              , allExprs     = expressionsIn context `Set.union` expressionsIn allConjs
              , initialConjunctSignals = [ (conj, viols) | conj <- allConjs 
                                         , let viols = conjunctViolations conj
                                         , not $ null viols
                                         ]
              , fcontextInfo = contextinfo
              , ftypologies   = typologies context
              , typologyOf = typologyOf'
              , largestConcept = getLargestConcept 
              , specializationsOf = smallerConcepts (gens context)
              , generalizationsOf = largerConcepts  (gens context)
              }
   where           
     getLargestConcept cpt = case largerConcepts (gens context) cpt of
                              [] -> cpt
                              x:_ -> getLargestConcept x
     handleType :: A_Concept -> A_Concept -> Expression
     handleType gen spc = EEps gen (Sign gen spc) .:. EDcI spc .:. EEps gen (Sign spc gen)
     fMaintains' :: Role -> Rules
     fMaintains' role' = Set.filter f allrules
        where f rule = role' `elem` maintainersOf rule
     typologyOf' cpt = 
        case [t | t <- typologies context, cpt `elem` tyCpts t] of
           [t] -> t
           _   -> fatal ("concept "<>name cpt<>" should be in exactly one typology!")
     pairsinexpr  :: Expression -> AAtomPairs
     pairsinexpr = fullContents contextinfo initialpopsDefinedInScript
     -- Purpose: to write a rule violation in Text as specified in the user's script,
     -- to be used in error messages too.
     apply_viol_text :: Rule -> AAtomPair -> Text
     apply_viol_text rule violPair
      = case rrviol rule of
          Nothing -> "(" <> aavtxt (apLeft violPair) <> ", " <> aavtxt (apRight violPair) <> ")"
          Just pv -> pairsegs
            where
              pairsegs :: Text
              pairsegs = let (h NE.:| tl) = NE.map totext . ppv_segs $ pv 
                         in foldl' (<>) h tl
              totext :: PairViewSegment Expression -> Text
              totext (PairViewText _ str) = str
              totext (PairViewExp _ Src expr) = lrToText apLeft expr
              totext (PairViewExp _ Tgt expr) = lrToText apRight expr
              lrToText :: (AAtomPair -> AAtomValue) -> Expression -> Text
              lrToText g expr
               = case fmap (aavtxt.apRight) . toList . Set.filter (\ap->g violPair==apLeft ap) . pairsinexpr $ expr
                 of [h]  -> h
                    []   -> ""
                    xs   -> "{" <> T.intercalate ", " xs <> "}"
     ruleviolations :: Rule -> AAtomPairs
     ruleviolations r = case formalExpression r of
          EEqu{} -> (cra Set.\\ crc) `Set.union` (crc Set.\\ cra)
          EInc{} -> cra Set.\\ crc
          _      -> pairsinexpr (EDcV (sign (consequent r))) Set.\\ crc  --everything not in con
          where cra = pairsinexpr (antecedent r)
                crc = pairsinexpr (consequent r)
     conjunctViolations :: Conjunct -> AAtomPairs
     conjunctViolations conj = pairsinexpr (notCpl (rc_conjunct conj))
     contextinfo = ctxInfo context

     fSpecAllConcepts = concs context
     fSpecAllInterfaces :: [Interface]
     fSpecAllInterfaces = map enrichIfc (ctxifcs context)
       where
          enrichIfc :: Interface -> Interface
          enrichIfc ifc
           = ifc{ ifcControls = makeIfcControls Set.empty allConjs
                }
     fSpecRoleInterfaces :: Role -> [Interface]
     fSpecRoleInterfaces role' = filter (forThisRole role') fSpecAllInterfaces
     forThisRole ::Role -> Interface -> Bool
     forThisRole role' interf = case ifcRoles interf of
                                     []   -> True -- interface is for all roles
                                     rs  -> role' `elem` rs
     
     initialpopsDefinedInScript = 
                   [ let dcl = popdcl (NE.head eqclass)
                     in ARelPopu{ popsrc = source dcl
                                , poptgt = target dcl
                                , popdcl = dcl
                                , popps  = Set.unions [ popps pop | pop<-NE.toList eqclass ]
                                }
                   | eqclass<-eqCl popdcl [ pop | pop@ARelPopu{}<-populations ] ] <>
                   [ ACptPopu{ popcpt = popcpt (NE.head eqclass)
                             , popas  = (L.nub.concat) [ popas pop | pop<-NE.toList eqclass ]
                             }
                   | eqclass<-eqCl popcpt [ pop | pop@ACptPopu{}<-populations ] ]
       where populations = ctxpopus context<>concatMap ptups (patterns context)       
     allConjs = makeAllConjs env allrules
     fSpecAllConjsPerRule :: [(Rule, NE.NonEmpty Conjunct)]
     fSpecAllConjsPerRule = converseNE [ (conj, rc_orgRules conj) | conj <- allConjs ]
     fSpecAllConjsPerDecl = converse [ (conj, Set.elems . bindedRelationsIn $ rc_conjunct conj) | conj <- allConjs ] 
     fSpecAllConjsPerConcept = 
           converse [ (conj, L.nub $ smaller (source e) <> smaller (target e)) 
                    | conj <- allConjs
                    , e    <- Set.elems . modifyablesByInsOrDel . rc_conjunct $ conj ]
               where 
                 smaller :: A_Concept -> [A_Concept]
                 smaller cpt = L.nub $ cpt : smallerConcepts (gens context) cpt
     allQuads = quadsOfRules env allrules 
     
     allrules = Set.map setIsSignal (allRules context)
        where setIsSignal r = r{isSignal = (not.null) (maintainersOf r)}
     maintainersOf :: Rule -> [Role]
     maintainersOf r 
       = L.nub . concatMap (NE.toList . arRoles) . filter forThisRule . ctxrrules $ context
         where
          forThisRule :: A_RoleRule -> Bool
          forThisRule x = name r `elem` arRules x
     isUserDefined rul =
       case r_usr rul of
         UserDefined  -> True
         Multiplicity -> False
         Identity     -> False
     calcProps :: Relation -> Relation
     calcProps d = d{decprps_calc = Just calculated}
         where calculated = decprps d `Set.union` (if d `elem` totals      then Set.singleton Tot else Set.empty)
                                      `Set.union` (if d `elem` surjectives then Set.singleton Sur else Set.empty)
     calculatedDecls :: Relations
     calculatedDecls = Set.map calcProps (relsDefdIn context)
  -- determine relations that are total (as many as possible, but not necessarily all)
     totals      = [ d |       EDcD d  <- totsurs ]
     surjectives = [ d | EFlp (EDcD d) <- totsurs ]
     totsurs :: [Expression]
     totsurs = []
--      = L.nub [rel | q<- filter (isIdent . EDcD . qDcl)   -- FIXME: This cannot be correct. This filter will block everything!
--                     . filter (not . isSignal . qRule)
--                     $ allQuads -- all quads for invariant rules
--                 , dnf<- concatMap rc_dnfClauses . qConjuncts $ q
--                 , let antc = conjNF env (foldr (./\.) (EDcV (sign (NE.head (antcs dnf)))) (antcs dnf))
--                 , isRfx antc -- We now know that I is a subset of the antecedent of this dnf clause.
--                 , cons<- case conss dnf of
--                            []   -> []
--                            h:tl -> NE.toList $ fmap exprCps2list (h NE.:| tl)
--            -- let I |- r;s;t be an invariant rule, then r and s and t~ and s~ are all total.
--                 , rel<-NE.init cons<>[flp r | r<-NE.tail cons]
--                 ]
  -- Lookup view by id in fSpec.
     lookupView' :: ViewUsage -> ViewDef
     lookupView'  ViewUsage{vuView = viewId} =
       case filter (\v -> name v == viewId) $ viewDefs context of
         []   -> fatal ("Undeclared view " <> tshow viewId <> ".") -- Will be caught by static analysis
         [vd] -> vd
         vds  -> fatal ("Multiple views with id " <> tshow viewId <> ": " <> tshow (map name vds)) -- Will be caught by static analysis
     
   -- get all views for a specific concept and all larger concepts.
     getAllViewsForConcept' :: A_Concept -> [ViewDef]
     getAllViewsForConcept' concpt = 
              concatMap viewsOfThisConcept
            . sortSpecific2Generic (gens context) 
            $ concpt : largerConcepts (gens context) concpt 
       
     viewsOfThisConcept :: A_Concept -> [ViewDef]
     viewsOfThisConcept cpt = filter isForConcept $ viewDefs context
       where
         isForConcept :: ViewDef -> Bool
         isForConcept vd = vdcpt vd == cpt
     -- Return the default view for cpt, which is either the view for cpt itself (if it has one) or the view for
     -- cpt's smallest superconcept that has a default view. Return Nothing if there is no default view.
     getDefaultViewForConcept' :: A_Concept -> Maybe ViewDef
     getDefaultViewForConcept' cpt =
       case  filter vdIsDefault
           . concatMap viewsOfThisConcept
           . sortSpecific2Generic (gens context) 
           $ cpt : largerConcepts (gens context) cpt of
         []     -> Nothing 
         (vd:_) -> Just vd

     --------------
     --making plugs
     --------------
     allplugs = genPlugs             -- all generated plugs
     genPlugs = [InternalPlug (rename p (qlfname (name p)))
                | p <- uniqueNames [] (makeGeneratedSqlPlugs env context calcProps)
                ]
     qlfname x = if T.null ns then x else "ns"<>ns<>x
       where ns = view namespaceL env
     --TODO151210 -> Plug A is overbodig, want A zit al in plug r
--CONTEXT Temp
--PATTERN Temp
--r::A*B[TOT].
--t::E*ECps[UNI].
--ENDPATTERN
--ENDCONTEXT
{-
    **************************************
    * Plug E                               *
    * I  [INJ,SUR,UNI,TOT,SYM,ASY,TRN,RFX] *
    * t  [UNI]                             *
    **************************************
    * Plug ECps                            *
    * I  [INJ,SUR,UNI,TOT,SYM,ASY,TRN,RFX] *
    **************************************
    * Plug B                               *
    * I  [INJ,SUR,UNI,TOT,SYM,ASY,TRN,RFX] *
    **************************************
    * Plug A                               *
    * I  [INJ,SUR,UNI,TOT,SYM,ASY,TRN,RFX] *
    **************************************
    * Plug r                               *
    * I  [INJ,SUR,UNI,TOT,SYM,ASY,TRN,RFX] *
    * r  [TOT]                             *
    **************************************
-}
     -------------------
     --END: making plugs
     -------------------
     -------------------
     --making interfaces
     -------------------
     -- interfaces (type BoxItem) can be generated from a basic ontology. That is: they can be derived from a set
     -- of relations together with multiplicity constraints. That is what interfaceG does.
     -- This is meant to help a developer to build his own list of interfaces, by providing a set of interfaces that works.
     -- The developer may relabel attributes by names of his own choice.
     -- This is easier than to invent a set of interfaces from scratch.

     -- Rule: a interface must be large enough to allow the required transactions to take place within that interface.
     -- Attributes of an BoxItem have unique names within that BoxItem.

--- generation of interfaces:
--  Ampersand generates interfaces for the purpose of quick prototyping.
--  A script without any mention of interfaces is supplemented
--  by a number of interface definitions that gives a user full access to all data.
--  Step 1: select and arrange all relations to obtain a set cRels of total relations
--          to ensure insertability of entities (signal relations are excluded)
     cRels = Set.elems $
              Set.filter isTot toconsider 
                `Set.union`
             (Set.map flp . Set.filter (not.isTot) . Set.filter isSur $ toconsider)
       where toconsider = Set.map EDcD calculatedDecls
--  Step 2: select and arrange all relations to obtain a set dRels of injective relations
--          to ensure deletability of entities (signal relations are excluded)
     dRels = Set.elems $
              Set.filter isInj toconsider
                `Set.union`
             (Set.map flp . Set.filter (not.isInj) . Set.filter isUni $ toconsider)
       where toconsider = Set.map EDcD calculatedDecls
--  Step 3: compute longest sequences of total expressions and longest sequences of injective expressions.
     maxTotPaths,maxInjPaths :: [NE.NonEmpty Expression]
     maxTotPaths = map (NE.:|[]) cRels   -- note: instead of computing the longest sequence, we take sequences of length 1, the function clos1 below is too slow!
     maxInjPaths = map (NE.:|[]) dRels   -- note: instead of computing the longest sequence, we take sequences of length 1, the function clos1 below is too slow!
     --    Warshall's transitive closure algorithm, adapted for this purpose:
--     clos1 :: [Expression] -> [[Expression]]
--     clos1 xs
--      = foldl' f [ [ x ] | x<-xs] (nub (map source xs) `Set.intersection` nub (map target xs))
--        where
--          f :: [[Expression]] -> A_Concept -> [[Expression]]
--          f q x = q <> [l <> r | l <- q, x == target (last l),
--                                 r <- q, x == source (head r), null (l `Set.intersection` r)]

--  Step 4: i) generate interfaces starting with INTERFACE concept: I[Concept]
--          ii) generate interfaces starting with INTERFACE concepts: V[ONE*Concept]
--          note: based on a theme one can pick a certain set of generated interfaces (there is not one correct set)
--                default theme => generate interfaces from the clos total expressions and clos injective expressions (see step1-3).
--                student theme => generate interface for each concept with relations where concept is source or target (note: step1-3 are skipped)
     interfaceGen = step4a <> step4b
     step4a :: [Interface]
     step4a
      = let recur :: [NE.NonEmpty Expression] -> [ObjectDef]
            recur es = 
               [ ObjectDef
                     { objnm   = showA t
                     , objpos  = orig
                     , objExpression  = t
                     , objcrud = mostLiberalCruds env t (Right orig)
                     , objmView = Nothing
                     , objmsub = Just . Box orig (target t) (simpleBoxHeader orig) . map BxExpr $ recur [ PARTIAL.fromList pth | pth <-map NE.tail $ NE.toList cl, not (null pth) ]
                     }
               | cl<-eqCl NE.head es
               , let t = NE.head . NE.head $ cl
               ] 
               where orig = Origin "generated recur object: step 4a - default theme"
            -- es is a list of expression lists, each with at least one expression in it. They all have the same source concept (i.e. source.head)
            -- Each expression list represents a path from the origin of a box to the attribute.
            -- 16 Aug 2011: (recur es) is applied once where es originates from (maxTotPaths `Set.union` maxInjPaths) both based on clos
            -- Interfaces for I[Concept] are generated only for concepts that have been analysed to be an entity.
            -- These concepts are collected in gPlugConcepts
            gPlugConcepts :: [A_Concept]
            gPlugConcepts = [ c | InternalPlug plug@TblSQL{}<-genPlugs , (c,_)<-take 1 (cLkpTbl plug) ]
            -- Each interface gets all attributes that are required to create and delete the object.
            -- All total attributes must be included, because the interface must allow an object to be deleted.
            plugPaths :: [NE.NonEmpty Expression]
            plugPaths = [ pth | pth <- L.nub (maxTotPaths <> maxInjPaths)
                        , (source.NE.head) pth `elem` gPlugConcepts
                        ]
            f :: NE.NonEmpty (NE.NonEmpty Expression) -> Maybe (A_Concept,NE.NonEmpty ObjectDef)
            f cl = 
              case recur $ NE.toList cl of
                []   -> Nothing
                h:tl -> if isIdent (objExpression h) && null tl
                          then Nothing --exclude concept A without cRels or dRels (i.e. A in Scalar without total associations to other plugs)
                          else Just ( source  . NE.head . NE.head $ cl
                                    , h NE.:| tl
                                    )
        in
        [Ifc { ifcIsAPI    = False
             , ifcname     = name c
             , ifcObj      = let orig = Origin "generated object: step 4a - default theme" 
                                 expr = EDcI c
                             in ObjectDef
                                 { objnm   = name c
                                 , objpos  = orig
                                 , objExpression  = expr
                                 , objcrud = mostLiberalCruds env expr (Right orig)
                                 , objmView = Nothing
                                 , objmsub = Just . Box orig c (simpleBoxHeader orig) . map BxExpr $ NE.toList objattributes
                                 }
                               
             , ifcControls = makeIfcControls params allConjs
             , ifcPos      = Origin "generated interface: step 4a - default theme"
             , ifcPrp      = "Interface " <>name c<>" has been generated by Ampersand."
             , ifcRoles    = []
             }
        | (c, objattributes) <- mapMaybe f $ eqCl (source . NE.head) plugPaths
        , let params = bindedRelationsIn . expressionsIn $ objattributes
        ]
     --end otherwise: default theme
     --end stap4a
     step4b --generate lists of concept instances for those concepts that have a generated INTERFACE in step4a
      = [Ifc { ifcIsAPI    = False
             , ifcname     = nm
             , ifcObj      = let orig = Origin "generated object: step 4b"
                                 expr = EDcI ONE
                             in ObjectDef
                                 { objnm   = nm
                                 , objpos  = orig
                                 , objExpression  = expr
                                 , objcrud = mostLiberalCruds env expr (Right orig)
                                 , objmView = Nothing
                                 , objmsub = Just . Box orig ONE (simpleBoxHeader orig) $ [BxExpr att]
                                 }
             , ifcControls = ifcControls ifcc
             , ifcPos      = ifcPos      ifcc
             , ifcPrp      = ifcPrp      ifcc
             , ifcRoles    = []
             }
        | ifcc<-step4a
        , let c   = source(objExpression (ifcObj ifcc))
              nm'::Int->Text
              nm' 0  = plural (ctxlang context) (name c)
              nm' i  = plural (ctxlang context) (name c) <> tshow i
              nm = case [nm' i |i<-[0..], nm' i `notElem` map name (ctxifcs context)] of
                     []  -> fatal "impossible"
                     h:_ -> h
              att = let orig = Origin "generated attribute object: step 4b" 
                        expr = EDcV (Sign ONE c)
                    in ObjectDef
                        { objnm    = name c
                        , objpos   = orig
                        , objExpression   = expr
                        , objcrud  = mostLiberalCruds env expr (Right orig)
                        , objmView = Nothing
                        , objmsub  = Nothing
                        }
        ]
     ----------------------
     --END: making interfaces
     ----------------------

makeIfcControls :: Relations -> [Conjunct] -> [Conjunct]
makeIfcControls params allConjs
 = [ conj 
   | conj<-allConjs
   , (not.null) (Set.map EDcD params `Set.intersection` primsMentionedIn (rc_conjunct conj))
   -- Filtering for uni/inj invariants is pointless here, as we can only filter out those conjuncts for which all
   -- originating rules are uni/inj invariants. Conjuncts that also have other originating rules need to be included
   -- and the uni/inj invariant rules need to be filtered out at a later stage (in Generate.hs).
   ]
  

class Named a => Rename a where
 rename :: a->Text->a
 -- | the function uniqueNames ensures case-insensitive unique names like sql plug names
 uniqueNames :: [Text]->[a]->[a]
 uniqueNames taken xs
  = [p | cl<-eqCl (T.toLower.name) xs  -- each equivalence class cl contains (identified a) with the same map toLower (name p)
       , p <-if name (NE.head cl) `elem` taken || length cl>1
             then [rename p (name p<>tshow i) | (p,i)<-zip (NE.toList cl) [(1::Int)..]]
             else NE.toList cl
    ]

instance Rename PlugSQL where
 rename p x = p{sqlname=x}
     

tblcontents :: ContextInfo -> [Population] -> PlugSQL -> [[Maybe AAtomValue]]
tblcontents ci ps plug
   = case plug of
     BinSQL{}    -> let expr = case dLkpTbl plug of
                                 [store] -> if rsStoredFlipped store
                                            then EFlp . EDcD . rsDcl $ store
                                            else        EDcD . rsDcl $ store
                                 ss       -> fatal ("Exactly one relation sould be stored in BinSQL. However, there are "<>tshow (length ss))
                    in [[(Just . apLeft) p,(Just . apRight) p] |p<-Set.elems $ fullContents ci ps expr]
     TblSQL{}    -> 
 --TODO15122010 -> remove the assumptions (see comment data PlugSQL)
 --attributes are assumed to be in the order kernel+other,
 --where NULL in a kernel attribute implies NULL in the following kernel attributes
 --and the first attribute is unique and not null
 --(r,s,t)<-mLkpTbl: s is assumed to be in the kernel, attExpr t is expected to hold r or (flp r), s and t are assumed to be different
        case attributes plug of 
         []   -> fatal "no attributes in plug."
         f:fs -> (L.nub . L.transpose)
                 ( map Just (Set.elems cAtoms)
                 : [case fExp of
                       EDcI c -> [ if a `elem` atomValuesOf ci ps c then Just a else Nothing | a<-Set.elems cAtoms ]
                       _      -> [ (lkp att a . fullContents ci ps) fExp | a<-Set.elems cAtoms ]
                   | att<-fs, let fExp=attExpr att
                   ]
                 )
                 where
                   cAtoms = (atomValuesOf ci ps. source . attExpr) f
                   lkp :: SqlAttribute -> AAtomValue -> AAtomPairs -> Maybe AAtomValue
                   lkp att a pairs
                    = case [ p | p<-Set.elems pairs, a==apLeft p ] of
                       [] -> Nothing
                       [p] -> Just (apRight p)
                       ps' -> fatal . T.unlines $ 
                                [ "There is an attempt to populate multiple values into "
                                , "     the row of table `"<>name plug<>"`, where id = "<>tshow(showValADL a)<>":"
                                , "     Values to be inserted in field `"<>name att<>"` are: "<>tshow (map (showValADL . apRight) ps')
                                ] --this has happend before due to:
                                  --    when using --dev flag
                                  --  , when there are violations
                                  --  , when you have INCLUDE \"MinimalAST.xlsx\" in formalampersand.)
                                  --  , when a relation in formalAmpersand is declared UNI, but actually it isn't.

-- convenient function to give a Box header without keyvalues
simpleBoxHeader :: Origin -> HTMLTemplateUsage
simpleBoxHeader orig = HTMLTemplateUsage {pos = orig, btType = "ROWS", btKeys = []}
