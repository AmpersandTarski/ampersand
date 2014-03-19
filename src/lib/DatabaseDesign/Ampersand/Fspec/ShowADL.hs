{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
  -- The purpose of ShowADL is to print things in Ampersand source format.
  -- Rule: The semantics of each fSpec produced by the compiler is identical to the semantics  of (parse (showADL fSpec)).
  -- Rule: The standard show is used only for simple error messages during testing.
  -- Question (SJC): If STRING is the code produced by showADL, would STRING == showADL (parse STRING) (context (parse STRING)) be true?
  -- Answer (SJ):   No, not for every STRING. Yet, for every fSpec we want  semantics fSpec == semantics (parse (showADL fSpec)).
  --                Note that 'parse' and 'semantics' do not exist in this shape, so the actual expression is slightly more complicated.
  --
  -- Every Expression should be disambiguated before printing to ensure unambiguity.
module DatabaseDesign.Ampersand.Fspec.ShowADL
    ( ShowADL(..), LanguageDependent(..), exprIsc2list, exprUni2list, exprCps2list, exprRad2list, exprPrd2list)
where
import DatabaseDesign.Ampersand.Core.ParseTree
import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree
import DatabaseDesign.Ampersand.Basics      (fatalMsg,eqCl,Collection(..),Identified(..))
import DatabaseDesign.Ampersand.Classes
import DatabaseDesign.Ampersand.Fspec.Fspec
import Data.List hiding (head)
import Prelude hiding (head)
--import Debug.Trace

head :: [a] -> a
head [] = fatal 30 "head must not be used on an empty list!"
head (a:_) = a


fatal :: Int -> String -> a
fatal = fatalMsg "Fspec.ShowADL"

exprIsc2list, exprUni2list, exprCps2list, exprRad2list, exprPrd2list :: Expression -> [Expression]
exprIsc2list (EIsc (l,r)) = exprIsc2list l++exprIsc2list r
exprIsc2list r            = [r]
exprUni2list (EUni (l,r)) = exprUni2list l++exprUni2list r
exprUni2list r            = [r]
exprCps2list (ECps (l,r)) = exprCps2list l++exprCps2list r
exprCps2list r            = [r]
exprRad2list (ERad (l,r)) = exprRad2list l++exprRad2list r
exprRad2list r            = [r]
exprPrd2list (EPrd (l,r)) = exprPrd2list l++exprPrd2list r
exprPrd2list r            = [r]

class ShowADL a where
 showADL :: a -> String

-- there are data types yielding language dependent data like an Expression
-- the LanguageDependent class provides function(s) to map language dependent functions on those data if any.
-- for example to print all expressions in a data structure with a practical amount of type directives
-- (instances have been created when needed)
--
-- LanguageDependent is part of ShowAdl because the only application at time of writing is to disambiguate expressions for the purpose of printing
-- SJ 31-12-2012: Since 'disambiguate' has become obsolete, do we still need this?
class LanguageDependent a where
  mapexprs :: (Language l, ConceptStructure l, Identified l) => (l -> Expression -> Expression) -> l -> a -> a
  mapexprs _ _ = id

instance LanguageDependent a => LanguageDependent (Maybe a) where
  mapexprs _ _ Nothing  = Nothing
  mapexprs f l (Just x) = Just $ mapexprs f l x

instance LanguageDependent (a, Expression) where
  mapexprs f l (x,e) = (x, f l e)
instance LanguageDependent Rule where
  mapexprs f l rul = rul{rrexp = f l (rrexp rul)}
instance LanguageDependent Interface where
  mapexprs f l ifc = ifc{ifcObj = mapexprs f l (ifcObj ifc)}
instance LanguageDependent ObjectDef where
  mapexprs f l obj = obj{objctx = f l (objctx obj), objmsub = mapexprs f l $ objmsub obj}
instance LanguageDependent SubInterface where
  mapexprs _ _ iref@(InterfaceRef _) = iref
  mapexprs f l (Box o objs) = Box o $ map (mapexprs f l) objs
instance LanguageDependent Declaration where
  mapexprs _ _ = id
instance LanguageDependent ECArule where
  mapexprs _ _ = id
instance LanguageDependent Event where
  mapexprs _ _ = id
--------------------------------------------------------------
instance ShowADL (P_SubIfc a) where
  showADL (P_Box{}) = "BOX"
  showADL (P_InterfaceRef _ nm) = " INTERFACE "++showstr nm
  

instance ShowADL ObjectDef where
-- WHY (HJ)? In deze instance van ShowADL worden diverse zaken gebruikt die ik hier niet zou verwachten.
--              Het vertroebelt de code ook een beetje, want nu moeten er dingen als 'source' en
--              'target' hier al bekend zijn.
--              Dat lijkt me hier nog niet op z'n plaats, als je alleen maar wat wilt kunnen 'prettyprinten'. 
-- BECAUSE (SJ): Dit blijft nog even zo, omdat showADL gebruikt wordt in het genereren van interfaces.
--              Zolang we dat nog niet onder de knie hebben blijft de code wat troebel.
 showADL obj = " : "++showADL (objctx obj)++
               recur "\n  " (objmsub obj)
  where recur :: String -> Maybe SubInterface -> String
        recur _   Nothing = ""
        recur ind (Just (InterfaceRef nm)) = ind++" INTERFACE "++showstr nm
        recur ind (Just (Box _ objs))
         = ind++" BOX [ "++
           intercalate (ind++"     , ") 
                               [ showstr (name o)++
                                  (if null (objstrs o) then "" else " {"++intercalate ", " [showstr (unwords ss) | ss<-objstrs o]++"}")++
                                  " : "++showADL (objctx o)++
                                  recur (ind++"      ") (objmsub o)
                               | o<-objs
                               ]++
           ind++"     ]"

instance ShowADL Meta where
 showADL (Meta _ metaObj nm val) = 
   "META "++(if null $ showADL metaObj then "" else showADL metaObj++" ") ++show nm++" "++show val

instance ShowADL MetaObj where
 showADL ContextMeta = ""
 
instance ShowADL Purpose where
 showADL expl = "PURPOSE "++showADL (explObj expl)
                ++showADL (amLang (explMarkup expl))
                ++showADL (amFormat (explMarkup expl))
                ++(if null (explRefIds expl) then "" else " REF "++intercalate ";" (explRefIds expl))
                ++ "{+"++aMarkup2String (explMarkup expl)++"-}"

instance ShowADL PandocFormat where
 showADL LaTeX = "LATEX"
 showADL HTML  = "HTML"
 showADL ReST  = "REST"
 showADL Markdown = "MARKDOWN"
 
instance ShowADL A_Markup where
 showADL m 
     = "IN " ++showADL (amLang m) 
    ++ " "++showADL (amFormat m) 
    ++ "{+"++aMarkup2String m++"-}"
    
instance ShowADL Lang where
 showADL Dutch   = "DUTCH"
 showADL English = "ENGLISH"
   
--instance ShowADL (Maybe Lang) where
-- showADL  Nothing       = "IN DUTCH"
-- showADL (Just Dutch  ) = "IN DUTCH"
-- showADL (Just English) = "IN ENGLISH"
   
instance ShowADL ExplObj where
 showADL e = case e of
      ExplConceptDef cd  -> "CONCEPT "++cdcpt cd
      ExplDeclaration d  -> "RELATION "++show (name d)
      ExplRule str       -> "RULE "++showstr str
      ExplIdentityDef str-> "IDENT "++showstr str
      ExplViewDef str    -> "VIEW "++showstr str
      ExplPattern str    -> "PATTERN "++ showstr str
      ExplProcess str    -> "PROCESS "++str
      ExplInterface str  -> "INTERFACE "++showstr str
      ExplContext str    -> "CONTEXT "++showstr str

showstr :: String -> String
showstr str = "\""++str++"\""

-- The declarations of the pattern are supplemented by all declarations needed to define the rules.
-- Thus, the resulting pattern is self-contained with respect to declarations.
instance ShowADL Process where
 showADL prc
  = "PROCESS " ++ name prc 
    ++ (if null (udefrules prc) then "" else "\n  " ++intercalate "\n  " (map showADL (udefrules prc)) ++ "\n")
    ++ (if null (maintains prc) then "" else "\n  " ++                        showRM prc               ++ "\n")
    ++ (if null (mayEdit prc)   then "" else "\n  " ++                        showRR prc               ++ "\n")
-- concept definitions are not printed, because we have no way of telling where they come from.... 
    ++ (if null (prcIds prc)    then "" else "\n  " ++intercalate "\n  " (map showADL (prcIds prc))    ++ "\n")
    ++ (if null (prcXps prc)    then "" else "\n  " ++intercalate "\n  " (map showADL (prcXps prc))    ++ "\n")
    ++ "ENDPROCESS"
    where -- TODO: the following definitions should be unneccessary, but 'map showADL (maintains prc)' and "map showADL (mayEdit prc)" don't work... 
      showRM :: Process -> String
      showRM pr = intercalate "\n  " [ "ROLE "++role++" MAINTAINS "++intercalate ", " [name rul | (_,rul)<-cl]
                                     | cl<-eqCl fst (maintains pr), let role = fst (head cl)]
      showRR :: Process -> String
      showRR pr = intercalate "\n  " [ "ROLE "++role++" EDITS "++intercalate ", " [name rul | (_,rul)<-cl]
                                     | cl<-eqCl fst (mayEdit pr), let role = fst (head cl)]

instance ShowADL (String,Rule) where
 showADL (role,rul) = "ROLE "++role++" MAINTAINS "++show (name rul)

instance ShowADL (String,Declaration) where
 showADL           (role,rel) = "ROLE "++role++" EDITS "++showADL rel

instance ShowADL (String,Interface) where
 showADL (role,ifc) = "ROLE "++role++" USES "++show (name ifc)
 
instance ShowADL Pattern where
 showADL pat
  = "PATTERN " ++ showstr (name pat) ++ "\n"
    ++ (if null (ptrls pat)  then "" else "\n  " ++intercalate "\n  " (map showADL (ptrls pat)) ++ "\n")
    ++ (if null (maintains pat) then "" else "\n  " ++                        showRM pat               ++ "\n")
    ++ (if null (mayEdit pat)   then "" else "\n  " ++                        showRR pat               ++ "\n")
    ++ (if null (ptgns pat)  then "" else "\n  " ++intercalate "\n  " (map showADL (ptgns pat)) ++ "\n")
    ++ (if null (ptdcs pat)  then "" else "\n  " ++intercalate "\n  " (map showADL (ptdcs pat)) ++ "\n")
-- concept definitions are not printed, because we have no way of telling where they come from.... 
    ++ (if null (ptids pat)  then "" else "\n  " ++intercalate "\n  " (map showADL (ptids pat)) ++ "\n")
    ++ "ENDPATTERN"
    where -- TODO: the following definitions should be unneccessary, but 'map showADL (maintains prc)' and "map showADL (mayEdit prc)" don't work... 
      showRM :: Pattern -> String
      showRM pt = intercalate "\n  " [ "ROLE "++role++" MAINTAINS "++intercalate ", " [name rul | (_,rul)<-cl]
                                      | cl<-eqCl fst (maintains pt), let role = fst (head cl)]
      showRR :: Pattern -> String
      showRR pt = intercalate "\n  " [ "ROLE "++role++" EDITS "++intercalate ", " [name rul | (_,rul)<-cl]
                                      | cl<-eqCl fst (mayEdit pt), let role = fst (head cl)]

instance ShowADL (PairViewSegment Expression) where
 showADL (PairViewText str)         = "TXT " ++ show str
 showADL (PairViewExp srcOrTgt e) = showADL srcOrTgt ++ " " ++ showADL e

instance ShowADL SrcOrTgt where
 showADL Src = "source"
 showADL Tgt = "target"
 
--showADLSrcOrTgt :: SrcOrTgt -> String
--showADLSrcOrTgt Src = "SRC"
--showADLSrcOrTgt Tgt = "TGT"
        
instance ShowADL Rule where
 showADL r
  = "RULE \""++rrnm r++"\" : "++showADL (rrexp r)
     ++ concat ["\n     MEANING "++showADL mng | mng <- ameaMrk $ rrmean r ]
     ++ concat ["\n     MESSAGE "++showADL msg | msg <- rrmsg r]
     ++ case rrviol r of
          Nothing                -> ""
          Just (PairView pvSegs) -> "\n     VIOLATION ("++intercalate ", " (map showADL pvSegs)++")"
       
instance ShowADL A_Gen where
 showADL g =
   case g of
    Isa{} -> "CLASSIFY "++showADL (genspc g)++" ISA "++showADL (gengen g)
    IsE{} -> "CLASSIFY "++showADL (genspc g)++" IS "++intercalate " /\\ " (map showADL (genrhs g))

instance ShowADL RoleRelation where
 showADL r
  = "ROLE "++intercalate ", " (map show (rrRoles r))++" EDITS "++intercalate ", " (map showADL (rrRels r))

instance ShowADL RoleRule where
 showADL r = "ROLE "++intercalate ", " (map show (mRoles r))++" MAINTAINS "++intercalate ", " (map show (mRules r))

instance ShowADL Interface where
 showADL ifc 
  = "INTERFACE "++showstr(name ifc)
          ++(if null (ifcParams ifc) then "" else "("++intercalate ", " [showADL r | r<-ifcParams ifc]++")")
          ++(if null (ifcArgs ifc) then "" else "{"++intercalate ", " [showstr(unwords strs) | strs<-ifcArgs ifc]++"}")
          ++(if null (ifcRoles ifc) then "" else " FOR "++intercalate ", " [ r | r<-ifcRoles ifc])
          ++showADL (ifcObj ifc)

instance ShowADL IdentityDef where
 showADL identity 
  = "IDENT "++idLbl identity
          ++ ": " ++name (idCpt identity)
          ++ "(" ++intercalate ", " (map showADL $ identityAts identity) ++ ")"

instance ShowADL IdentitySegment where
 showADL (IdentityExp objDef) = (if null (name objDef) then "" else "\""++name objDef++"\":") ++ showADL (objctx objDef)

instance ShowADL ViewDef where
 showADL vd 
  = "VIEW "++vdlbl vd
          ++ ": " ++name (vdcpt vd)
          ++ "(" ++intercalate ", " (map showADL $ vdats vd) ++ ")"

instance ShowADL ViewSegment where
 showADL (ViewExp objDef) = (if null (name objDef) then "" else "\""++name objDef++"\":") ++ showADL (objctx objDef)
 showADL (ViewText str) = "TXT " ++ show str
 showADL (ViewHtml str) = "PRIMHTML " ++ show str

-- showADL Relation only prints complete signatures to ensure unambiguity.
-- therefore, when printing expressions, do not apply this function to print relations, but apply one that prints names only
--instance ShowADL Relation where
-- showADL rel = show rel

instance ShowADL Expression where
 showADL = showExpr (" = ", " |- ", " /\\ ", " \\/ ", " - ", " / ", " \\ ", ";", "!", "*", "*", "+", "~", ("-"++), "(", ")", "[", "*", "]") . insParentheses
-- NOTE: retain space after \\, because of unexpected side effects if it is used just before an 'r' or 'n'....
   where 
     showExpr :: (String,String,String,String,String,String,String,String,String,String,String,String,String,String -> String,String,String,String,String,String)
            -> Expression -> String
     showExpr    (equi,  impl,  inter, union',diff,  lresi, rresi, rMul  , rAdd , rPrd ,closK0,closK1,flp',  compl,           lpar,  rpar,  lbr,   star,  rbr)  expr
      = showchar (insA_Parentheses expr)
        where
          showchar (EEqu (l,r)) = showchar l++equi++showchar r
          showchar (EImp (l,r)) = showchar l++impl++showchar r
          showchar (EIsc (l,r)) = showchar l++inter++showchar r
          showchar (EUni (l,r)) = showchar l++union'++showchar r
          showchar (EDif (l,r)) = showchar l++diff ++showchar r
          showchar (ELrs (l,r)) = showchar l++lresi++showchar r
          showchar (ERrs (l,r)) = showchar l++rresi++showchar r
{-          showchar (ECps (EEps i sgn,r)) | i==source sgn||i==target sgn = showchar  r
                                         | otherwise                    = showchar (ECps (EDcI i,r))
          showchar (ECps (l,EEps i sgn)) | i==source sgn||i==target sgn = showchar  l
                                         | otherwise                    = showchar (ECps (l,EDcI i)) -}
          showchar (ECps (l,r)) = showchar l++rMul++showchar r
          showchar (ERad (l,r)) = showchar l++rAdd++showchar r
          showchar (EPrd (l,r)) = showchar l++rPrd++showchar r
          showchar (EKl0 e)     = showchar e++closK0
          showchar (EKl1 e)     = showchar e++closK1
          showchar (EFlp e)     = showchar e++flp'
          showchar (ECpl e)     = compl (showchar e)
          showchar (EBrk e)     = lpar++showchar e++rpar
          showchar (EDcD dcl)   = name dcl
          showchar (EDcI c)     = "I"++lbr++name c++rbr
          showchar (EEps i _)   = "Eps{"++name i++"}"
          showchar (EDcV sgn)   = "V"++lbr++name (source sgn)++star++name (target sgn)++rbr
          showchar (EMp1 a c)   = "'"++a++"'"++lbr++name c++rbr

     insA_Parentheses :: Expression -> Expression
     insA_Parentheses = insPar 0
        where
          wrap :: Integer -> Integer -> Expression -> Expression
          wrap i j e' = if i<=j then e' else EBrk (insPar 0 e')
          insPar :: Integer -> Expression -> Expression
          insPar i  (EEqu (l,r)) = wrap i 0 (EEqu (insPar 1 l, insPar 1 r))
          insPar i  (EImp (l,r)) = wrap i 0 (EImp (insPar 1 l, insPar 1 r))
          insPar i x@EIsc{}      = wrap i 2 (foldr1 f [insPar 3 e | e<-exprIsc2list x ]) where f y z = EIsc (y,z)
          insPar i x@EUni{}      = wrap i 2 (foldr1 f [insPar 3 e | e<-exprUni2list x ]) where f y z = EUni (y,z)
          insPar i  (EDif (l,r)) = wrap i 4 (EDif (insPar 5 l, insPar 5 r))
          insPar i  (ELrs (l,r)) = wrap i 6 (ELrs (insPar 7 l, insPar 7 r))
          insPar i  (ERrs (l,r)) = wrap i 6 (ERrs (insPar 7 l, insPar 7 r))
          insPar i x@ECps{}      = wrap i 8 (foldr1 f [insPar 9 e | e<-exprCps2list x ]) where f y z = ECps (y,z)
          insPar i x@ERad{}      = wrap i 8 (foldr1 f [insPar 9 e | e<-exprRad2list x ]) where f y z = ERad (y,z)
          insPar i x@EPrd{}      = wrap i 8 (foldr1 f [insPar 9 e | e<-exprPrd2list x ]) where f y z = EPrd (y,z)
          insPar _  (EKl0 e)     = EKl0 (insPar 10 e)
          insPar _  (EKl1 e)     = EKl1 (insPar 10 e)
          insPar _  (EFlp e)     = EFlp (insPar 10 e)
          insPar _  (ECpl e)     = ECpl (insPar 10 e)
          insPar i  (EBrk e)     = insPar i e
          insPar _  x            = x

{-
   insPar 0 (r/\s/\t/\x/\y|-p)
=
   wrap 0 0 (insPar 1 (r/\s/\t/\x/\y) |- insPar 1 p)
=
   insPar 1 (r/\s/\t/\x/\y) |- insPar 1 p
=
   wrap 1 2 (foldr1 f [insPar 3 e | e<-exprIsc2list (r/\s/\t/\x/\y) ]) |- p   where f x y = EIsc (x,y)
=
   foldr1 f [insPar 3 e | e<-exprIsc2list (r/\s/\t/\x/\y) ] |- p   where f x y = EIsc (x,y)
=
   foldr1 f [insPar 3 e | e<-[r,s,t,x,y] ] |- p   where f x y = EIsc (x,y)
=
   foldr1 f [insPar 3 r,insPar 3 s,insPar 3 t,insPar 3 x,insPar 3 y] |- p   where f x y = EIsc (x,y)
=
   foldr1 f [r,s,t,x,y] |- p   where f x y = EIsc (x,y)
=
   r/\s/\t/\x/\y |- p



   insPar 0 (r;s;t;x;y|-p)
=
   wrap 0 0 (insPar 1 (r;s;t;x;y) |- insPar 1 p)
=
   insPar 1 (r;s;t;x;y) |- insPar 1 p
=
   wrap 1 8 (insPar 8 r ; insPar 8 (s;t;x;y)) |- p
=
   r; insPar 8 (s;t;x;y) |- p
=
   r; wrap 8 8 (insPar 8 s; insPar 8 (t;x;y)) |- p
=
   r; insPar 8 s; insPar 8 (t;x;y) |- p
=
   r; s; insPar 8 (t;x;y) |- p
-}

instance ShowADL DnfClause where
 showADL dnfClause = showADL (dnf2expr dnfClause)

instance ShowADL Declaration where
 showADL decl = 
  case decl of
     Sgn{} -> name decl++" :: "++name (source decl)++(if null ([Uni,Tot]>-multiplicities decl) then " -> " else " * ")++name (target decl)++
              (let mults=if null ([Uni,Tot]>-multiplicities decl) then multiplicities decl>-[Uni,Tot] else multiplicities decl in
               if null mults then "" else "["++intercalate "," (map showADL mults)++"]")++
              (if null(decprL decl++decprM decl++decprR decl) then "" else
               " PRAGMA "++unwords (map show [decprL decl,decprM decl,decprR decl]))
               ++ concatMap meaning (ameaMrk (decMean decl))
               ++ maybe "" (\(RelConceptDef srcOrTgt def) -> " DEFINE "++showADL srcOrTgt ++ " " ++ def) (decConceptDef decl)
     Isn{} -> "I["++show (detyp decl)++"]" -- Isn{} is of type Declaration and it is implicitly defined
     Vs{}  -> "V"++show (decsgn decl)      -- Vs{}  is of type Declaration and it is implicitly defined
   where
     meaning :: A_Markup -> String
     meaning pmkup = " MEANING "++showADL pmkup

instance ShowADL P_Markup where
 showADL (P_Markup lng fmt str) = case lng of
                                     Nothing -> ""
                                     Just l  -> "IN "++show l++" "
                                 ++
                                  case fmt of
                                     Nothing -> ""
                                     Just f  -> " "++show f++" "
                                 ++
                                  "{+"++str++"-} "
 
instance ShowADL Prop where
 showADL = show

instance ShowADL A_Concept where
 showADL c = show (name c)

instance ShowADL ConceptDef where
 showADL cd
  = "\n  CONCEPT "++show (cdcpt cd)++" "++show (cddef cd)++" "++(if null (cdref cd) then "" else show (cdref cd))

instance ShowADL A_Context where
 showADL context
  = "CONTEXT " ++name context
    ++ (if null (ctxmetas context) then "" else "\n"      ++intercalate "\n\n" (map showADL (ctxmetas context))++ "\n")
    ++ (if null (ctxprocs context) then "" else "\n"      ++intercalate "\n\n" (map showADL (ctxprocs context))++ "\n") -- All processes
    ++ (if null (ctxifcs context)  then "" else "\n"      ++intercalate "\n\n" (map showADL (ctxifcs context)) ++ "\n")
    ++ (if null (ctxpats context)  then "" else "\n"      ++intercalate "\n\n" (map showADL (ctxpats context)) ++ "\n")
    ++ (if null (ctxrs context)    then "" else "\n"      ++intercalate "\n"   (map showADL (ctxrs context))   ++ "\n")
    ++ (if null (ctxds context)    then "" else "\n"      ++intercalate "\n"   (map showADL (ctxds context))   ++ "\n")
    ++ (if null (ctxks context)    then "" else "\n"      ++intercalate "\n"   (map showADL (ctxks context))   ++ "\n")
    ++ (if null (ctxgs context)    then "" else "\n"      ++intercalate "\n"   (map showADL (ctxgs context))   ++ "\n")
    ++ (if null (ctxcds context)   then "" else "\n"      ++intercalate "\n"   (map showADL (ctxcds context))  ++ "\n")
    ++ (if null (ctxpopus context) then "" else "\n"      ++intercalate "\n"   (map showADL (ctxpopus context))++ "\n")
    ++ (if null (ctxsql context)   then "" else "\n"      ++intercalate "\n\n" (map showADL (ctxsql context)) ++ "\n")
    ++ (if null (ctxphp context)   then "" else "\n"      ++intercalate "\n\n" (map showADL (ctxphp context)) ++ "\n")
    ++ "\n\nENDCONTEXT"

instance ShowADL Fspc where
 showADL fSpec
  = "CONTEXT " ++name fSpec
    ++ (if null (map ifcObj [] {- map fsv_ifcdef (fActivities fSpec) -})     
        then "" 
        else "\n"++intercalate "\n\n" (map (showADL . ifcObj) [] {- map fsv_ifcdef (fActivities fSpec) -})     ++ "\n")
    ++ (if null (metas fSpec)    then "" else "\n"++intercalate "\n\n" (map showADL (metas fSpec))    ++ "\n")
    ++ (if null (patterns fSpec)    then "" else "\n"++intercalate "\n\n" (map showADL (patterns fSpec))    ++ "\n")
    ++ (if null (conceptDefs fSpec) then "" else "\n"++intercalate "\n"   (map showADL (conceptDefs fSpec)) ++ "\n")
    ++ (if null (gens fSpec) then "" else "\n"++intercalate "\n"   (map showADL (gens fSpec)) ++ "\n")
    ++ (if null (identities fSpec)       then "" else "\n"++intercalate "\n"   (map showADL (identities fSpec >- concatMap identities (patterns fSpec)))       ++ "\n")
    ++ (if null (declarations fSpec) then "" else "\n"++intercalate "\n"   (map showADL (declarations fSpec >- concatMap declarations (patterns fSpec))) ++ "\n")
    ++ (if null (udefrules fSpec) then "" else "\n"++intercalate "\n"   (map showADL (udefrules fSpec >- concatMap udefrules (patterns fSpec))) ++ "\n")
    ++ (if null (fSexpls fSpec) then "" else "\n"++intercalate "\n"   (map showADL (fSexpls fSpec)) ++ "\n")
    ++ "TODO: Populations are not shown..\n" --TODO.
--    ++ (if null showADLpops         then "" else "\n"++intercalate "\n\n" showADLpops                                    ++ "\n")
    ++ (if null (interfaceS fSpec)    then "" else "\n"++intercalate "\n\n" (map showADL (interfaceS fSpec))    ++ "\n")
    ++ "\n\nENDCONTEXT"
instance ShowADL (Maybe String) where
  showADL _ = ""
instance ShowADL ECArule where
  showADL eca = "ECA #"++show (ecaNum eca)
instance ShowADL Event where
  showADL = show
instance (ShowADL a, ShowADL b) => ShowADL (a,b) where
 showADL (a,b) = "(" ++ showADL a ++ ", " ++ showADL b ++ ")"

instance ShowADL P_Population where
 showADL pop
  = "POPULATION "++name pop
  ++ case pop of
        P_TRelPop{} -> "["++(name.pSrc.p_type) pop++"*"++(name.pTgt.p_type) pop++"]"
        _ -> ""
  ++ " CONTAINS\n"
  ++ if (case pop of
            P_RelPopu{} -> null (p_popps pop)
            P_TRelPop{} -> null (p_popps pop)
            P_CptPopu{} -> null (p_popas pop)
        ) 
     then "" 
     else indent++"[ "++intercalate ("\n"++indent++", ") showContent++indent++"]"
    where indent = "   "
          showContent = case pop of
                          P_RelPopu{} -> map showPaire (p_popps pop)
                          P_TRelPop{} -> map showPaire (p_popps pop)
                          P_CptPopu{} -> map showAtom  (p_popas pop)
showPaire :: Paire -> String
showPaire p = showAtom (srcPaire p)++" * "++ showAtom (trgPaire p)

instance ShowADL Population where
 showADL pop
  = "POPULATION "
  ++ case pop of
        PRelPopu{} -> (name.popdcl) pop++(show.sign.popdcl) pop
        PCptPopu{} -> (name.popcpt) pop
  ++ " CONTAINS\n"
  ++ if (case pop of
            PRelPopu{} -> null (popps pop)
            PCptPopu{} -> null (popas pop)
        ) 
     then "" 
     else indent++"[ "++intercalate ("\n"++indent++", ") showContent++indent++"]"
    where indent = "   "
          showContent = case pop of
                          PRelPopu{} -> map showPaire (popps pop)
                          PCptPopu{} -> map showAtom  (popas pop)


-- showADL (PRelPopu r pairs)
--  = "POPULATION "++showADL r++" CONTAINS\n"++
--    indent++"[ "++intercalate ("\n"++indent++", ") (map (\(x,y)-> showatom x++" * "++ showatom y) pairs)++indent++"]"
--    where indent = "   "


showAtom :: String -> String
showAtom x = "'"++[if c=='\'' then '`' else c|c<-x]++"'"              

instance ShowADL TermPrim where
 showADL (PI _)                                   = "I"
 showADL (Pid _ c)                                = "I["++showADL c++"]"
 showADL (Patm _ a Nothing)                       = "'"++a++"'"
 showADL (Patm _ a (Just c))                      = "'"++a++"'["++show c++"]"
 showADL (PVee _)                                 = "V"
 showADL (Pfull _ s t)                            = "V["++show s++"*"++show t++"]"
 showADL (Prel _ rel)                             = rel
 showADL (PTrel _ rel psgn)                       = rel++showsign psgn
  where     showsign (P_Sign src trg)                         = "["++showADL src++"*"++showADL trg++"]"


--used to compose error messages at p2a time
instance (ShowADL a, Traced a) => ShowADL (Term a) where
 showADL = showPExpr (" = ", " |- ", " /\\ ", " \\/ ", " - ", " / ", " \\ ", ";", "!", "*", "*", "+", "~", "(", ")")
   where
    showPExpr (equi,impl,inter,union',diff,lresi,rresi,rMul,rAdd,rPrd,closK0,closK1,flp',lpar,rpar) expr
     = showchar (insP_Parentheses expr)
      where
       showchar (Prim a) = showADL a
       showchar (Pequ _ l r)                             = showchar l++equi++showchar r
       showchar (Pimp _ l r)                             = showchar l++impl++showchar r
       showchar (PIsc _ l r)                             = showchar l++inter++showchar r
       showchar (PUni _ l r)                             = showchar l++union'++showchar r
       showchar (PDif _ l r)                             = showchar l++diff ++showchar r
       showchar (PLrs _ l r)                             = showchar l++lresi++showchar r
       showchar (PRrs _ l r)                             = showchar l++rresi++showchar r
       showchar (PCps _ l r)                             = showchar l++rMul++showchar r
       showchar (PRad _ l r)                             = showchar l++rAdd++showchar r
       showchar (PPrd _ l r)                             = showchar l++rPrd++showchar r
       showchar (PKl0 _ e)                               = showchar e++closK0
       showchar (PKl1 _ e)                               = showchar e++closK1
       showchar (PFlp _ e)                               = showchar e++flp'
       showchar (PCpl _ e)                               = '-':showchar e
       showchar (PBrk _ e)                               = lpar++showchar e++rpar

insP_Parentheses :: (Traced a) => Term a -> Term a
insP_Parentheses = insPar 0
      where
       wrap :: (Traced a) => Integer -> Integer -> Term a -> Term a
       wrap i j e' = if i<=j then e' else PBrk (origin e') e'
       insPar :: (Traced a) => Integer -> Term a -> Term a
       insPar i (Pequ o l r) = wrap i     0 (Pequ o (insPar 1 l) (insPar 1 r))
       insPar i (Pimp o l r) = wrap i     0 (Pimp o (insPar 1 l) (insPar 1 r))
       insPar i (PIsc o l r) = wrap (i+1) 2 (PIsc o (insPar 2 l) (insPar 2 r))
       insPar i (PUni o l r) = wrap (i+1) 2 (PUni o (insPar 2 l) (insPar 2 r))
       insPar i (PDif o l r) = wrap i     4 (PDif o (insPar 5 l) (insPar 5 r))
       insPar i (PLrs o l r) = wrap i     6 (PLrs o (insPar 7 l) (insPar 7 r))
       insPar i (PRrs o l r) = wrap i     6 (PRrs o (insPar 7 l) (insPar 7 r))
       insPar i (PCps o l r) = wrap (i+1) 8 (PCps o (insPar 8 l) (insPar 8 r))
       insPar i (PRad o l r) = wrap (i+1) 8 (PRad o (insPar 8 l) (insPar 8 r))
       insPar i (PPrd o l r) = wrap (i+1) 8 (PPrd o (insPar 8 l) (insPar 8 r))
       insPar _ (PKl0 o e)   = PKl0 o (insPar 10 e)
       insPar _ (PKl1 o e)   = PKl1 o (insPar 10 e)
       insPar _ (PFlp o e)   = PFlp o (insPar 10 e)
       insPar _ (PCpl o e)   = PCpl o (insPar 10 e)
       insPar i (PBrk _ e)   = insPar i e
       insPar _ x            = x


--used to compose error messages at p2a time
instance ShowADL P_Concept where
 showADL = name

instance ShowADL PAclause where
    showADL = showPAclause "\n "
     where
      showPAclause indent pa@CHC{}
       = let indent'=indent++"   " in "execute ONE from"++indent'++intercalate indent' [showPAclause indent' p' | p'<-paCls pa]
      showPAclause indent pa@ALL{}
       = let indent'=indent++"   " in "execute ALL of"++indent'++intercalate indent' [showPAclause indent' p' | p'<-paCls pa]
      showPAclause indent pa@Do{}
       = ( case paSrt pa of
            Ins -> "INSERT INTO "
            Del -> "DELETE FROM ")++
         show (paTo pa)++
         " SELECTFROM "++
         show (paDelta pa)
         ++concat [ indent++showConj rs | (_,rs)<-paMotiv pa ]
      showPAclause indent pa@Sel{}
       = let indent'=indent++"   " in
        "SELECT x:"++show (paCpt pa)++" FROM "++showADL (paExp pa)++";"++indent'++showPAclause indent' (paCl pa "x")
      showPAclause indent pa@New{}
       = let indent'=indent++"   " in
        "CREATE x:"++show (paCpt pa)++";"++indent'++showPAclause indent' (paCl pa "x")
      showPAclause indent pa@Rmv{}
       = let indent'=indent++"   " in
        "REMOVE x:"++show (paCpt pa)++";"++indent'++showPAclause indent' (paCl pa "x")
      showPAclause _ Nop{} = "Nop"
      showPAclause _ Blk{} = "Blk"
      showPAclause _ (Let _ _ _) = fatal 390 "showPAclause not defined for `Let`. Consult your dealer!"
      showPAclause _ (Ref _)     = fatal 391 "showPAclause not defined for `Ref`. Consult your dealer!"
      showConj rs
              = "(TO MAINTAIN"++intercalate ", " [name r | r<-rs]++")"
