{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Ampersand.ADL1.PrettyPrinters(Pretty(..),prettyPrint)
where

import           Ampersand.Basics hiding ((<$>),view)
import           Ampersand.Core.ParseTree
import           Ampersand.Input.ADL1.Lexer(keywords)
import           RIO.Char (toUpper)
import qualified RIO.List as L
import qualified RIO.NonEmpty as NE
import qualified RIO.Text as T
import qualified RIO.Text.Partial as Partial(replace)  --TODO: Get rid of replace, because it is partial
import qualified RIO.Set as Set
import           Text.PrettyPrint.Leijen

prettyPrint :: Pretty a => a -> Text
prettyPrint x = T.pack $ displayS (renderPretty rfrac col_width doc) ""
        where col_width = 120
              rfrac = 0.4
              doc = pretty x

(<~>) :: Pretty b => Doc -> b -> Doc
(<~>) a b = a <+> pretty b

(<+\>) :: Doc -> Doc -> Doc
(<+\>) a b = a <$$> b

(<~\>) :: Pretty b => Doc -> b -> Doc
(<~\>) a b = a <+\> pretty b

perline :: Pretty a => [a] -> Doc
perline = vsep . map pretty

quote :: Text -> Doc
quote = text.show

quotePurpose :: Text -> Doc
quotePurpose p = text "{+" </> escapeExpl p </> text "+}"
        where escapeExpl = text.T.unpack.escapeCommentStart.escapeLineComment.escapeExplEnd
              escapeCommentStart = escape "{-"
              escapeLineComment = escape "--"
              escapeExplEnd = escape "+}"
              escape x = replace' x (T.intersperse ' ' x)

isId :: Text -> Bool
isId xs = 
  case T.uncons xs of
   Nothing    -> False
   Just (h,_) -> T.all isIdChar xs && isFirstIdChar h && xs `notElem` map T.pack keywords
       where isFirstIdChar x = elem x $ "_"++['a'..'z']++['A'..'Z']
             isIdChar x = isFirstIdChar x || elem x ['0'..'9']

isUpperId :: Text -> Bool
isUpperId xs = 
  case T.uncons xs of
   Nothing    -> False
   Just (h,_) -> isId xs && h `elem` ['A'..'Z']

maybeQuote :: Text -> Doc
maybeQuote a = if isId a then (text . T.unpack) a else quote a

-- adds quotes unless it's an upper identifier
quoteConcept :: Text -> Doc
quoteConcept a = if isUpperId a then (text . T.unpack) a else quote a

prettyhsep :: Pretty a => [a] -> Doc
prettyhsep = hsep . map pretty

commas :: [Doc] -> Doc
commas = encloseSep empty empty comma

listOf :: Pretty a => [a] -> Doc
listOf = commas . map pretty
listOf1 :: Pretty a => NE.NonEmpty a -> Doc
listOf1 = listOf . NE.toList
separate :: Pretty a => Text -> [a] -> Doc
separate d xs = encloseSep empty empty ((text . T.unpack) d) $ map pretty xs

instance Pretty P_Context where
    pretty (PCtx nm _ lang markup pats rs ds cs ks rrules reprs vs gs ifcs ps pops metas) =
               text "CONTEXT"
               <+> quoteConcept nm
               <~> lang
               <~> markup
               <+\> perline metas
               <+\> perline ps
               <+\> perline pats
               <+\> perline rs
               <+\> perline ds
               <+\> perline cs
               <+\> perline ks
               <+\> perline rrules
               <+\> perline reprs
               <+\> perline vs
               <+\> perline gs
               <+\> perline ifcs
               <+\> perline pops
               <+\> text "ENDCONTEXT"
             
instance Pretty Meta where
    pretty (Meta _ nm val) =
        text "META" <+> quote nm <+> quote val

instance Pretty P_RoleRule where
    pretty (Maintain _ roles rules) =
        text "ROLE" <+> listOf1 roles <+> text "MAINTAINS" <+> commas (NE.toList . fmap maybeQuote $ rules)

instance Pretty Role where
    pretty (Role nm) = maybeQuote nm
    pretty (Service nm) = maybeQuote nm

instance Pretty P_Pattern where
    pretty (P_Pat _ nm rls gns dcs rruls reprs cds ids vds xps pop _) =
          text "PATTERN"
          <+>  quoteConcept nm
          <+\> perline rls
          <+\> perline gns
          <+\> perline dcs
          <+\> perline rruls
          <+\> perline reprs
          <+\> perline cds
          <+\> perline ids
          <+\> perline vds
          <+\> perline xps
          <+\> perline pop
          <+\> text "ENDPATTERN"

instance Pretty P_Relation where
    pretty (P_Relation nm sign prps pragma mean _) =
        text "RELATION" <+> (text . T.unpack) nm <~> sign <+> props <+\> pragmas <+\> prettyhsep mean
        where props | prps == Set.fromList [Sym, Asy] = text "[PROP]"
                    | null prps                       = empty
                    | otherwise                       = text ("["++(L.intercalate ",". map show . Set.toList) prps ++ "]") -- do not prettyprint list of properties.
              pragmas | T.null (T.concat pragma) = empty
                      | otherwise   = text "PRAGMA" <+> hsep (map quote pragma)

instance Pretty a => Pretty (Term a) where
   pretty p = case p of
       Prim a -> pretty a
       -- level 0 (rule)
       PEqu _ t1 t2 -> two t1 t2 "="
       PInc _ t1 t2 -> two t1 t2 " |- "
       -- level 1
       PIsc _ t1 t2 -> two t1 t2 "/\\"
       PUni _ t1 t2 -> two t1 t2 "\\/"
       -- level 2
       PDif _ t1 t2 -> two t1 t2 "-"
       -- level 3
       PLrs _ t1 t2 -> two t1 t2 "/"
       PRrs _ t1 t2 -> two t1 t2 "\\"
       PDia _ t1 t2 -> two t1 t2 "<>"
       -- level 4
       PCps _ t1 t2 -> two t1 t2 ";"
       PRad _ t1 t2 -> two t1 t2 "!"
       PPrd _ t1 t2 -> two t1 t2 "#"
       -- level 5
       PKl0 _ t -> post t "*"
       PKl1 _ t -> post t "+"
       PFlp _ t -> post t "~"
       PCpl _ t -> pre t " -" -- a double dash can happen when combined with PDif, therefore the extra space
       -- level 6
       PBrk _ t -> parens $ pretty t
       
       where post t op    = pretty t <> text op
             pre t op     = text op <> pretty t
             two t1 t2 op = pretty t1 <> text op <> pretty t2

instance Pretty TermPrim where
    pretty p = case p of
        PI _ -> text "I"
        Pid _ concept -> text "I[" <> pretty concept <> text "]"
        Patm _ val mCpt ->  pretty val <> case mCpt of
                                            Nothing -> empty
                                            Just concept -> text "[" <> pretty concept <> text "]"
        PVee _ -> text "V"
        Pfull _ s1 s2 -> text "V" <~> P_Sign s1 s2
        PNamedR rel -> pretty rel

instance Pretty P_NamedRel where
    pretty (PNamedRel _ str mpSign) = (text . T.unpack) str <~> mpSign

instance Pretty (PairView TermPrim) where
    pretty (PairView ss) = text "VIOLATION" <+> parens (listOf1 ss)
instance Pretty (PairView (Term TermPrim)) where
    pretty (PairView ss) = text "VIOLATION" <+> parens (listOf1 ss)

instance Pretty (PairViewSegment TermPrim) where
    pretty (PairViewText _ str) = text "TXT" <+> quote str
    pretty (PairViewExp _ srcTgt term) = pretty srcTgt <~> term
instance Pretty (PairViewSegment (Term TermPrim)) where
    pretty (PairViewText _ str) = text "TXT" <+> quote str
    pretty (PairViewExp _ srcTgt term) = pretty srcTgt <~> term

instance Pretty SrcOrTgt where
    pretty = text . map toUpper . show

instance Pretty (P_Rule TermPrim) where
    pretty (P_Rule _ nm expr mean msg viol) =
                text "RULE" <+> rName <~>
                expr <+\>
                perline mean <+\>
                perline msg <~\>
                viol
           where rName = if T.null nm then empty
                         else maybeQuote nm <> text ":"

instance Pretty ConceptDef where
    pretty (Cd _ cpt def ref _) -- from, the last argument, is not used in the parser
        = text "CONCEPT" <+> quoteConcept cpt
               <+> quote def <+> maybeText ref
        where maybeText txt = if T.null txt then empty
                              else quote txt

instance Pretty P_Population where
    pretty p = case p of
                P_RelPopu _ _ _ nrel cs -> text "POPULATION" <+> pretty nrel      <+> text "CONTAINS" <+> contents cs
                P_CptPopu _ cpt       ps -> text "POPULATION" <+> pretty cpt  <+> text "CONTAINS" <+> pretty ps
               where contents = list . map pretty

instance Pretty Representation where
    pretty (Repr _ cs tt) = text "REPRESENT" <+> listOf1 cs <~> text "TYPE" <+> pretty tt

instance Pretty TType where
    pretty = text . show
      
instance Pretty P_Interface where
    pretty (P_Ifc isAPI nm roles obj _ _) =
      text (if isAPI then "API " else "INTERFACE ") <+> maybeQuote nm 
        <+> iroles <+>
         (case obj of
            P_BxExpr{} -> 
                nest 2 (text ":" <+> pretty (obj_ctx obj) <$> pretty (obj_msub obj))
            P_BxTxt {} -> fatal "TXT must not be used directly in a P_Ifc."
         )
      where iroles = if null roles then empty
                     else text "FOR" <+> listOf roles
          
instance Pretty a => Pretty (P_BoxItem a) where
    pretty obj =
     maybeQuote (name obj) <+> text ":" <+>
       case obj of
        (P_BxExpr _ _ ctx mCrud mView msub)
           -> nest 2 (pretty ctx <+> pretty mCrud <+> pretty mView <$> pretty msub)
        (P_BxTxt  _ _ str)
           -> text "TXT" <+> quote str
           
instance Pretty ViewUsage where
    pretty vu = encloseSep  (text " <") (text "> ") (text " ") items
                    where
                      items = (text . T.unpack . vuView $ vu) : map pretty (vuKeys vu)
instance Pretty P_Cruds where
    pretty (P_Cruds _ str) = (text . T.unpack) str
instance Pretty a => Pretty (P_SubIfc a) where
    pretty p = case p of
                P_Box _ c bs         -> boxSpec c <+> text "[" <> listOf bs <> text "]"
                P_InterfaceRef _ isLink str -> text ((if isLink then "LINKTO "else "")++"INTERFACE") <+> maybeQuote str
            where boxSpec :: HTMLTemplateUsage -> Doc
                  boxSpec x = text "BOX "<+> encloseSep  (text " <") (text "> ") (text " ") items
                    where
                      items = (text . T.unpack . btType $ x) : map pretty (btKeys x)
     
instance Pretty (P_IdentDf TermPrim) where
    pretty (P_Id _ lbl cpt ats) =
        text "IDENT" <+> maybeQuote lbl <+> text ":" <~> cpt <+> parens (listOf1 ats)

instance Pretty (P_IdentSegmnt TermPrim) where
    pretty (P_IdentExp obj) =
      case obj of
        (P_BxExpr nm _ ctx _ mView _)
           -> (if T.null nm
               then pretty ctx -- no label
               else maybeQuote nm <> text ":" <~> ctx
              ) <+> pretty mView
        (P_BxTxt  nm _ str)
           -> maybeQuote nm 
              <~> text "TXT" <+> quote str

instance Pretty (P_ViewD TermPrim) where
    pretty (P_Vd _ lbl cpt True Nothing ats) = -- legacy syntax
        text "VIEW" <+> maybeQuote lbl   <+> text ":"
                    <~> cpt <+> parens (listOf ats)
    pretty (P_Vd _ lbl cpt isDefault html ats) = -- new syntax
        text "VIEW" <+> maybeQuote lbl  <+> text ":"
                    <~> cpt <+> (if isDefault then text "DEFAULT" else empty)
                    <+> brackets (listOf ats) <~> html <+> text "ENDVIEW"

instance Pretty HtmlTemplateSpec where
    pretty (HtmlTemplateSpec _ file keyVals) = text "HTML" <+> text "TEMPLATE" <+> quote (T.pack file) <~>
        (case keyVals of 
          _:_ -> encloseSep (text " <") (text "> ") (text " ") (map pretty keyVals)
          [] -> mempty
        )

instance Pretty TemplateKeyValue where
    pretty kv = (text . T.unpack . name $ kv) 
                                 <+> (case tkval kv of
                                        Nothing -> mempty
                                        Just t  -> text " = " <+> (text . show $ t)
                                     ) 

instance Pretty (P_ViewSegment TermPrim) where
    pretty (P_ViewSegment mlab _ pl)
        = ( case mlab of 
             Nothing  -> empty
             Just str -> maybeQuote str <+> text ":" 
          ) <~> pretty pl
instance Pretty (P_ViewSegmtPayLoad TermPrim) where
    pretty (P_ViewExp expr) = pretty expr
    pretty (P_ViewText txt) = text "TXT" <+> quote txt
                        
instance Pretty PPurpose where
    pretty (PRef2 _ obj markup refIds) =
             text "PURPOSE" <~> obj <~> lang <+> refs refIds
             <+\> quotePurpose (mString markup)
        where lang = mFormat markup
              refs rs = if null rs then empty
                        else text "REF" <+> quote (T.intercalate "; " rs)

instance Pretty PRef2Obj where
    pretty p = case p of
        PRef2ConceptDef str       -> text "CONCEPT"   <+> quoteConcept str
        PRef2Relation namedRel    -> text "RELATION"  <~> namedRel
        PRef2Rule str             -> text "RULE"      <+> maybeQuote str
        PRef2IdentityDef str      -> text "IDENT"     <+> maybeQuote str
        PRef2ViewDef str          -> text "VIEW"      <+> maybeQuote str
        PRef2Pattern str          -> text "PATTERN"   <+> maybeQuote str
        PRef2Interface str        -> text "INTERFACE" <+> maybeQuote str
        PRef2Context str          -> text "CONTEXT"   <+> maybeQuote str

instance Pretty PMeaning where
    pretty (PMeaning markup) = text "MEANING" <~> markup

instance Pretty PMessage where
    pretty (PMessage markup) = text "MESSAGE" <~> markup

instance Pretty P_Concept where
    pretty (PCpt nm)   = quoteConcept nm
    pretty P_ONE = text "ONE"

instance Pretty P_Sign where
    pretty (P_Sign src tgt) = brackets (pretty src <> text "*" <> pretty tgt)

instance Pretty PClassify where
    pretty p = 
      case p of
            PClassify _ spc gen -> 
                 text "CLASSIFY" <+> pretty spc <+> 
                     (case (NE.length gen, NE.filter (spc /=) gen) of
                        (2,[x]) -> text "ISA" <~> x
                        _       -> text "IS"  <+> separate "/\\" (NE.toList gen)
                     )
instance Pretty Lang where
    pretty x = text "IN" <+> (text . map toUpper . show $ x)

instance Pretty P_Markup where
    pretty (P_Markup lang format str) =
        pretty lang <~> format <+\> quotePurpose str

instance Pretty PandocFormat where
    pretty = text . map toUpper . show

instance Pretty Prop where
    pretty = text . map toUpper . show

instance Pretty PAtomPair where
    pretty (PPair _ l r) = text "(" <+> pretty l 
                       <~> text "," <+> pretty r 
                       <~> text ")"

instance Pretty PAtomValue where
    pretty pav =  
      case pav of 
       PSingleton   _ _ mav -> case mav of
                                Nothing  -> fatal ("The singleton "<>tshow pav<>" has no type, so it cannot be accuratly prettyprinted in a population statement.")
                                Just val -> text "{" <+> pretty val <+> text "}"
       ScriptString   _ s -> text . show $ s
       XlsxString     _ s -> text . show $ s
       ScriptInt      _ i -> text . show $ i
       ScriptFloat    _ d -> text . show $ d
       XlsxDouble     _ _ -> fatal "We got a value from an .xlsx file, which has to be shown in an expression, however the technicaltype is not known"
       ComnBool       _ b -> text . map toUpper . show $ b
       ScriptDate     _ x -> text . show $ x
       ScriptDateTime _ x -> text . show $ x


replace' :: Text -> Text -> Text -> Text
replace' needle replacement haystack 
   | T.null needle = fatal "Empty needle."
   | otherwise = Partial.replace needle replacement haystack

