{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Ampersand.Input.ADL1.Parser
    ( AmpParser
    , Include(..)
    , pContext
    , pContent
    , pPopulations
    , pTerm
    , pRule
    ) where

import           Ampersand.Basics hiding (many,try)
import           Ampersand.Core.ParseTree
import           Ampersand.Input.ADL1.Lexer (keywords)
import           Ampersand.Input.ADL1.ParsingLib
import qualified RIO.NonEmpty as NE
import qualified RIO.NonEmpty.Partial as PARTIAL
import qualified RIO.Text.Partial as PARTIAL
import qualified RIO.Set as Set
import qualified RIO.Text as T

--- Populations ::= Population+
-- | Parses a list of populations
pPopulations :: AmpParser [P_Population] -- ^ The population list parser
pPopulations = many1 pPopulation

--- Context ::= 'CONTEXT' ConceptName LanguageRef? TextMarkup? ContextElement* 'ENDCONTEXT'
-- | Parses a context
pContext :: AmpParser (P_Context, [Include]) -- ^ The result is the parsed context and a list of include filenames
pContext  = rebuild <$> posOf (pKey "CONTEXT")
                    <*> pConceptName
                    <*> pMaybe pLanguageRef
                    <*> pMaybe pTextMarkup
                    <*> many pContextElement
                    <*  pKey "ENDCONTEXT"
  where
    rebuild :: Origin -> Text -> Maybe Lang -> Maybe PandocFormat -> [ContextElement] -> (P_Context, [Include])
    rebuild    pos'      nm        lang          fmt                   ces
     = (PCtx{ ctx_nm     = nm
            , ctx_pos    = [pos']
            , ctx_lang   = lang
            , ctx_markup = fmt
            , ctx_pats   = [p | CPat p<-ces]       -- The patterns defined in this context
            , ctx_rs     = [p | CRul p<-ces]       -- All user defined rules in this context, but outside patterns
            , ctx_ds     = [p | CRel (p,_)<-ces]       -- The relations defined in this context, outside the scope of patterns
            , ctx_cs     = [c ("CONTEXT "<>nm) | CCon c<-ces]    -- The concept definitions defined in this context, outside the scope of patterns
            , ctx_gs     = concat [ys | CCfy ys<-ces]       -- The Classify definitions defined in this context, outside the scope of patterns
            , ctx_ks     = [k | CIndx k<-ces]      -- The identity definitions defined in this context, outside the scope of patterns
            , ctx_rrules = [x | Cm x <-ces]        -- The MAINTAINS statements in the context
            , ctx_reprs  = [r | CRep r<-ces]
            , ctx_vs     = [v | CView v<-ces]      -- The view definitions defined in this context, outside the scope of patterns
            , ctx_ifcs   = [s | Cifc s<-ces]       -- The interfaces defined in this context, outside the scope of patterns -- fatal ("Diagnostic: "<>concat ["\n\n   "<>show ifc | Cifc ifc<-ces])
            , ctx_ps     = [e | CPrp e<-ces]       -- The purposes defined in this context, outside the scope of patterns
            , ctx_pops   = [p | CPop p<-ces] <> concat [p | CRel (_,p)<-ces]  -- The populations defined in this contextplug, from POPULATION statements as well as from Relation declarations.
            , ctx_metas  = [meta | CMeta meta <-ces]
            }
       , [s | CIncl s<-ces] -- the INCLUDE filenames
       )

    --- ContextElement ::= Meta | PatternDef | ProcessDef | RuleDef | Classify | RelationDef | ConceptDef | Index | ViewDef | Interface | Sqlplug | Phpplug | Purpose | Population | PrintThemes | IncludeStatement
    pContextElement :: AmpParser ContextElement
    pContextElement = CMeta    <$> pMeta         <|>
                      CPat     <$> pPatternDef   <|>
                      CRul     <$> pRuleDef      <|>
                      CCfy     <$> pClassify     <|>
                      CRel     <$> pRelationDef  <|>
                      CCon     <$> pConceptDef   <|>
                      CRep     <$> pRepresentation <|>
                      Cm       <$> pRoleRule     <|>
                      Cm       <$> pServiceRule  <|>
                      CIndx    <$> pIndex        <|>
                      CView    <$> pViewDef      <|>
                      Cifc     <$> pInterface    <|>
                      CPrp     <$> pPurpose      <|>
                      CPop     <$> pPopulation   <|>
                      CIncl    <$> pIncludeStatement

data ContextElement = CMeta Meta
                    | CPat P_Pattern
                    | CRul (P_Rule TermPrim)
                    | CCfy [PClassify]
                    | CRel (P_Relation, [P_Population])
                    | CCon (Text -> ConceptDef)
                    | CRep Representation
                    | Cm P_RoleRule
                    | CIndx P_IdentDef
                    | CView P_ViewDef
                    | Cifc P_Interface
                    | CPrp PPurpose
                    | CPop P_Population
                    | CIncl Include    -- an INCLUDE statement

data Include = Include Origin FilePath [Text]
--- IncludeStatement ::= 'INCLUDE' Text
pIncludeStatement :: AmpParser Include
pIncludeStatement = 
      Include <$> currPos
              <*  pKey "INCLUDE" 
              <*> pString
              <*> (pBrackets (asText pString `sepBy` pComma) <|> return [])

--- LanguageRef ::= 'IN' ('DUTCH' | 'ENGLISH')
pLanguageRef :: AmpParser Lang
pLanguageRef = pKey "IN" *>
               (Dutch   <$ pKey "DUTCH"   <|>
                English <$ pKey "ENGLISH")

--- TextMarkup ::= 'REST' | 'HTML' | 'LATEX' | 'MARKDOWN'
pTextMarkup :: AmpParser PandocFormat
pTextMarkup = ReST     <$ pKey "REST"     <|>
              HTML     <$ pKey "HTML"     <|>
              LaTeX    <$ pKey "LATEX"    <|>
              Markdown <$ pKey "MARKDOWN"

--- Meta ::= 'META' Text Text
pMeta :: AmpParser Meta
pMeta = Meta <$> currPos <* pKey "META" <*> asText pString <*> asText pString

--- PatternDef ::= 'PATTERN' ConceptName PatElem* 'ENDPATTERN' 
pPatternDef  :: AmpParser P_Pattern
pPatternDef
     = rebuild <$> currPos
               <*  pKey "PATTERN"
               <*> pConceptName   -- The name spaces of patterns, processes and concepts are shared.
               <*> many pPatElem
               <*> currPos
               <*  pKey "ENDPATTERN"
  where
    rebuild :: Origin -> Text -> [PatElem] -> Origin -> P_Pattern
    rebuild pos' nm pes end
     = P_Pat { pos = pos'
             , pt_nm  = nm
             , pt_rls = [r | Pr r<-pes]
             , pt_gns = concat [ys | Py ys<-pes]
             , pt_dcs = [d | Pd (d,_)<-pes]
             , pt_RRuls = [rr | Pm rr<-pes]
             , pt_cds = [c nm | Pc c<-pes]
             , pt_Reprs = [x | Prep x<-pes]
             , pt_ids = [k | Pk k<-pes]
             , pt_vds = [v | Pv v<-pes]
             , pt_xps = [e | Pe e<-pes]
             , pt_pop = [p | Pp p<-pes]<>concat [p | Pd (_,p)<-pes]
             , pt_end = end
             }

-- PatElem used by PATTERN
--- PatElem ::= RuleDef | Classify | RelationDef | ConceptDef | Index | ViewDef | Purpose | Population
pPatElem :: AmpParser PatElem
pPatElem = Pr <$> pRuleDef          <|>
           Py <$> pClassify         <|>
           Pd <$> pRelationDef      <|>
           Pm <$> pRoleRule         <|>
           Pm <$> pServiceRule      <|>
           Pc <$> pConceptDef       <|>
           Prep <$> pRepresentation <|>
           Pk <$> pIndex            <|>
           Pv <$> pViewDef          <|>
           Pe <$> pPurpose          <|>
           Pp <$> pPopulation

data PatElem = Pr (P_Rule TermPrim)
             | Py [PClassify]
             | Pd (P_Relation, [P_Population])
             | Pm P_RoleRule
             | Pc (Text -> ConceptDef)
             | Prep Representation
             | Pk P_IdentDef
             | Pv P_ViewDef
             | Pe PPurpose
             | Pp P_Population

--- Classify ::= 'CLASSIFY' ConceptRef ('IS' Cterm | 'ISA' ConceptRef)
pClassify :: AmpParser [PClassify]   -- Example: CLASSIFY A IS B /\ C /\ D
pClassify = fun <$> currPos
                <*  pKey "CLASSIFY"
                <*> pConceptRef `sepBy1` pComma
                <*> (     (is  <$ pKey "IS"  <*> pCterm)
                      <|> (isa <$ pKey "ISA" <*> pConceptRef)
                    )
               where
                 fun :: Origin -> NE.NonEmpty P_Concept -> (Bool, [P_Concept]) -> [PClassify]
                 fun p lhs (isISA ,rhs) = NE.toList $ fmap f lhs
                   where 
                     f s = PClassify 
                             { pos      = p
                             , specific = s
                             , generics = if isISA then s NE.:| rhs else PARTIAL.fromList rhs
                             }
                 --- Cterm ::= Cterm1 ('/\' Cterm1)*
                 --- Cterm1 ::= ConceptRef | ('('? Cterm ')'?)
                 pCterm  = concat <$> pCterm1 `sepBy1` pOperator "/\\"
                 pCterm1 = pure   <$> pConceptRef <|>
                                      pParens pCterm  -- brackets are allowed for educational reasons.
                 is :: [P_Concept] -> (Bool, [P_Concept])
                 is gens = (False, gens)
                 isa :: P_Concept -> (Bool, [P_Concept])
                 isa gen = (True, [gen])

--- RuleDef ::= 'RULE' Label? Rule Meaning* Message* Violation?
pRuleDef :: AmpParser (P_Rule TermPrim)
pRuleDef =  P_Rule <$> currPos
                 <*  pKey "RULE"
                 <*> (try pLabel <|> rulid <$> currPos)
                 <*> pRule
                 <*> many pMeaning
                 <*> many pMessage
                 <*> pMaybe pViolation
           where rulid (FileLoc pos' _) = "rule@" <>tshow pos'
                 rulid _ = fatal "pRuleDef is expecting a file location."

                 --- Violation ::= 'VIOLATION' PairView
                 pViolation :: AmpParser (PairView (Term TermPrim))
                 pViolation = id <$ pKey "VIOLATION" <*> pPairView

                 --- PairView ::= '(' PairViewSegmentList ')'
                 pPairView :: AmpParser (PairView (Term TermPrim))
                 pPairView = PairView <$> pParens (pPairViewSegment `sepBy1` pComma)
                   --    where f xs = PairView {ppv_segs = xs}
                             
                 --- PairViewSegmentList ::= PairViewSegment (',' PairViewSegment)*
                 --- PairViewSegment ::= 'SRC' Term | 'TGT' Term | 'TXT' Text
                 pPairViewSegment :: AmpParser (PairViewSegment (Term TermPrim))
                 pPairViewSegment = PairViewExp  <$> posOf (pKey "SRC") <*> return Src <*> pTerm
                                <|> PairViewExp  <$> posOf (pKey "TGT") <*> return Tgt <*> pTerm
                                <|> PairViewText <$> posOf (pKey "TXT") <*> asText pString

--- RelationDef ::= (RelationNew | RelationOld) Props? ('PRAGMA' Text+)? Meaning* ('=' Content)? '.'?
pRelationDef :: AmpParser (P_Relation, [P_Population])
pRelationDef = reorder <$> currPos
                       <*> (pRelationNew <|> pRelationOld)
                       <*> optSet pProps
                       <*> optList (pKey "PRAGMA" *> many1 (asText pString))
                       <*> many pMeaning
                       <*> optList (pOperator "=" *> pContent)
                       <*  optList (pOperator ".")
            where reorder pos' (nm,sign,fun) prop pragma meanings prs =
                    (P_Relation nm sign props pragma meanings pos', map pair2pop prs)
                    where 
                      props = prop `Set.union` fun
                      pair2pop :: PAtomPair -> P_Population
                      pair2pop a = P_RelPopu Nothing Nothing (origin a) rel [a]
                      rel :: P_NamedRel   -- the named relation
                      rel = PNamedRel pos' nm (Just sign)

--- RelationNew ::= 'RELATION' Varid Signature
pRelationNew :: AmpParser (Text,P_Sign,Props)
pRelationNew = (,,) <$  pKey "RELATION"
                    <*> asText pVarid
                    <*> pSign
                    <*> return Set.empty

--- RelationOld ::= Varid '::' ConceptRef Fun ConceptRef
pRelationOld :: AmpParser (Text,P_Sign,Props)
pRelationOld = relOld <$> asText pVarid
                      <*  pOperator "::"
                      <*> pConceptRef
                      <*> pFun
                      <*> pConceptRef
            where relOld nm src fun tgt = (nm,P_Sign src tgt,fun)

--- Props ::= '[' PropList? ']'
pProps :: AmpParser (Set.Set Prop)
pProps  = normalizeProps <$> pBrackets (pProp `sepBy` pComma)
        --- PropList ::= Prop (',' Prop)*
        --- Prop ::= 'UNI' | 'INJ' | 'SUR' | 'TOT' | 'SYM' | 'ASY' | 'TRN' | 'RFX' | 'IRF' | 'PROP'
  where pProp :: AmpParser Prop
        pProp = choice [ p <$ pKey (show p) | p <- [minBound..] ]
        normalizeProps :: [Prop] -> Props
        normalizeProps = conv.rep . Set.fromList
            where -- replace PROP by SYM, ASY
                  rep :: Props -> Props
                  rep ps 
                    | Prop `elem` ps = Set.fromList [Sym, Asy] `Set.union` (Prop `Set.delete` ps)
                    | otherwise            = ps
                  -- add Uni and Inj if ps has neither Sym nor Asy
                  conv :: Props -> Props
                  conv ps = ps `Set.union`
                    if Sym `elem` ps && Asy `elem` ps 
                    then Set.fromList [Uni,Inj]
                    else Set.empty


--- Fun ::= '*' | '->' | '<-' | '[' Mults ']'
pFun :: AmpParser Props
pFun  =  Set.empty               <$ pOperator "*"  <|>
        Set.fromList [Uni,Tot] <$ pOperator "->" <|>
        Set.fromList [Sur,Inj] <$ pOperator "<-" <|>
        pBrackets pMults
        --- Mults ::= Mult '-' Mult
  where pMults :: AmpParser Props
        pMults = Set.union <$> optSet (pMult (Sur,Inj))
                           <*  pDash
                           <*> optSet (pMult (Tot,Uni))

        --- Mult ::= ('0' | '1') '..' ('1' | '*') | '*' | '1'
        --TODO: refactor to Mult ::= '0' '..' ('1' | '*') | '1'('..' ('1' | '*'))? | '*'
        pMult :: (Prop,Prop) -> AmpParser Props
        pMult (ts,ui) = Set.union <$> (Set.empty    <$ pZero   <|> Set.singleton ts <$ try pOne)
                                  <*  pOperator ".."
                                  <*> (Set.singleton ui <$ try pOne <|> (Set.empty   <$ pOperator "*" )) <|>
                        Set.empty <$ pOperator "*"  <|>
                        Set.fromList [ts,ui] <$ try pOne

--- ConceptDef ::= 'CONCEPT' ConceptName Text ('TYPE' Text)? Text?
pConceptDef :: AmpParser (Text->ConceptDef)
pConceptDef       = Cd <$> currPos
                       <*  pKey "CONCEPT"
                       <*> pConceptName
                       <*> (asText pString <?> "concept definition (string)")
                       <*> (asText pString `opt` "") -- a reference to the source of this definition.

--- Representation ::= 'REPRESENT' ConceptNameList 'TYPE' AdlTType
pRepresentation :: AmpParser Representation
pRepresentation
  = Repr <$> currPos
         <*  pKey "REPRESENT"
         <*> pConceptRef `sepBy1` pComma
         <*  pKey "TYPE"
         <*> pAdlTType
--- AdlTType = ...<enumeration>
pAdlTType :: AmpParser TType
pAdlTType
        = k Alphanumeric     "ALPHANUMERIC"
      <|> k BigAlphanumeric  "BIGALPHANUMERIC"
      <|> k HugeAlphanumeric "HUGEALPHANUMERIC"
      <|> k Password         "PASSWORD"
      <|> k Binary           "BINARY"
      <|> k BigBinary        "BIGBINARY"
      <|> k HugeBinary       "HUGEBINARY"
      <|> k Date             "DATE"
      <|> k DateTime         "DATETIME"
      <|> k Boolean          "BOOLEAN"
      <|> k Integer          "INTEGER"
      <|> k Float            "FLOAT"
      <|> k Object           "OBJECT"

  where
   k tt str = f <$> pKey str where f _ = tt

-- | A identity definition looks like:   IDENT onNameAdress : Person(name, address),
-- which means that name<>name~ /\ address<>addres~ |- I[Person].
-- The label 'onNameAddress' is used to refer to this identity.
-- You may also use an expression on each attribute place, for example: IDENT onpassport: Person(nationality, passport;documentnr),
-- which means that nationality<>nationality~ /\ passport;documentnr<>(passport;documentnr)~ |- I[Person].
--- Index ::= 'IDENT' Label ConceptRef '(' IndSegmentList ')'
pIndex :: AmpParser P_IdentDef
pIndex  = P_Id <$> currPos
               <*  pKey "IDENT"
               <*> pLabel
               <*> pConceptRef
               <*> pParens (pIndSegment `sepBy1` pComma)
    where
          --- IndSegmentList ::= Attr (',' Attr)*
          pIndSegment :: AmpParser P_IdentSegment
          pIndSegment = P_IdentExp <$> pAtt


--- ViewDef ::= FancyViewDef | ViewDefLegacy
pViewDef :: AmpParser P_ViewDef
pViewDef = try pFancyViewDef <|> try pViewDefLegacy -- introduces backtracking, but is more elegant than rewriting pViewDefLegacy to disallow "KEY ... ENDVIEW".

--- FancyViewDef ::= 'VIEW' Label ConceptOneRef 'DEFAULT'? ('[' ViewObjList ']')?  HtmlView? 'ENDVIEW'
pFancyViewDef :: AmpParser P_ViewDef
pFancyViewDef  = mkViewDef <$> currPos
                      <*  pKey "VIEW"
                      <*> pLabel
                      <*> pConceptOneRef
                      <*> pIsThere (pKey "DEFAULT")
                      <*> (pBrackets (pViewSegment False `sepBy` pComma) `opt` [])
                      <*> pMaybe pHtmlView
                      <*  pKey "ENDVIEW"
    where mkViewDef pos' nm cpt isDef ats html =
            P_Vd { pos = pos'
                 , vd_lbl = nm
                 , vd_cpt = cpt
                 , vd_isDefault = isDef
                 , vd_html = html
                 , vd_ats = ats
                 }
          --- ViewSegmentList ::= ViewSegment (',' ViewSegment)*
          --- HtmlView ::= 'HTML' 'TEMPLATE' Text ('{' (KEYVALS)'}')?
          pHtmlView :: AmpParser HtmlTemplateSpec
          pHtmlView = build <$> currPos <* pKey "HTML" <* pKey "TEMPLATE" <*> pString <*> optional pdefAtts
            where build :: Origin -> FilePath -> Maybe [TemplateKeyValue] -> HtmlTemplateSpec
                  build o fp mkeys = HtmlTemplateSpec o fp (fromMaybe [] mkeys)
                  pdefAtts :: AmpParser [TemplateKeyValue]
                  pdefAtts = pChevrons $ many pTemplateKeyValue
--- ViewSegmentLoad ::= Term | 'TXT' Text
pViewSegmentLoad :: AmpParser (P_ViewSegmtPayLoad TermPrim)           
pViewSegmentLoad = P_ViewExp  <$> pTerm
               <|> P_ViewText <$ pKey "TXT" <*> asText pString
            
--- ViewSegment ::= Label ViewSegmentLoad
pViewSegment :: Bool -> AmpParser (P_ViewSegment  TermPrim)
pViewSegment labelIsOptional
       = P_ViewSegment
               <$> (if labelIsOptional then pMaybe (try pLabel) else Just <$> try pLabel)
               <*> currPos
               <*> pViewSegmentLoad

--- ViewDefLegacy ::= 'VIEW' Label ConceptOneRef '(' ViewSegmentList ')'
pViewDefLegacy :: AmpParser P_ViewDef
pViewDefLegacy = P_Vd <$> currPos
                      <*  pKey "VIEW"
                      <*> pLabel
                      <*> pConceptOneRef
                      <*> return True
                      <*> return Nothing
                      <*> pParens (pViewSegment True `sepBy` pComma)


--- Interface ::= 'INTERFACE' ADLid Params? Roles? ':' Term (ADLid | Conid)? SubInterface?
pInterface :: AmpParser P_Interface
pInterface = lbl <$> currPos                                       
                 <*> pInterfaceIsAPI
                 <*> pADLid
                 <*> pMaybe pParams
                 <*> pMaybe pRoles 
                 <*> (pColon *> pTerm)          -- the expression of the interface object
                 <*> pMaybe pCruds              -- The Crud-string (will later be tested, that it can contain only characters crud (upper/lower case)
                 <*> pMaybe pViewUsage  -- The view that should be used for this object
                 <*> pSubInterface
    where lbl :: Origin -> Bool -> Text ->  a -> Maybe (NE.NonEmpty Role) -> Term TermPrim -> Maybe P_Cruds -> Maybe ViewUsage -> P_SubInterface -> P_Interface
          lbl p isAPI nm _params roles ctx mCrud mView sub
             = P_Ifc { ifc_IsAPI  = isAPI
                     , ifc_Name   = nm
                     , ifc_Roles  = maybe [] NE.toList roles
                     , ifc_Obj    = P_BxExpr { obj_nm   = nm
                                          , pos      = p
                                          , obj_ctx  = ctx
                                          , obj_crud = mCrud
                                          , obj_mView = mView
                                          , obj_msub = Just sub
                                          }
                     , pos        = p
                     , ifc_Prp    = ""   --TODO: Nothing in syntax defined for the purpose of the interface.
                     }
          --- Params ::= '(' NamedRel ')'
          pParams = pParens(pNamedRel `sepBy1` pComma)
          --- Roles ::= 'FOR' RoleList
          pRoles  = pKey "FOR" *> pRole False `sepBy1` pComma

--- SubInterface ::= ('BOX' HTMLTemplateUsage? | 'FORM' | 'TABLE') Box | 'LINKTO'? 'INTERFACE' ADLid
pSubInterface :: AmpParser P_SubInterface
pSubInterface = P_Box          <$> currPos <*> pBoxHeader <*> pBox
            <|> P_InterfaceRef <$> currPos 
                               <*> pIsThere (pKey "LINKTO") <*  pInterfaceKey 
                               <*> pADLid
  where pBoxHeader :: AmpParser HTMLTemplateUsage
        pBoxHeader = 
              build <$> currPos <* pKey "BOX" <*> optional pHtmlTemplateUsage
        build :: Origin -> Maybe HTMLTemplateUsage ->  HTMLTemplateUsage
        build o = fromMaybe HTMLTemplateUsage 
                              { pos = o
                              , btType = "FORM"
                              , btKeys = []
                              } 
         
pHtmlTemplateUsage :: AmpParser HTMLTemplateUsage
pHtmlTemplateUsage = do
    cp <- currPos
    (typ,keys) <- pTemplateSpecification
    return HTMLTemplateUsage
             { pos = cp
             , btType = typ
             , btKeys = keys
             }
pViewUsage :: AmpParser ViewUsage
pViewUsage = do
    cp <- currPos
    (typ,keys) <- pTemplateSpecification
    return ViewUsage
             { pos = cp
             , vuView = typ
             , vuKeys = keys
             }
pTemplateSpecification :: AmpParser (Text, [TemplateKeyValue])
pTemplateSpecification = pChevrons $
                          (,) <$> asText (pVarid <|> pConid <|> anyKeyWord)
                              <*> many pTemplateKeyValue
anyKeyWord :: AmpParser String
anyKeyWord = case map pKey keywords of
                [] -> fatal "We should have keywords. We allways have."
                h:tl -> foldr (<|>) h tl
pTemplateKeyValue :: AmpParser TemplateKeyValue
pTemplateKeyValue = 
  TemplateKeyValue 
          <$> currPos
          <*> asText (pVarid <|> pConid <|> anyKeyWord)
          <*> optional (id <$ pOperator "=" <*> asText pString)

--- ObjDef ::= Label Term ('<' Conid '>')? SubInterface?
--- ObjDefList ::= ObjDef (',' ObjDef)*
pObjDef :: AmpParser P_BoxItemTermPrim
pObjDef = pBoxItem <$> currPos
                   <*> pLabel
                   <*> (pObj <|> pTxt) 
  where
    --build p lable fun = pBoxItem p lable <$> fun
    pBoxItem :: Origin -> Text -> P_BoxItemTermPrim -> P_BoxItemTermPrim
    pBoxItem p nm fun = fun{ pos    = p
                           , obj_nm = nm}
      
    pObj :: AmpParser P_BoxItemTermPrim
    pObj = obj     <$> pTerm            -- the context expression (for example: I[c])
                   <*> pMaybe pCruds
                   <*> pMaybe pViewUsage --for the view
                   <*> pMaybe pSubInterface  -- the optional subinterface
          where obj ctx mCrud mView msub =
                  P_BxExpr { obj_nm    = fatal "This should have been filled in promptly."
                        , pos       = fatal "This should have been filled in promptly."
                        , obj_ctx   = ctx
                        , obj_crud  = mCrud
                        , obj_mView = mView
                        , obj_msub  = msub
                        }
    pTxt :: AmpParser P_BoxItemTermPrim
    pTxt = obj <$ pKey "TXT"
               <*> asText pString
          where obj txt = 
                  P_BxTxt  { obj_nm   = fatal "This should have been filled in promptly."
                        , pos      = fatal "This should have been filled in promptly."
                        , obj_txt  = txt
                        }

--- Cruds ::= crud in upper /lowercase combinations
pCruds :: AmpParser P_Cruds
pCruds = P_Cruds <$> currPos <*> asText pCrudString

--- Box ::= '[' ObjDefList ']'
pBox :: AmpParser [P_BoxItemTermPrim]
pBox = pBrackets $ pObjDef `sepBy` pComma

--- Purpose ::= 'PURPOSE' Ref2Obj LanguageRef? TextMarkup? ('REF' StringListSemi)? Expl
pPurpose :: AmpParser PPurpose
pPurpose = rebuild <$> currPos
                   <*  pKey "PURPOSE"
                   <*> pRef2Obj
                   <*> pMaybe pLanguageRef
                   <*> pMaybe pTextMarkup
                   <*> pMaybe (pKey "REF" *> asText pString `sepBy1` pSemi)
                   <*> asText pAmpersandMarkup
     where
       rebuild :: Origin -> PRef2Obj -> Maybe Lang -> Maybe PandocFormat -> Maybe (NE.NonEmpty Text) -> Text -> PPurpose
       rebuild    orig      obj         lang          fmt                   refs       str
           = PRef2 orig obj (P_Markup lang fmt str) (concatMap splitOnSemicolon (maybe [] NE.toList refs))
              -- TODO: This separation should not happen in the parser
              where splitOnSemicolon :: Text -> [Text]
                    splitOnSemicolon = PARTIAL.splitOn ";" -- This is safe: The first argument of splitOn must not be empty.
       --- Ref2Obj ::= 'CONCEPT' ConceptName | 'RELATION' NamedRel | 'RULE' ADLid | 'IDENT' ADLid | 'VIEW' ADLid | 'PATTERN' ADLid | 'INTERFACE' ADLid | 'CONTEXT' ADLid
       pRef2Obj :: AmpParser PRef2Obj
       pRef2Obj = PRef2ConceptDef  <$ pKey "CONCEPT"   <*> pConceptName <|>
                  PRef2Relation    <$ pKey "RELATION"  <*> pNamedRel    <|>
                  PRef2Rule        <$ pKey "RULE"      <*> pADLid       <|>
                  PRef2IdentityDef <$ pKey "IDENT"     <*> pADLid       <|>
                  PRef2ViewDef     <$ pKey "VIEW"      <*> pADLid       <|>
                  PRef2Pattern     <$ pKey "PATTERN"   <*> pADLid       <|>
                  PRef2Interface   <$ pInterfaceKey    <*> pADLid       <|>
                  PRef2Context     <$ pKey "CONTEXT"   <*> pADLid

pInterfaceKey :: AmpParser Text
pInterfaceKey = asText $ pKey "INTERFACE" <|> pKey "API" -- On special request of Rieks, the keyword "API" is allowed everywhere where the keyword "INTERFACE" is used. https://github.com/AmpersandTarski/Ampersand/issues/789

pInterfaceIsAPI :: AmpParser Bool
pInterfaceIsAPI = ("API" ==) <$> pInterfaceKey

--- Population ::= 'POPULATION' (NamedRel 'CONTAINS' Content | ConceptName 'CONTAINS' '[' ValueList ']')
-- | Parses a population
pPopulation :: AmpParser P_Population -- ^ The population parser
pPopulation = pKey "POPULATION" *> (
                  P_RelPopu Nothing Nothing <$> currPos <*> pNamedRel <* pKey "CONTAINS" <*> pContent <|>
                  P_CptPopu <$> currPos <*> pConceptRef <* pKey "CONTAINS" <*> pBrackets (pAtomValue `sepBy` pComma))

--- RoleRule ::= 'ROLE' RoleList 'MAINTAINS' ADLidList
--TODO: Rename the RoleRule to RoleMantains and RoleRelation to RoleEdits.
pRoleRule :: AmpParser P_RoleRule
pRoleRule = try (Maintain <$> currPos
                          <*  pKey "ROLE"
                          <*> pRole False `sepBy1` pComma
                          <*  pKey "MAINTAINS")
                <*> pADLid `sepBy1` pComma
--- ServiceRule ::= 'SERVICE' RoleList 'MAINTAINS' ADLidList
--TODO: Rename the RoleRule to RoleMantains and RoleRelation to RoleEdits.
pServiceRule :: AmpParser P_RoleRule
pServiceRule = try (Maintain <$> currPos
                          <*  pKey "SERVICE"
                          <*> pRole True `sepBy1` pComma
                          <*  pKey "MAINTAINS")
                <*> pADLid `sepBy1` pComma

--- Role ::= ADLid
--- RoleList ::= Role (',' Role)*
pRole :: Bool -> AmpParser Role
pRole isService =  (if isService then Service else Role) <$> pADLid

--- Meaning ::= 'MEANING' Markup
pMeaning :: AmpParser PMeaning
pMeaning = PMeaning <$  pKey "MEANING"
                    <*> pMarkup

--- Message ::= 'MESSAGE' Markup
pMessage :: AmpParser PMessage
pMessage = PMessage <$ pKey "MESSAGE" <*> pMarkup

--- Markup ::= LanguageRef? TextMarkup? (Text | Expl)
pMarkup :: AmpParser P_Markup
pMarkup = P_Markup
           <$> pMaybe pLanguageRef
           <*> pMaybe pTextMarkup
           <*> (asText pString <|> asText pAmpersandMarkup)

--- Rule ::= Term ('=' Term | '|-' Term)?
-- | Parses a rule
pRule :: AmpParser (Term TermPrim) -- ^ The rule parser
pRule  =  pTerm <??> (invert PEqu  <$> currPos <* pOperator "="  <*> pTerm <|>
                      invert PInc  <$> currPos <* pOperator "|-" <*> pTerm)


{-
pTerm is slightly more complicated, for the purpose of avoiding "associative" brackets.
The idea is that each operator ("/\\" or "\\/") can be parsed as a sequence without brackets.
However, as soon as they are combined, brackets are needed to disambiguate the combination.
There is no natural precedence of one operator over the other.
Brackets are enforced by parsing the subexpression as pTrm5.
In order to maintain performance standards, the parser is left factored.
The functions pars and f have arguments 'combinator' and 'operator' only to avoid writing the same code twice.
-}
--- Term ::= Trm2 (('/\' Trm2)+ | ('\/' Trm2)+)?
-- | Parses a term
pTerm :: AmpParser (Term TermPrim) -- ^ The term parser
pTerm = pTrm2 <??> (invertT PIsc <$> rightAssociate PIsc "/\\" pTrm2 <|>
                    invertT PUni <$> rightAssociate PUni "\\/" pTrm2)

-- The left factored version of difference: (Actually, there is no need for left-factoring here, but no harm either)
--- Trm2 ::= Trm3 ('-' Trm3)?
pTrm2 :: AmpParser (Term TermPrim)
pTrm2   = pTrm3 <??> (invert PDif <$> posOf pDash <*> pTrm3)

-- The left factored version of right- and left residuals:
--- Trm3 ::= Trm4 ('/' Trm4 | '\' Trm4 | '<>' Trm4)?
pTrm3 :: AmpParser (Term TermPrim)
pTrm3  =  pTrm4 <??> (invert PLrs <$> currPos <* pOperator "/"  <*> pTrm4 <|>
                      invert PRrs <$> currPos <* pOperator "\\" <*> pTrm4 <|>
                      invert PDia <$> currPos <* pOperator "<>" <*> pTrm4 )

-- composition and relational addition are associative, and parsed similar to union and intersect...
--- Trm4 ::= Trm5 ((';' Trm5)+ | ('!' Trm5)+ | ('#' Trm5)+)?
pTrm4 :: AmpParser (Term TermPrim)
pTrm4   = pTrm5 <??> (invertT PCps <$> rightAssociate PCps ";" pTrm5 <|>
                      invertT PRad <$> rightAssociate PRad "!" pTrm5 <|>
                      invertT PPrd <$> rightAssociate PPrd "#" pTrm5)

--- Trm5 ::= '-'* Trm6 ('~' | '*' | '+')*
pTrm5 :: AmpParser (Term TermPrim)
--TODO: Separate into prefix and postfix top-level functions
pTrm5  =  f <$> many (valPosOf pDash) <*> pTrm6  <*> many (valPosOf (pOperator "~" <|> pOperator "*" <|> pOperator "+" ))
          where f ms pe (("~",_):ps) = let x=f ms pe ps in PFlp (origin x) x  -- the type checker requires that the origin of x is equal to the origin of its converse.
                f ms pe (("*",orig):ps) = PKl0 orig (f ms pe ps)              -- e*  Kleene closure (star)
                f ms pe (("+",orig):ps) = PKl1 orig (f ms pe ps)              -- e+  Kleene closure (plus)
                f (_:_:ms) pe ps        = f ms pe ps                          -- -e  complement     (unary minus)
                f ((_,orig):ms) pe ps   = let x=f ms pe ps in PCpl orig x     -- the type checker requires that the origin of x is equal to the origin of its complement.
                f _ pe _                = pe

--- Trm6 ::= RelationRef | '(' Term ')'
pTrm6 :: AmpParser (Term TermPrim)
pTrm6 = Prim <$> pRelationRef  <|>
        PBrk <$> currPos <*> pParens pTerm

-- Help function for several expressions. The type 't' is each of the terms.
invert :: (Origin -> t -> t -> t) -> Origin -> t -> t -> t
invert constructor position rightTerm leftTerm = constructor position leftTerm rightTerm

-- Variant for the above function with a tuple, for usage with right association
invertT :: (Origin -> t -> t -> t) -> (Origin, t) -> t -> t
invertT constructor (position,rightTerm) leftTerm = constructor position leftTerm rightTerm

-- Help function for pTerm and pTrm4, to allow right association
rightAssociate :: (Origin -> t -> t -> t) -> String -> AmpParser t -> AmpParser (Origin, t)
rightAssociate combinator operator term
                 = g <$> currPos <* pOperator operator <*> term <*> pMaybe (rightAssociate combinator operator term)
                          where g orig y Nothing  = (orig, y)
                                g orig y (Just (org,z)) = (orig, combinator org y z)

--- RelationRef ::= NamedRel | 'I' ('[' ConceptOneRef ']')? | 'V' Signature? | Singleton ('[' ConceptOneRef ']')?
pRelationRef :: AmpParser TermPrim
pRelationRef      = PNamedR <$> pNamedRel
                <|> pid   <$> currPos <* pKey "I" <*> pMaybe (pBrackets pConceptOneRef)
                <|> pfull <$> currPos <* pKey "V" <*> pMaybe pSign
                <|> Patm  <$> currPos <*> pSingleton <*> pMaybe (pBrackets pConceptOneRef)
                    where pid orig Nothing = PI orig
                          pid orig (Just c)= Pid orig c
                          pfull orig Nothing = PVee orig
                          pfull orig (Just (P_Sign src trg)) = Pfull orig src trg

pSingleton :: AmpParser PAtomValue
pSingleton = value2PAtomValue <$> currPos <*> 
                 (             pAtomValInPopulation True
                  <|> pBraces (pAtomValInPopulation False)
                 ) 
pAtomValue :: AmpParser PAtomValue
pAtomValue = value2PAtomValue <$> currPos <*> pAtomValInPopulation False

value2PAtomValue :: Origin -> Value -> PAtomValue
value2PAtomValue o v = case v of
         VSingleton s x -> PSingleton o s (fmap (value2PAtomValue o) x)
         VRealString s  -> ScriptString o s
         VInt i         -> ScriptInt o (toInteger i)
         VFloat x       -> ScriptFloat o x
         VBoolean b     -> ComnBool o b
         VDateTime x    -> ScriptDateTime o x
         VDate x        -> ScriptDate o x

--- Attr ::= Label? Term
pAtt :: AmpParser P_BoxItemTermPrim
-- There's an ambiguity in the grammar here: If we see an identifier, we don't know whether it's a label followed by ':' or a term name.
pAtt = rebuild <$> currPos <*> try pLabel `opt` "" <*> try pTerm
  where rebuild pos' nm ctx = P_BxExpr { obj_nm   = nm
                                    , pos      = pos'
                                    , obj_ctx  = ctx
                                    , obj_crud = Nothing
                                    , obj_mView = Nothing
                                    , obj_msub = Nothing
                                    }

--- NamedRelList ::= NamedRel (',' NamedRel)*
--- NamedRel ::= Varid Signature?
pNamedRel :: AmpParser P_NamedRel
pNamedRel = PNamedRel  <$> currPos <*> asText pVarid <*> pMaybe pSign

--- Signature ::= '[' ConceptOneRef ('*' ConceptOneRef)? ']'
pSign :: AmpParser P_Sign
pSign = pBrackets sign
   where sign = mkSign <$> pConceptOneRef <*> pMaybe (pOperator "*" *> pConceptOneRef)
         mkSign src mTgt = P_Sign src (fromMaybe src mTgt)

--- ConceptName ::= Conid | Text
--- ConceptNameList ::= ConceptName (',' ConceptName)
pConceptName ::   AmpParser Text
pConceptName = asText $ pConid <|> pString

--- ConceptRef ::= ConceptName
pConceptRef ::    AmpParser P_Concept
pConceptRef = PCpt <$> pConceptName

--- ConceptOneRef ::= 'ONE' | ConceptRef
pConceptOneRef :: AmpParser P_Concept
pConceptOneRef = (P_ONE <$ pKey "ONE") <|> pConceptRef

--- Label ::= ADLid ':'
pLabel :: AmpParser Text
pLabel = pADLid <* pColon

--- Content ::= '[' RecordList? ']'
pContent :: AmpParser [PAtomPair]
pContent = pBrackets (pRecord `sepBy` (pComma <|> pSemi))
          --- RecordList ::= Record ((','|';') Record)*
          --- Record ::= Text ',' Text
    where pRecord :: AmpParser PAtomPair
          pRecord =
             pParens (PPair <$> currPos
                            <*> pAtomValue
                            <*  pComma
                            <*> pAtomValue
                     )

--- ADLid ::= Varid | Conid | String
--- ADLidList ::= ADLid (',' ADLid)*
--- ADLidListList ::= ADLid+ (',' ADLid+)*
pADLid :: AmpParser Text
pADLid = asText $ pVarid <|> pConid <|> pString

asText :: AmpParser String -> AmpParser Text
asText = fmap T.pack