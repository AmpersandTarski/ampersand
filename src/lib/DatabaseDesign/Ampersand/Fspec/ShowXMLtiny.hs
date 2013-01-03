{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module DatabaseDesign.Ampersand.Fspec.ShowXMLtiny (showXML)
where

-- TODO: Als het Ampersand bestand strings bevat met speciale characters als '&' en '"', dan wordt nu nog foute XML-code gegenereerd...

   import DatabaseDesign.Ampersand.ADL1
--   import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree
   import DatabaseDesign.Ampersand.Classes
   import DatabaseDesign.Ampersand.Fspec.ShowADL 
   import DatabaseDesign.Ampersand.Basics
   import DatabaseDesign.Ampersand.Fspec.Fspec
   import Data.Time.LocalTime
   import DatabaseDesign.Ampersand.Fspec.Plug 
   import DatabaseDesign.Ampersand.Misc.TinyXML 
   
   fatal :: Int -> String -> a
   fatal = fatalMsg "Fspec.ShowXMLtiny"

   showXML :: Fspc -> LocalTime -> String
   showXML fSpec now 
            = validXML 
               ("<?xml version=\"1.0\" encoding=\"utf-8\"?>" ++
                "<tns:ADL xmlns:tns=\"http://ampersand.sourceforge.net/ADL\" "++
                "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" "++
                "xsi:schemaLocation=\"http://ampersand.sourceforge.net/AdlDocs "++
                "ADL.xsd \">"++
                "<!-- Generated with "++ ampersandVersionStr ++", at "++ show now ++" -->" ++
                "<!-- Warning: The format of this generated xml document is subject to changes, and it"++
                " isn't very stable. Please notify the developers of Ampersand if you have specific needs. -->")
              ++
              showXTree ( mkXmlTree fSpec) ++
              "</tns:ADL>"   

   nameToAttr :: Identified x => x -> XAtt 
   nameToAttr x = mkAttr "name" (name x)

   ----------------------------------------------------------------------
  
   class XML a where 
    mkTag :: a -> XTag
    mkXmlTree :: a -> XTree
   
   still2bdone :: String -> XTree
   still2bdone worktxt = Node (Tag "NotImplementedYet" [mkAttr "work2do_in_ShowXML.hs"  worktxt])     


   instance XML Fspc where
     mkTag f = Tag "Fspec" [nameToAttr f] 
     mkXmlTree f@(Fspc{})
        = Elem (mkTag f) ( 
             []
--          ++ [ Elem (simpleTag "Plugs-In-Ampersand-Script")     (map mkXmlTree (vplugs f))]
--          ++ [ Elem (simpleTag "Plugs-also-derived-ones") (map mkXmlTree (plugs f))]
          ++ [ Elem (simpleTag "Patterns")     (map mkXmlTree (patterns f))] 
          ++ [ Elem (simpleTag "InterfaceS")   (map mkXmlTree (interfaceS f))] 
          ++ [ Elem (simpleTag "InterfaceG")   (map mkXmlTree (interfaceG f))] 
      --    ++ [ Elem (simpleTag "Activities")   (map mkXmlTree (interfaces f))] 
          ++ [ Elem (simpleTag "Rules")        (map mkXmlTree (vrules f))] 
          ++ [ Elem (simpleTag "GRules")       (map mkXmlTree (grules f))] 
          ++ [ Elem (simpleTag "Declarations") (map mkXmlTree (vrels f))] 
          ++ [ Elem (simpleTag "Violations")   (map violation2XmlTree (allViolations f))]
          ++ [ still2bdone "Ontology" ] -- ++ [ Elem (simpleTag "Ontology") [mkXmlTree hhh] 
          ++ [ Elem (simpleTag "Explanations") (map mkXmlTree (fSexpls f))]
                 )
             where violation2XmlTree :: (Rule,[Paire]) -> XTree
                   violation2XmlTree (r,ps) = 
                     Elem (Tag "Violation" [] )
                      (
                       Elem (simpleTag "ViolatedRule") [mkXmlTree r]
                       :[Elem (simpleTag "Culprits")(map mkXmlTree ps)]
                      )
                 
   instance XML Activity where
     mkTag _ = Tag "Activity" [] 
     mkXmlTree act
        = Elem (mkTag act) (  
             [ Elem (simpleTag "Rule")               [mkXmlTree (actRule act)]]
          ++ [ Elem (simpleTag "Editable Relations") (map mkXmlTree (actTrig   act)) |not (null (actTrig   act))] 
          ++ [ Elem (simpleTag "Affected Relations") (map mkXmlTree (actAffect act)) |not (null (actAffect act))] 
          ++ [ Elem (simpleTag "Affected Quads")     []] -- TODO
          ++ [ Elem (simpleTag "ECArules")           (map mkXmlTree (actEcas   act)) |not (null (actEcas   act))] 
          ++ [ Elem (simpleTag "Explanations")       (map mkXmlTree (actPurp   act)) |not (null (actPurp   act))] 
           )

   instance XML FPA where
     mkTag _ = Tag "FPA" [] 
     mkXmlTree fpa'
        = Elem (mkTag fpa') []  -- TODO make content for this XML field

   instance XML Field where
     mkTag _ = Tag "Field" [] 
     mkXmlTree f
        = Elem (Tag "Field"
                    [ mkAttr "Editable" (show (fld_editable f))
                    , mkAttr "list"     (show (fld_list     f))
                    , mkAttr "Must"     (show (fld_must     f))
                    , mkAttr "New"      (show (fld_new      f))
                    , mkAttr "sLevel"   (show (fld_sLevel   f))
                    ])
               ( Elem (simpleTag "Expression") [mkXmlTree (fld_expr f)] :
                 [ Elem (simpleTag "Relation")   [mkXmlTree (fld_rel f)]]
               ) 

   instance XML Pattern where
     mkTag pat = Tag "Pattern" [ nameToAttr pat]
     mkXmlTree pat
        = Elem (mkTag pat) (  
             [ Elem (simpleTag "Rules")        (map mkXmlTree (ptrls pat)) |not (null (ptrls pat))] 
          ++ [ Elem (simpleTag "Gens")         (map mkXmlTree (ptgns pat)) |not (null (ptgns pat))] 
          ++ [ Elem (simpleTag "Declarations") (map mkXmlTree (ptdcs pat)) |not (null (ptdcs pat))] 
          ++ [ Elem (simpleTag "Concepts")     (map mkXmlTree (conceptDefs pat)) |not (null (conceptDefs pat))] 
          ++ [ Elem (simpleTag "Keys")         (map mkXmlTree (ptkds pat)) |not (null (ptkds pat))] 
          ++ [ Elem (simpleTag "Explanations") (map mkXmlTree (ptxps pat)) |not (null (ptxps pat))] 
           )

   instance XML Rule where
     mkTag r = Tag "Rule" [mkAttr "ruleId" (name r)]
     mkXmlTree r
      = Elem (mkTag r)
             [Elem (simpleTag "Expression")   [PlainText (showADL (rrexp r))]]
   
   instance XML KeyDef where
     mkTag k = Tag "KeyDef" [nameToAttr k]
     mkXmlTree k = Elem (mkTag k)
                        ( Elem (simpleTag "Key on") [mkXmlTree (kdcpt k)] :
                          attributesTree [e | KeyExp e <- kdats k] -- TODO: currently ignores KeyText and KeyHtml segments
                        )


   instance XML Interface where
     mkTag x = Tag "Interface" [ nameToAttr x]
     mkXmlTree x
           = Elem (mkTag x) []
                      --TODO: moet nog verder uitgewerkt.

   
   instance XML ObjectDef where
     mkTag x = Tag "ObjectDef" [ nameToAttr x]
     mkXmlTree x@(Obj{}) 
           = Elem (mkTag x)
                      ( descriptionTree (objctx x)
                     ++ attributesTree (objatsLegacy x)
                     ++ [Elem (simpleTag "Directives")
                              [PlainText (show (objstrs x))] |not (null (objstrs x))]
                      )    --TODO: De directieven moeten waarschijnlijk nog verder uitgewerkt.


   instance XML Expression where
     mkTag _  = fatal 184 "mkTag should not be used for expressions."
     mkXmlTree expr 
         = case expr of
               (EEqu (l,r))   -> Elem (simpleTag equi) (map mkXmlTree [l,r])
               (EImp (l,r))   -> Elem (simpleTag impl) (map mkXmlTree [l,r])
               (EIsc [])      -> Node (Tag rl [mkAttr "Name" "V"])
               (EIsc [e])     -> mkXmlTree e
               (EIsc es)      -> Elem (simpleTag inter) (map mkXmlTree es)
               (EUni [])      -> Elem (simpleTag compl)  [ Node (Tag rl [mkAttr "Name" "V"])]
               (EUni [e])     -> mkXmlTree e
               (EUni es)      -> Elem (simpleTag union') (map mkXmlTree es)
               (EDif (l,r))   -> Elem (simpleTag diff) (map mkXmlTree [l,r])
               (ELrs (l,r))   -> Elem (simpleTag lres) (map mkXmlTree [l,r])
               (ERrs (l,r))   -> Elem (simpleTag rres) (map mkXmlTree [l,r])
               (ECps [])      -> Node (Tag rl [mkAttr "Name" "I"])
               (ECps [e])     -> mkXmlTree e
               (ECps es)      -> Elem (simpleTag rMul) (map mkXmlTree es)
               (ERad [])      -> Elem (simpleTag compl) [ Node (Tag rl [mkAttr "Name" "I"])]
               (ERad [e])     -> mkXmlTree e
               (ERad es)      -> Elem (simpleTag rAdd) (map mkXmlTree es)
               (EPrd [])      -> Elem (simpleTag compl) [ Node (Tag rl [mkAttr "Name" "I"])]
               (EPrd [e])     -> mkXmlTree e
               (EPrd es)      -> Elem (simpleTag rPrd) (map mkXmlTree es)
               (EKl0 e)       -> Elem (simpleTag clos0) [mkXmlTree e]
               (EKl1 e)       -> Elem (simpleTag clos1') [mkXmlTree e]
               (EFlp e)       -> Elem (simpleTag flip') [mkXmlTree e]
               (ECpl e)       -> Elem (simpleTag compl) [mkXmlTree e]
               (EBrk e)       -> mkXmlTree e
               (ETyp e sgn)   -> Elem (simpleTag cast) [mkXmlTree e,mkXmlTree (source sgn),mkXmlTree (target sgn)]
               (ERel rel)     -> Elem (simpleTag flip') [mkXmlTree (EFlp (ERel rel))]


      where
      (equi,impl,inter,union',diff,lres,rres,rMul,rAdd,rPrd,clos0,clos1',compl,flip',cast,rl)
       = ("EQUI","IMPL","CONJ","DISJ","DIFF","LRES","RRES","RMUL","RADD","RPRD","CLS0","CLS1","CMPL","CONV","CAST","REL")



   instance XML PPurpose where
     mkTag expl =
       Tag "PRef2" atts
        
--        = case expl of
--                PRef2ConceptDef{}  -> Tag "ExplConceptDef"  atts
--                PRef2Declaration{} -> Tag "ExplDeclaration" atts
--                PRef2Rule{}        -> Tag "ExplRule"        atts
--                PRef2KeyDef{}      -> Tag "ExplKeyDef"      atts
--                PRef2Pattern{}     -> Tag "ExplPattern"     atts
--                PRef2Process{}     -> Tag "ExplProcess"     atts
--                PRef2Interface{}   -> Tag "ExplInterface"   atts
--                PRef2Context{}     -> Tag "ExplContext"     atts
--                PRef2Fspc{}        -> Tag "ExplFspc"        atts
           where
            atts ::  [XAtt]
            atts = [mkAttr "Explains" (name expl)
                   ,mkAttr "Markup" (show(pexMarkup expl))
                   ,mkAttr "Ref" (pexRefID expl)]
     mkXmlTree expl 
         = Elem (mkTag expl) [PlainText (show (pexMarkup expl))]

   instance XML Purpose where
     mkTag purp = Tag "Purp" [mkAttr "TODO" "TODO"] 
                           --  [mkAttr "Purpose" (show expl)
                           --  ,mkAttr "Markup" (show (explMarkup expl))
                           --  ,mkAttr "Ref" (explRefId expl)]

--        = case expl of
--                ExplConceptDef  cdef  lang ref _ -> Tag "ExplConceptDef"  (atts cdef lang ref)
--                ExplDeclaration d     lang ref _ -> Tag "ExplDeclaration" (atts (name d++name(source d)++name(target d)) lang ref)
--                ExplRule        rname lang ref _ -> Tag "ExplRule"        (atts rname lang ref)
--                ExplKeyDef      kname lang ref _ -> Tag "ExplKeyDef"      (atts kname lang ref)
--                ExplPattern     pname lang ref _ -> Tag "ExplPattern"     (atts pname lang ref)
--                ExplProcess     pname lang ref _ -> Tag "ExplProcess"     (atts pname lang ref)
--                ExplInterface   cname lang ref _ -> Tag "ExplInterface"   (atts cname lang ref)
--                ExplContext     cname lang ref _ -> Tag "ExplContext"     (atts cname lang ref)
--                ExplFspc        cname lang ref _ -> Tag "ExplFspc"        (atts cname lang ref)
--           where
--            atts :: String -> Lang -> String -> [XAtt]
--            atts str lang ref = [mkAttr "Explains" str
--                                ,mkAttr "Lang" (show lang)
--                                ,mkAttr "Ref" ref]
     mkXmlTree expl 
         = Elem (mkTag expl) [PlainText ((validXML.show.explMarkup) expl)]


   instance XML A_Gen where
     mkTag g = Tag "Gen" (mkAttr "Generic" (show (gengen g))
                          :[mkAttr "Specific" (show (genspc g))]
                         )
     mkXmlTree g = Node (mkTag g) 
   

   instance XML Relation where
     mkTag f = Tag "Relation" [nameToAttr f] 
     mkXmlTree rel = Elem (mkTag rel) 
      (case rel of  
          Rel{} ->  Elem (simpleTag "Source") [mkXmlTree (source rel)]
                    :[Elem (simpleTag "Target") [mkXmlTree (target rel)]]                  
          I{}   ->  [still2bdone "Relation_I"]
          V{}   ->  [still2bdone "Relation_V"]
          Mp1{} ->  [still2bdone "Relation_ONE"]
           ) 


   instance XML Declaration where
     mkTag d = Tag "Association" ([nameToAttr d]
                                ++[ mkAttr "type" t]
                                ++ extraAtts )
            where t = case d of
                        Sgn{} -> "Sgn"
                        Isn{} -> "Isn"
                        Iscompl{} -> "Iscompl"
                        Vs{} -> "Vs"
                  extraAtts = case d of
                                Sgn{} -> [mkAttr "IsSignal" (show (deciss d))]
                                _     -> []
            
     mkXmlTree d = Elem (mkTag d)
        (case d of  
          Sgn{} 
                ->  [Node (Tag "Source" [mkAttr "concept" (name(source d))])]
                  ++[Node (Tag "Target" [mkAttr "concept" (name(target d))])]
                  ++[Elem (simpleTag "MultFrom") [PlainText (multiplicity (multiplicities d))]]
                  ++[Elem (simpleTag "MultTo") [PlainText (multiplicity (map flp (multiplicities d)))]]
                  ++[Elem (simpleTag "Pragma") 
                             [PlainText (show (prL++"%f"++prM++"%t"++prR))] 
                                | not (null (prL++prM++prR))]
                  ++[Elem (simpleTag "Meaning") [PlainText "Still 2 be done"]
                    --         [PlainText (explainContent2String LaTeX True (decMean d))]
                    ]
--                  ++[Elem (simpleTag "Population") 
--                             (map mkXmlTree (decpopu d)) 
--                                | not (null (decpopu d))]                 
          Isn{}
                ->  [Elem (simpleTag "Type") [mkXmlTree (source d)]]
          Iscompl{}
                ->  [Elem (simpleTag "Type") [mkXmlTree (source d)]]
          Vs{}
                ->  Elem (simpleTag "Generic") [mkXmlTree (source d)]
                    :[Elem (simpleTag "Specific")[mkXmlTree (target d)]]
           ) 
       where
         multiplicity ms | null ([Sur,Inj]>-ms) = "1"
                         | null (    [Inj]>-ms) = "0..1"
                         | null ([Sur]    >-ms) = "1..n"
                         | otherwise            = "0..n"
         prL = decprL d
         prM = decprM d
         prR = decprR d

   instance XML Paire where
     mkTag p = Tag "link" atts
                where
                   atts = mkAttr "from" (srcPaire p)
                          :[mkAttr "to"   (trgPaire p)]
     mkXmlTree p = Elem (mkTag p) []
                        
   instance XML ConceptDef where
     mkTag f = Tag "ConceptDef" ( mkAttr "name" (cdcpt f)
                                  : [mkAttr "Trace" (cdref f) |not (null (cdref f))])
     mkXmlTree f = Elem (mkTag f) (explainTree (cddef f))
   

   instance XML A_Concept where
     mkTag f = Tag "A_Concept" [nameToAttr f]
     mkXmlTree f
        = Node (mkTag f)  


   instance XML (ECArule) where
     mkTag _ = Tag "ECArule" []
     mkXmlTree _ = still2bdone "ECArule"
   
   instance XML (Declaration->ECArule) where
     mkTag _ = Tag "ECArule" []
     mkXmlTree _ = still2bdone "Declaration->ECArule"
   
   instance XML PlugSQL where --TODO151210 -> tags for BinSQL and ScalarSQL
     mkTag p = Tag "PlugSql" [ nameToAttr p]
     mkXmlTree p 
      = Elem (mkTag p) 
             [ Elem (simpleTag "Fields") (map mkXmlTree (fields p))]
   instance XML SqlField where
      mkTag x = Tag "Field" (   [mkAttr "name" (fldname x)]
                              ++[mkAttr "type" (showSQL (fldtype x))]
                              ++[mkAttr "null" (show (fldnull x))]
                              ++[mkAttr "uniq" (show (flduniq x))]
                              ++[mkAttr "auto" (show (fldauto x))]
                              )
      mkXmlTree sf = Elem (mkTag sf)
                        [Elem (simpleTag "Expression") [mkXmlTree (fldexpr sf)]]
                        
   attributesTree :: [ObjectDef] -> [XTree]
   attributesTree atts = [Elem (simpleTag "Attributes") 
                               (map mkXmlTree atts)    |not(null atts)]

   descriptionTree :: Expression -> [XTree]
   descriptionTree f = [Elem (simpleTag "Description")
                           [mkXmlTree f] ]

   explainTree :: String -> [XTree]
   explainTree str = [Elem (simpleTag "Explanation")
                           [PlainText (validXML str)] | not (null str)]
                           
                           
   -- | XML has a special set of characters that cannot be used in normal XML strings. 
   validXML :: String -> String
   validXML []       = []
   validXML ('&':s)  = "&amp;"++validXML s
   validXML ('<':s)  = "&lt;"++validXML s
   validXML ('>':s)  = "&gt;"++validXML s
   validXML ('"':s)  = "&quot;"++validXML s
   validXML ('\'':s) = "&#39;"++validXML s
   validXML (c:s)    = c:validXML s