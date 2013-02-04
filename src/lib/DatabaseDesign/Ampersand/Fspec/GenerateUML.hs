{-# OPTIONS_GHC -Wall #-}
module DatabaseDesign.Ampersand.Fspec.GenerateUML (generateUML) where

import DatabaseDesign.Ampersand.Basics
import DatabaseDesign.Ampersand.Misc
import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree (explMarkup,aMarkup2String,Rule,Declaration,Purpose(..))
import DatabaseDesign.Ampersand.Fspec.Graphic.ClassDiagram
import DatabaseDesign.Ampersand.Fspec
import DatabaseDesign.Ampersand.Output.AdlExplanation
import Data.Map (Map)
import Data.List
import qualified Data.Map as Map
import Control.Monad.State.Lazy  (State, gets, evalState, modify)

fatal :: Int -> String -> a
fatal = fatalMsg "Fspec.GenerateUML"

-- TODO: escape
-- TODO: names of model, package, assoc (empty?), etc.

generateUML :: Fspc -> Options -> String
generateUML fSpec flags = showUML (fSpec2UML fSpec flags)

showUML :: UML -> String
showUML uml = unlines $ evalState uml $ UMLState 0 Map.empty [] []

fSpec2UML :: Fspc -> Options -> UML
fSpec2UML fSpec flags = 
 do { packageId0 <- mkUnlabeledId "TopPackage"
    ; packageId1 <- mkUnlabeledId "PackageClasses" 
    ; packageId2 <- mkUnlabeledId "PackageReqs"
    ; diagramId <- mkUnlabeledId "Diagram"
    
    ; _ <- mapM (mkLabeledId "Datatype") datatypeNames
    ; _ <- mapM (mkLabeledId "Class") classNames

    ; datatypesUML <- mapM genUMLDatatype datatypeNames
    ; classesUML <- mapM genUMLClass (classes classDiag)
    ; assocsUML <- mapM genUMLAssociation (assocs classDiag)
    ; requirementsUML <- mapM genUMLRequirement (requirements flags fSpec)
    ; diagramElements <- genDiagramElements 
    ; customProfileElements <- genCustomProfileElements
    ; customReqElements <- genCustomReqElements flags packageId2
    ; return $ [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
               , "<!-- Generated by "++ampersandVersionStr++" -->"
               , "<xmi:XMI xmi:version=\"2.1\" xmlns:uml=\"http://schema.omg.org/spec/UML/2.1\" xmlns:xmi=\"http://schema.omg.org/spec/XMI/2.1\" xmlns:thecustomprofile=\"http://www.sparxsystems.com/profiles/thecustomprofile/1.0\">"
               -- WHY is the exporter not something like `Ampersand` (in the string below)?
               -- BECAUSE then for some reason the importer doesn't show the properties of the requirements.
               , " <xmi:Documentation exporter=\"Enterprise Architect\" exporterVersion=\"6.5\"/>"
               , " <uml:Model xmi:type=\"uml:Model\" name=\""++contextName++"\" visibility=\"public\">"
               , "  <packagedElement xmi:type=\"uml:Package\" xmi:id="++show packageId0++" name="++show contextName++" visibility=\"public\">" ] ++
               [ "   <packagedElement xmi:type=\"uml:Package\" xmi:id="++show packageId1++" name="++show ("classesOf_"++contextName)++" visibility=\"public\">" ] ++
               concat datatypesUML ++
               concat classesUML ++ 
               concat assocsUML ++
               [ "   </packagedElement>" ] ++
               [ "   <packagedElement xmi:type=\"uml:Package\" xmi:id="++show packageId2++" name="++show ("RequirementsOf_"++contextName)++" visibility=\"public\">" ] ++
               concat requirementsUML ++
               [ "   </packagedElement>" ] ++
               [ "  </packagedElement>" ] ++
               customProfileElements ++
               [ " </uml:Model>"
               , " <xmi:Extension extender=\"Enterprise Architect\" extenderID=\"6.5\">"
               , "  <elements>"] ++
               [ "    <element xmi:idref="++show packageId0++" xmi:type=\"uml:Package\" name="++show contextName++" scope=\"public\">"]++
               [ "    </element>"]++
               [ "    <element xmi:idref="++show packageId1++" xmi:type=\"uml:Package\" name="++show ("classesOf_"++contextName)++" scope=\"public\">"]++
               [ "     <model package2="++show packageId1++" package="++show packageId0++" tpos=\"0\" ea_eleType=\"package\"/>"]++
               [ "    </element>"]++
               [ "    <element xmi:idref="++show packageId2++" xmi:type=\"uml:Package\" name="++show ("RequirementsOf_"++contextName)++" scope=\"public\">"]++
               [ "     <model package2="++show packageId2++" package="++show packageId0++" tpos=\"0\" ea_eleType=\"package\"/>"]++
               [ "    </element>"]++
               customReqElements ++
               [ "  </elements>"
               , "  <diagrams>"
               , "   <diagram xmi:id=\""++diagramId++"\">"
               , "    <model package=\""++packageId1++"\" owner=\""++packageId1++"\"/>"
               , "    <properties name=\"Data Model\" type=\"Logical\"/>"
               , "    <elements>" ] ++
               diagramElements ++
               [ "    </elements>"
               , "   </diagram>"
               , "  </diagrams>"
               , " </xmi:Extension>"
               , "</xmi:XMI>" ]
    }
 where classDiag = cdAnalysis fSpec flags
       (contextName, allConcs) = nameandcpts classDiag
       classNames    = [ nm | OOClass nm _ _ <-  classes classDiag]
       datatypeNames = map name allConcs >- classNames

genUMLRequirement :: Req -> UML
genUMLRequirement req =
 do { reqLId <- mkUnlabeledId "Req"
    ; addReqToState (reqLId, req)
    ; return $ [ "    <packagedElement xmi:type=\"uml:Class\" xmi:id=\""++reqLId++"\" name=\""++reqId req++"\" visibility=\"public\"/> " ] 
    }

genUMLDatatype :: String -> UML
genUMLDatatype nm =
 do { datatypeId <- refLabeledId nm
    ; addToDiagram datatypeId
    ; return [ "    <packagedElement xmi:type=\"uml:DataType\" xmi:id=\""++datatypeId++"\" name=\""++nm++"\" visibility=\"public\"/> " ]
    }

genUMLClass :: Class -> UML
genUMLClass (OOClass nm attrs _) =
 do { classId <- refLabeledId nm
    ; addToDiagram classId
    ; attributesUML <- mapM genUMAttribute attrs
    ; return $ [ "    <packagedElement xmi:type=\"uml:Class\" xmi:id=\""++classId++"\" name=\""++nm++"\" visibility=\"public\">"] ++
               concat attributesUML ++
               [ "    </packagedElement>"]
    }

genUMAttribute :: CdAttribute -> UML
genUMAttribute  (OOAttr nm attrType optional) =
 do { attrId <- mkUnlabeledId "Attr"
    ; lIntId <- mkUnlabeledId "Int"
    ; uIntId <- mkUnlabeledId "Int"
    ; classId <- refLabeledId attrType
    ; return [ "       <ownedAttribute xmi:type=\"uml:Property\" xmi:id=\""++attrId++"\" name=\""++nm++"\" visibility=\"public\" isStatic=\"false\""++
                                    " isReadOnly=\"false\" isDerived=\"false\" isOrdered=\"false\" isUnique=\"true\" isDerivedUnion=\"false\">"
             , "        <type xmi:idref=\""++classId++"\"/>"
             , "        <lowerValue xmi:type=\"uml:LiteralInteger\" xmi:id=\""++lIntId++"\" value=\""++(if optional then "0" else "1")++"\"/>"
             , "        <upperValue xmi:type=\"uml:LiteralInteger\" xmi:id=\""++uIntId++"\" value=\"1\"/>"
             , "       </ownedAttribute>"]
    }

genUMLAssociation :: Association -> UML
genUMLAssociation (OOAssoc lType lMults _ rType rMults rRole) =
 do { assocId <- mkUnlabeledId "Assoc"
    ; lMemberAndOwnedEnd <- genMemberAndOwnedEnd lMults assocId lType
    ; rMemberAndOwnedEnd <- genMemberAndOwnedEnd rMults assocId rType
        
    ; return $
        [ "    <packagedElement xmi:type=\"uml:Association\" xmi:id=\""++assocId++"\" name=\""++rRole++"\" visibility=\"public\">"
        ] ++
        lMemberAndOwnedEnd ++
        rMemberAndOwnedEnd ++
        [ "    </packagedElement>"
        ]
    }
 where genMemberAndOwnedEnd (Mult minVal maxVal) assocId typeName =
        do { endId <- mkUnlabeledId "MemberEnd"
           ; typeId <- refLabeledId typeName
           ; lIntId <- mkUnlabeledId "Int"
           ; uIntId <- mkUnlabeledId "Int"
           ; return 
               [ "     <memberEnd xmi:idref=\""++endId++"\"/>"
               , "     <ownedEnd xmi:type=\"uml:Property\" xmi:id=\""++endId++"\" visibility=\"public\" association=\""++assocId++"\" isStatic=\"false\""++
                              " isReadOnly=\"false\" isDerived=\"false\" isOrdered=\"false\" isUnique=\"true\" isDerivedUnion=\"false\" aggregation=\"none\">"
               , "      <type xmi:idref=\""++typeId++"\"/>"
               , "      <lowerValue xmi:type=\"uml:LiteralInteger\" xmi:id=\""++lIntId++"\" value=\""++(if minVal == MinZero then "0" else "1")++"\"/>"
               , case maxVal of
                   MaxOne  -> "      <upperValue xmi:type=\"uml:LiteralInteger\" xmi:id=\""++uIntId++"\" value=\"1\"/>"
                   MaxMany -> "      <upperValue xmi:type=\"uml:LiteralUnlimitedNatural\" xmi:id=\""++uIntId++"\" value=\"-1\"/>"
               , "     </ownedEnd>"
               ]
           }
           
genDiagramElements :: UML
genDiagramElements =
 do { elementIds <- gets diagramEltIds
    ; return [ "      <element subject=\""++elementId++"\"/>" | elementId <- elementIds ]
    }

genCustomProfileElements :: UML
genCustomProfileElements =
 do { reqVals <- gets reqValues
    ; return  [reqUML req | req <- reverse reqVals]
    }
  where
    reqUML :: ReqValue2 -> String
    reqUML (xmiId, req) = intercalate "\n"
     ( ["   <thecustomprofile:Functional base_Requirement="++show xmiId++"/>"]++
       [tagUML xmiId count puprtxt reftxt | (count, (puprtxt, reftxt)) <- zip [0::Int ..] [(aMarkup2String (explMarkup p), explRefId p) | p <- reqPurposes req]]
     )
    tagUML xmiId nr value reftxt = intercalate "\n"
      [ "     <thecustomprofile:"++keyMeaning++" base_Requirement="++show xmiId++" "++keyMeaning++"="++show value++"/>"
      , "     <thecustomprofile:"++keyRef    ++" base_Requirement="++show xmiId++" "++keyRef++"="++show reftxt++"/>"
      ]
        where keyMeaning = "Meaning"++show nr
              keyRef     = "Reference"++show nr
              
genCustomReqElements :: Options -> String -> UML
genCustomReqElements flags parentPackageId =
 do { reqVals <- gets reqValues
    ; return [reqUML req | req <- reverse reqVals]
    }
  where
    reqUML :: ReqValue2 -> String
    reqUML (xmiId, req) = intercalate "\n"
     ([ "    <element xmi:idref="++show xmiId++" xmi:type=\"uml:Requirement\" name="++show (reqId req)++" scope=\"public\""++">"
      , "      <model package="++show parentPackageId++" ea_eleType=\"element\"/>"
      , "      <properties documentation="++show (maybe "" aMarkup2String (meaning (language flags) req))++" isSpecification=\"false\" sType=\"Requirement\" nType=\"0\" scope=\"public\" stereotype=\"Functional\"/>"
      , "      <tags>"]++
      [ "         <tag name=\"Purpose"++nr++"\" value="++show p++" modelElement="++show xmiId++"/>" | (nr ,p) <- zip ("" : map show [1::Int ..]) ([aMarkup2String (explMarkup p) | p <- reqPurposes req])  ]++
      [ "      </tags>"
      , "    </element>"
      ])

-- Requirements

data Req = Req { reqId :: String
            --   , reqRef :: String
               , reqOrig :: Either Rule Declaration
               , reqPurposes :: [Purpose]
               }

instance Meaning Req where
  meaning l r = case reqOrig r of
                  Right rul -> meaning l rul
                  Left  dcl -> meaning l dcl

requirements :: Options -> Fspc -> [Req]
requirements flags fSpec
   = [decl2req d | d <- vrels  fSpec]
   ++[rule2req r | r <- vrules fSpec]
  where
    decl2req d = Req { reqId = name d
                     , reqOrig = Right d
                     , reqPurposes = purposesDefinedIn fSpec (language flags) d
                     }
    rule2req r = Req { reqId = name r
                     , reqOrig = Left r
                     , reqPurposes = purposesDefinedIn fSpec (language flags) r
                     }
                     


-- State and Monad

data UMLState = UMLState { idCounter :: Int
                         , labelIdMap :: Map String String
                         , diagramEltIds :: [String]
                         , reqValues :: [ReqValue2]
                         }
                         
type StateUML a = State UMLState a
                           

type UML = StateUML [String]

type ReqValue2 = ( String -- the xmi-id
                , Req
                )

addToDiagram :: String -> StateUML ()
addToDiagram elementId =
  modify $ \state' -> state' { diagramEltIds = elementId : diagramEltIds state'} 

addReqToState :: ReqValue2 -> StateUML ()
addReqToState reqVal =
  modify $ \state' -> state' { reqValues = reqVal : reqValues state'}

mkUnlabeledId :: String -> StateUML String
mkUnlabeledId tag =
 do { idC <- gets idCounter 
    ; modify $ \state' -> state' { idCounter = idCounter state' + 1}
    ; let unlabeledId = tag++"ID_"++show idC 
    ; return unlabeledId
    }

refLabeledId :: String -> StateUML String
refLabeledId label =
 do { lidMap <- gets labelIdMap
    ; case Map.lookup label lidMap of
          Just lid -> return lid
          Nothing  -> fatal 147 $ "Requesting non-existent label "++label
    }
    
mkLabeledId :: String -> String -> StateUML ()
mkLabeledId tag label =
 do { let classId = tag++"ID_"++label 
    ; modify $ \state' -> state' { labelIdMap =  Map.insert label classId (labelIdMap state') }
    }