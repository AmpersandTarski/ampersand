{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{- | The intentions behind FSpec (SJ 30 dec 2008):
Generation of functional designs is a core functionality of Ampersand.
All items in a specification are generated into the following data structure, FSpec.
It is built by compiling an Ampersand script and translating that to FSpec.
In the future, other ways of 'filling' FSpec are foreseen.
All generators (such as the code generator, the proof generator, the atlas generator, etc.)
are merely different ways to show FSpec.
-}
module Ampersand.FSpec.FSpec
          ( FSpec(..), concDefs, Atom(..), A_Pair(..)
          , Quad(..)
          , PlugSQL(..),plugAttributes
          , lookupCpt, getConceptTableFor
          , RelStore(..)
          , metaValues
          , SqlAttribute(..),isPrimaryKey,isForeignKey
          , Typology(..)
          , Interface(..)
          , Object(..)
          , BoxItem(..)
          , SubInterface(..)
          , PlugInfo(..)
          , SqlAttributeUsage(..)
          , Conjunct(..),DnfClause(..), dnf2expr, notCpl
          , Language(..)
          , defOutputLang
          , showSQL
          , substituteReferenceObjectDef
          , violationsOfInvariants
          ) where
import           Ampersand.ADL1
import           Ampersand.Basics
import           Ampersand.Classes
import           Ampersand.FSpec.Crud
import           Data.Hashable
import qualified RIO.NonEmpty as NE
import qualified RIO.Set as Set
import qualified RIO.Text as T 
import qualified RIO.List as L
import           Text.Pandoc.Builder (Blocks)

data FSpec = FSpec { fsName ::       Text
                   -- ^ The name of the specification, taken from the Ampersand script
                   , originalContext :: A_Context             
                   -- ^ the original context. (for showA)  
                   , fspos ::        [Origin]                 
                   -- ^ The origin of the FSpec. An FSpec can be a merge of a file including other files c.q. a list of Origin.
                   , plugInfos ::    [PlugInfo]               
                   -- ^ All plugs (derived)
                   , interfaceS ::   [Interface]              
                   -- ^ All interfaces defined in the Ampersand script
                   , interfaceG ::   [Interface]              
                   -- ^ All interfaces derived from the basic ontology (the Lonneker interface)
                   , roleInterfaces  :: Role -> [Interface]   
                   -- ^ All interfaces defined in the Ampersand script, for use by a specific Role
                   , fDeriveProofs :: Blocks                  
                   -- ^ The proofs in Pandoc format
                   , fRoleRuls ::    [(Role,Rule)]            
                   -- ^ the relation saying which roles maintain which rules.
                   , fMaintains ::   Role -> Rules
                   , fRoles ::       [(Role,Int)]
                   -- ^ All roles mentioned in this context, numbered.
                   , fallRules ::    Rules
                   , vrules ::       Rules                   
                   -- ^ All user defined rules that apply in the entire FSpec
                   , grules ::       Rules                   
                   -- ^ All rules that are generated: multiplicity rules and identity rules
                   , invariants ::   Rules                   
                   -- ^ All invariant rules
                   , signals ::      Rules                   
                   -- ^ All signal rules
                   , allUsedDecls :: Relations               
                   -- ^ All relations that are used in the fSpec
                   , vrels ::        Relations               
                   -- ^ All user defined and generated relations plus all defined and computed totals.
                                                              --   The generated relations are all generalizations and
                                                              --   one relation for each signal.
                   , allConcepts ::  A_Concepts
                   -- ^ All concepts in the fSpec
                   , cptTType :: A_Concept -> TType 
                   , vIndices ::     [IdentityDef]            
                   -- ^ All keys that apply in the entire FSpec
                   , vviews ::       [ViewDef]                
                   -- ^ All views that apply in the entire FSpec
                   , getDefaultViewForConcept :: A_Concept -> Maybe ViewDef
                   , getAllViewsForConcept :: A_Concept -> [ViewDef]
                   , lookupView :: ViewUsage -> ViewDef            
                   -- ^ Lookup view by id in fSpec.
                   , vgens ::        [AClassify]              
                   -- ^ All gens that apply in the entire FSpec
                   , allConjuncts :: [Conjunct]               
                   -- ^ All conjuncts generated (by ADL2FSpec)
                   , allConjsPerRule :: [(Rule,NE.NonEmpty Conjunct)]   
                   -- ^ Maps each rule onto the conjuncts it consists of (note that a single conjunct may be part of several rules) 
                   , allConjsPerDecl :: [(Relation, [Conjunct])]        
                   -- ^ Maps each relation to the conjuncts it appears in   
                   , allConjsPerConcept :: [(A_Concept, [Conjunct])]    
                   -- ^ Maps each concept to the conjuncts it appears in (as source or target of a constituent relation)
                   , vquads ::       [Quad]                   
                   -- ^ All quads generated (by ADL2FSpec)
                   , fsisa ::        [(A_Concept, A_Concept)] 
                   -- ^ generated: The data structure containing the generalization structure of concepts
                   , vpatterns ::    [Pattern]                
                   -- ^ All patterns taken from the Ampersand script
                   , conceptDefs ::  [ConceptDef]             
                   -- ^ All concept definitions defined throughout a context, including those inside patterns and processes
                   , fSexpls ::      [Purpose]                
                   -- ^ All purposes that have been declared at the top level of the current specification, but not in the processes, patterns and interfaces.
                   , metas ::        [Meta]                   
                   -- ^ All meta relations from the entire context
                   , crudInfo ::     CrudInfo                 
                   -- ^ Information for CRUD matrices 
                   , atomsInCptIncludingSmaller :: A_Concept -> AAtomValues 
                   -- ^ All user defined populations of an A_concept, INCLUDING the populations of smaller A_Concepts
                   , atomsBySmallestConcept :: A_Concept -> AAtomValues     
                   -- ^ All user defined populations of an A_Concept, where a population is NOT listed iff it also is in a smaller A_Concept.
                   , tableContents :: PlugSQL -> [[Maybe AAtomValue]] 
                   -- ^ tableContents is meant to compute the contents of an entity table.
                   --   It yields a list of records. Values in the records may be absent, which is why Maybe is used rather than Text.
                   -- SJ 2016-05-06: Why is that? `tableContents` should represent a set of atoms, so `Maybe` should have no part in this. Why is Maybe necessary?
                   -- HJO 2016-09-05: Answer: Broad tables may contain rows where some of the attributes implement a relation that is UNI, but not TOT. In such case,
                   --                         we may see empty attributes. (NULL values in database terminology)
                   -- 'tableContents fSpec plug' is used in `PHP.hs` for filling the database initially.
                   -- 'tableContents fSpec plug' is used in `Population2Xlsx.hs` for filling a spreadsheet.
                   , pairsInExpr :: Expression -> AAtomPairs
                   , applyViolText :: Rule -> AAtomPair -> Text
                   , initialConjunctSignals :: [(Conjunct,AAtomPairs)] 
                   -- ^ All conjuncts that have process-rule violations.
                   , allViolations ::  [(Rule,AAtomPairs)]   
                   -- ^ All invariant rules with violations.
                   , allExprs ::     Expressions             
                   -- ^ All expressions in the fSpec
                   , fcontextInfo   :: ContextInfo 
                   , ftypologies   :: [Typology]
                   , typologyOf :: A_Concept -> Typology
                   , largestConcept :: A_Concept -> A_Concept
                   , specializationsOf :: A_Concept -> [A_Concept]    
                   , generalizationsOf :: A_Concept -> [A_Concept]
                   } deriving Typeable
instance Eq FSpec where
 f == f' = originalContext f == originalContext f'
instance Unique FSpec where
 showUnique = showUnique . originalContext
metaValues :: Text -> FSpec -> [Text]
metaValues key fSpec = [mtVal m | m <-metas fSpec, mtName m == key]

-- The point of calculating a hash for FSpec is that such a hash can be used 
-- at runtime to determine if the database structure and content is still valid.
-- We want to detect if the user has made changes in her script, that require
-- to reinstall the database. (e.g. changing the order of things in the script
-- would not require a change of the database. However, change the name of concepts would.)
instance Hashable FSpec where
    hashWithSalt salt fSpec = salt 
      `composeHash` name
      `composeHash` (L.sort . Set.toList . fallRules) 
      `composeHash` (L.sort . Set.toList . vrels)
      `composeHash` (L.sort . Set.toList . allConcepts)
      `composeHash` (L.sortBy (compare `on` genspc) . vgens)
      where 
        composeHash :: Hashable a => Int -> (FSpec -> a) -> Int
        composeHash s fun = s `hashWithSalt` fun fSpec 

instance Language FSpec where
  relsDefdIn = relsDefdIn.originalContext
  udefrules  = udefrules.originalContext
  identities = identities.originalContext
  viewDefs   = viewDefs.originalContext
  gens       = gens.originalContext
  patterns   = patterns.originalContext

data Atom = Atom { atmRoots :: [A_Concept] -- The root concept(s) of the atom.
                 , atmIn ::    [A_Concept] -- all concepts the atom is in. (Based on generalizations)
                 , atmVal   :: AAtomValue
                 } deriving (Typeable,Eq)
instance Unique Atom where
  showUnique a = showValADL (atmVal a)<>" in "
         <>case atmRoots a of
             []  -> fatal "an atom must have at least one root concept"
             [x] -> uniqueShowWithType x
             xs  -> "["<>T.intercalate ", " (map uniqueShowWithType xs)<>"]"

data A_Pair = Pair { lnkDcl :: Relation
                   , lnkLeft :: Atom
                   , lnkRight :: Atom
                   } deriving (Typeable,Eq)
instance HasSignature A_Pair where
  sign = sign . lnkDcl
instance Unique A_Pair where
  showUnique x = showUnique (lnkDcl x)
              <> showUnique (lnkLeft x)
              <> showUnique (lnkRight x)
concDefs :: FSpec -> A_Concept -> [ConceptDef]
concDefs fSpec c = 
  case c of
    ONE -> []
    PlainConcept{} -> [ cdef | cdef<-conceptDefs fSpec, name cdef `elem` aliases c ]

instance ConceptStructure FSpec where
  concs         = allConcepts
  expressionsIn = allExprs 


--type Attributes = [Attribute]
--data Attribute  = Attr { fld_name :: Text        -- The name of this attribute
--                       , fld_sub :: Attributes        -- all sub-attributes
--                       , fld_expr :: Expression    -- The expression by which this attribute is attached to the interface
--                       , fld_rel :: Relation      -- The relation to which the database table is attached.
--                       , fld_editable :: Bool          -- can this attribute be changed by the user of this interface?
--                       , fld_list :: Bool          -- can there be multiple values in this attribute?
--                       , fld_must :: Bool          -- is this attribute obligatory?
--                       , fld_new :: Bool          -- can new elements be filled in? (if no, only existing elements can be selected)
--                       , fld_sLevel :: Int           -- The (recursive) depth of the current servlet wrt the entire interface. This is used for documentation.
--                       , fld_insAble :: Bool          -- can the user insert in this attribute?
--                       , fld_onIns :: ECArule       -- the PAclause to be executed after an insert on this attribute
--                       , fld_delAble :: Bool          -- can the user delete this attribute?
--                       , fld_onDel :: ECArule       -- the PAclause to be executed after a delete on this attribute
--                       }

{- from http://www.w3.org/TR/wsdl20/#InterfaceOperation
 - "The properties of the Interface Operation component are as follows:
 - ...
 - * {interface message references} OPTIONAL. A set of Interface Message Reference components for the ordinary messages the operation accepts or sends.
 - ..."
-}

instance Named FSpec where
  name = fsName

data Quad = Quad { qDcl ::       Relation   -- The relation that, when affected, triggers a restore action.
                 , qRule ::      Rule          -- The rule from which qConjuncts is derived.
                 , qConjuncts :: NE.NonEmpty Conjunct    -- The conjuncts, with clauses included
                 } deriving Show

instance Ord Quad where
  q `compare` q'  = (qDcl q,qRule q) `compare` (qDcl q',qRule q')
instance Eq Quad where
  a == b = compare a b == EQ

--
dnf2expr :: DnfClause -> Expression
dnf2expr dnf =
  case (antcs dnf, conss dnf) of
    ([],[]) -> fatal ("empty dnf clause in "<>tshow dnf)
    ([],hc:tlc ) -> foldr (.\/.) hc tlc
    (ha:tla,[]) -> notCpl (foldr (./\.) ha tla)
    (ha:tla,hc:tlc) -> notCpl (foldr (./\.) ha tla) .\/. foldr (.\/.) hc tlc
newtype PlugInfo = InternalPlug PlugSQL
                deriving (Show, Eq,Typeable)
instance Named PlugInfo where
  name (InternalPlug psql) = name psql
instance Unique PlugInfo where
  showUnique (InternalPlug psql) = "SQLTable "<>name psql
instance ConceptStructure PlugInfo where
  concs   (InternalPlug psql) = concs   psql
  expressionsIn (InternalPlug psql) = expressionsIn psql
instance ConceptStructure PlugSQL where
  concs     p = concs   (plugAttributes p)
  expressionsIn   p = expressionsIn (plugAttributes p)

data PlugSQL
   -- | stores a related collection of relations: a kernel of concepts and attribute relations of this kernel
   --   i.e. a list of SqlAttribute given some A -> [target r | r::A*B,isUni r,isTot r, isInj r]
   --                                            <> [target r | r::A*B,isUni r, not(isTot r), not(isSur r)]
   --     kernel = A closure of concepts A,B for which there exists a r::A->B[INJ]
   --              (r=attExpr of kernel attribute holding instances of B, in practice r is I or a makeRelation(flipped relation))
   --      attribute relations = All concepts B, A in kernel for which there exists a r::A*B[UNI] and r not TOT and SUR
   --              (r=attExpr of attMor attribute, in practice r is a makeRelation(relation))
 = TblSQL  { sqlname ::    Text
           , attributes :: [SqlAttribute]                           -- ^ the first attribute is the concept table of the most general concept (e.g. Person)
                                                                    --   then follow concept tables of specializations. Together with the first attribute this is called the "kernel"
                                                                    --   the remaining attributes represent attributes.
           , cLkpTbl ::    [(A_Concept,SqlAttribute)]               -- ^ lookup table that links all typology concepts to attributes in the plug
                                                                    -- cLkpTbl is een lijst concepten die in deze plug opgeslagen zitten, en hoe je ze eruit kunt halen
           , dLkpTbl ::   [RelStore]
           }
   -- | stores one relation r in two ordered columns
   --   i.e. a tuple of SqlAttribute -> (source r,target r) with (attExpr=I/\r;r~, attExpr=r)
   --   (note: if r TOT then (I/\r;r~ = I). Thus, the concept (source r) is stored in this plug too)
   --   with tblcontents = [[Just x,Just y] |(x,y)<-contents r].
   --   Typical for BinSQL is that it has exactly two columns that are not unique and may not contain NULL values
 | BinSQL  { sqlname :: Text
           , cLkpTbl :: [(A_Concept,SqlAttribute)] 
           , dLkpTbl :: [RelStore]
           }
   deriving (Show, Typeable)

instance Named PlugSQL where
  name = sqlname
instance Eq PlugSQL where
  a == b = compare a b == EQ
instance Unique PlugSQL where
  showUnique = name
instance Ord PlugSQL where
  compare x y = compare (name x) (name y)

plugAttributes :: PlugSQL-> NE.NonEmpty SqlAttribute
plugAttributes plug = case plug of
    TblSQL{}    -> case attributes plug of
                     [] -> fatal "attributes should contain at least one element" -- FIXME: change type of attributes to  attributes :: NE.NonEmpty SqlAttribute
                     h:tl -> h NE.:| tl
    BinSQL{}    -> let store = case dLkpTbl plug of
                         [x] -> x
                         _   -> fatal $ "Relation lookup table of a binary table should contain exactly one element:\n" <>
                                            tshow (dLkpTbl plug)
                   in rsSrcAtt store NE.:| [rsTrgAtt store]

-- | This returns all column/table pairs that serve as a concept table for cpt. When adding/removing atoms, all of these
-- columns need to be updated
lookupCpt :: FSpec -> A_Concept -> [(PlugSQL,SqlAttribute)]
lookupCpt fSpec cpt = [(plug,att) 
                      |InternalPlug plug<-plugInfos fSpec
                      , (c,att)<-cLkpTbl plug
                      , c==cpt
                      ]

-- Convenience function that returns the name of the table that contains the concept table (or more accurately concept column) for c
getConceptTableFor :: FSpec -> A_Concept -> PlugSQL
getConceptTableFor fSpec c = case lookupCpt fSpec c of
                               []      -> fatal $ "tableFor: No concept table for " <> name c
                               (t,_):_ -> t -- in case there are more, we use the first one

-- | Information about the source and target attributes of a relation in an sqlTable. The relation could be stored either flipped or not.  
data RelStore 
  = RelStore
     { rsDcl       :: Relation
     , rsStoredFlipped :: Bool
     , rsSrcAtt    :: SqlAttribute
     , rsTrgAtt    :: SqlAttribute
     } deriving (Show, Typeable)
data SqlAttributeUsage = PrimaryKey A_Concept
                       | ForeignKey A_Concept  -- The SQL-attribute is a reference (containing the primary key value of) a TblSQL
                       | PlainAttr             -- None of the above
                       deriving (Eq, Show)

data SqlAttribute = Att { attName ::    Text
                        , attExpr ::    Expression     -- ^ De target van de expressie geeft de waarden weer in de SQL-tabel-kolom.
                        , attType ::    TType
                        , attUse ::     SqlAttributeUsage
                        , attNull ::    Bool           -- ^ True if there can be NULL-values in the SQL-attribute (intended for data dictionary of DB-implementation)
                        , attDBNull ::  Bool           -- ^ True for all fields, to disable strict checking by the database itself. 
                        , attUniq ::    Bool           -- ^ True if all values in the SQL-attribute are unique? (intended for data dictionary of DB-implementation)
                        , attFlipped :: Bool
                        } deriving (Eq, Show,Typeable)
instance Named SqlAttribute where
  name = attName
instance Unique (PlugSQL,SqlAttribute) where
  showUnique (p,f) = showUnique p<>"."<>attName f
instance Ord SqlAttribute where
  compare x y = compare (attName x) (attName y)
instance ConceptStructure SqlAttribute where
  concs     f = Set.fromList [target e' |let e'=attExpr f,isSur e']
  expressionsIn   f = expressionsIn   (attExpr f)

isPrimaryKey :: SqlAttribute -> Bool
isPrimaryKey att = case attUse att of
                    PrimaryKey _ -> True
                    _ -> False
isForeignKey :: SqlAttribute -> Bool
isForeignKey att = case attUse att of
                    ForeignKey _ -> True
                    _ -> False

showSQL :: TType -> Text
showSQL tt =
  case tt of 
     Alphanumeric     -> "VARCHAR(255)"
     BigAlphanumeric  -> "TEXT"
     HugeAlphanumeric -> "MEDIUMTEXT"
     Password         -> "VARCHAR(255)"
     Binary           -> "BLOB"
     BigBinary        -> "MEDIUMBLOB"
     HugeBinary       -> "LONGBLOB"
     Date             -> "DATE"
     DateTime         -> "DATETIME"
     Boolean          -> "BOOLEAN"
     Integer          -> "BIGINT"
     Float            -> "FLOAT"
     Object           -> "VARCHAR(255)"
     TypeOfOne        -> fatal "ONE is not represented in SQL" 

-- In case of reference to an INTERFACE, not used as a LINKTO, the
-- expression and cruds are replaced. This is introduced with the
-- refactoring of the frontend interfaces in oct/nov 2016. 
substituteReferenceObjectDef :: FSpec -> ObjectDef -> ObjectDef
substituteReferenceObjectDef fSpec originalObjectDef =
  case substitution of
    Nothing           -> originalObjectDef
    Just (expr,cruds) -> originalObjectDef
                            { objExpression  = expr
                            , objcrud = cruds
                            }
  where
    substitution :: Maybe (Expression, Cruds)
    substitution =
      case objmsub originalObjectDef of
        Just InterfaceRef{ siIsLink=False
                          , siIfcId=interfaceId} 
          -> let ifc = substituteReferenceObjectDef fSpec (ifcObj (lookupInterface interfaceId))
              in Just (objExpression originalObjectDef .:. objExpression ifc, objcrud ifc)
        _ -> Nothing
    lookupInterface :: Text -> Interface
    lookupInterface nm = 
        case [ ifc | ifc <- interfaceS fSpec <> interfaceG fSpec, name ifc == nm ] of
          [ifc] -> ifc
          _     -> fatal "Interface lookup returned zero or more than one result"

violationsOfInvariants :: FSpec -> [(Rule,AAtomPairs)]
violationsOfInvariants fSpec 
  = [(r,vs) |(r,vs) <- allViolations fSpec
            , not (isSignal r)
    ]
defOutputLang :: FSpec -> Lang
defOutputLang = ctxlang . originalContext
