{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE UndecidableInstances #-}
module Ampersand.Core.AbstractSyntaxTree (
   A_Context(..)
 , Typology(..)
 , Meta(..)
 , Origin(..)
 , Pattern(..) 
 , PairView(..)
 , PairViewSegment(..)
 , Rule(..), Rules
 , RuleOrigin(..)
 , Relation(..), Relations, showRel
 , IdentityDef(..)
 , IdentitySegment(..)
 , ViewDef(..)
 , ViewSegment(..)
 , ViewSegmentPayLoad(..)
 , AClassify(..)
 , Interface(..)
 , getInterfaceByName
 , SubInterface(..)
 , BoxItem(..),ObjectDef(..),BoxTxt(..)
 , ViewUsage(..)
 , Object(..)
 , Cruds(..)
 , Default(..)
 , Purpose(..)
 , ExplObj(..)
 , Expression(..)
 , getExpressionRelation
 , A_Concept(..), A_Concepts
 , ShowWithAliases(..)
 , Meaning(..)
 , A_RoleRule(..)
 , Representation(..), TType(..)
 , unsafePAtomVal2AtomValue, safePSingleton2AAtomVal
 , Signature(..)
 , Population(..)
 , HasSignature(..)
 , Prop(..),Traced(..)
 , Conjunct(..), DnfClause(..)
 , AAtomPair(..), AAtomPairs
 , AAtomValue(..), AAtomValues, mkAtomPair, PAtomValue(..)
 , ContextInfo(..)
 , showValADL,showValSQL
 , showSign
 , SignOrd(..), Type(..), typeOrConcept
-- , module Ampersand.Core.ParseTree  -- export all used constructors of the parsetree, because they have actually become part of the Abstract Syntax Tree.
 , (.==.), (.|-.), (./\.), (.\/.), (.-.), (./.), (.\.), (.<>.), (.:.), (.!.), (.*.)
 , makeConceptMap, ConceptMap
 ) where
import           Ampersand.Basics
import           Ampersand.Core.ParseTree 
    ( Meta(..)
    , Role(..)
    , ConceptDef, P_Concept(..), mkPConcept, PClassify(specific,generics)
    , Origin(..)
    , maybeOrdering
    , Traced(..)
    , HtmlTemplateSpec(..)
    , ViewUsage(..)
    , HTMLTemplateUsage(..) -- , TemplateKeyValue(..)
    , PairView(..)
    , PairViewSegment(..)
    , Prop(..), Props
    , Representation(..), TType(..), PAtomValue(..)
    )
import           Ampersand.ADL1.Lattices (Op1EqualitySystem)
import           Data.Default       (Default(..))
import           Data.Hashable      (Hashable(..),hashWithSalt)
import           Data.Typeable (typeOf)
import           RIO.Char           (toUpper)
import qualified RIO.List as L
import qualified RIO.Map as Map
import qualified RIO.NonEmpty as NE
import qualified RIO.Set as Set
import qualified RIO.Text as T
import           RIO.Time

data A_Context
   = ACtx{ ctxnm :: Text           -- ^ The name of this context
         , ctxpos :: [Origin]        -- ^ The origin of the context. A context can be a merge of a file including other files c.q. a list of Origin.
         , ctxlang :: Lang           -- ^ The default language used in this context.
         , ctxmarkup :: PandocFormat -- ^ The default markup format for free text in this context.
         , ctxpats :: [Pattern]      -- ^ The patterns defined in this context
         , ctxrs :: Rules           -- ^ All user defined rules in this context, but outside patterns and outside processes
         , ctxds :: Relations        -- ^ The relations that are declared in this context, outside the scope of patterns
         , ctxpopus :: [Population]  -- ^ The user defined populations of relations defined in this context, including those from patterns and processes
         , ctxcds :: [ConceptDef]    -- ^ The concept definitions defined in this context, including those from patterns and processes
         , ctxks :: [IdentityDef]    -- ^ The identity definitions defined in this context, outside the scope of patterns
         , ctxrrules :: [A_RoleRule]
         , ctxreprs :: A_Concept -> TType
         , ctxvs :: [ViewDef]        -- ^ The view definitions defined in this context, outside the scope of patterns
         , ctxgs :: [AClassify]          -- ^ The specialization statements defined in this context, outside the scope of patterns
         , ctxgenconcs :: [[A_Concept]] -- ^ A partitioning of all concepts: the union of all these concepts contains all atoms, and the concept-lists are mutually distinct in terms of atoms in one of the mentioned concepts
         , ctxifcs :: [Interface]    -- ^ The interfaces defined in this context
         , ctxps :: [Purpose]        -- ^ The purposes of objects defined in this context, outside the scope of patterns and processes
         , ctxmetas :: [Meta]        -- ^ used for Pandoc authors (and possibly other things)
         , ctxInfo :: ContextInfo
         } deriving (Typeable)              --deriving (Show) -- voor debugging
instance Show A_Context where
  show = T.unpack . name
instance Eq A_Context where
  c1 == c2  =  name c1 == name c2
instance Unique A_Context where
  showUnique = name
instance Named A_Context where
  name  = ctxnm

data Pattern
   = A_Pat { ptnm ::  Text        -- ^ Name of this pattern
           , ptpos :: Origin        -- ^ the position in the file in which this pattern was declared.
           , ptend :: Origin        -- ^ the end position in the file, elements with a position between pos and end are elements of this pattern.
           , ptrls :: Rules         -- ^ The user defined rules in this pattern
           , ptgns :: [AClassify]   -- ^ The generalizations defined in this pattern
           , ptdcs :: Relations     -- ^ The relations that are declared in this pattern
           , ptups :: [Population]  -- ^ The user defined populations in this pattern
           , ptids :: [IdentityDef] -- ^ The identity definitions defined in this pattern
           , ptvds :: [ViewDef]     -- ^ The view definitions defined in this pattern
           , ptxps :: [Purpose]     -- ^ The purposes of elements defined in this pattern
           }   deriving (Typeable)  -- Show for debugging purposes
instance Eq Pattern where
  a == b = compare a b == EQ
instance Unique Pattern where
  showUnique = name
instance Ord Pattern where
 a `compare` b = name a `compare` name b
instance Named Pattern where
 name = ptnm
instance Traced Pattern where
 origin = ptpos


data A_RoleRule = A_RoleRule { arPos ::   Origin
                             , arRoles :: NE.NonEmpty Role
                             , arRules :: NE.NonEmpty Text -- the names of the rules
                             } deriving (Show)
instance Ord A_RoleRule where
 compare a b = fromMaybe (fatal . T.intercalate "\n" $
                        ["PPurpose a should have a non-fuzzy Origin."
                        , tshow (origin a)
                        , tshow (origin b)
                        ])
                     (maybeOrdering (origin a) (origin b))
instance Eq A_RoleRule where
 p1 == p2 = compare p1 p2 == EQ
instance Traced A_RoleRule where
  origin = arPos
data RuleOrigin = UserDefined     -- This rule was specified explicitly as a rule in the Ampersand script
                | Multiplicity    -- This rule follows implicitly from the Ampersand script (Because of a property) and generated by a computer
                | Identity        -- This rule follows implicitly from the Ampersand script (Because of a identity) and generated by a computer
                deriving (Show, Eq)
type Rules = Set.Set Rule
data Rule =
     Ru { rrnm ::     Text                        -- ^ Name of this rule
        , formalExpression :: Expression          -- ^ The expression that should be True
        , rrfps ::    Origin                      -- ^ Position in the Ampersand file
        , rrmean ::   [Meaning]                   -- ^ Ampersand generated meaning (for all known languages)
        , rrmsg ::    [Markup]                    -- ^ User-specified violation messages, possibly more than one, for multiple languages.
        , rrviol ::   Maybe (PairView Expression) -- ^ Custom presentation for violations, currently only in a single language
        , rrdcl ::    Maybe (Prop,Relation)       -- ^ The property, if this rule originates from a property on a Relation
        , rrpat ::    Maybe Text                  -- ^ If the rule is defined in the context of a pattern, the name of that pattern.
        , r_usr ::    RuleOrigin                  -- ^ Where does this rule come from?
        , isSignal :: Bool                        -- ^ True if this is a signal; False if it is an invariant
        } deriving Typeable
instance Eq Rule where
  a == b = compare a b == EQ
instance Unique Rule where
  showUnique = name
instance Ord Rule where
  compare = compare `on` rrnm  -- Origin should not be here: A check that they all have unique names is done before typechecking.
instance Show Rule where
  show x
   = "RULE "<> T.unpack (if T.null (name x) then mempty else name x <> ": ") <> show (formalExpression x)
instance Traced Rule where
  origin = rrfps
instance Named Rule where
  name   = rrnm
instance Hashable Rule where
  hashWithSalt s rul = s 
    `hashWithSalt` name rul
    `hashWithSalt` formalExpression rul

data Conjunct = Cjct { rc_id ::         Text -- string that identifies this conjunct ('id' rather than 'name', because
                                               -- this is an internal id that has no counterpart at the ADL level)
                     , rc_orgRules ::   NE.NonEmpty Rule -- All rules this conjunct originates from
                     , rc_conjunct ::   Expression
                     , rc_dnfClauses :: [DnfClause]
                     } deriving (Show,Typeable)

data DnfClause = Dnf { antcs :: [Expression]
                     , conss :: [Expression]
                     }  deriving (Show, Eq) -- Show is for debugging purposes only.

{- The intended semantics of |Dnf ns ps| is the disjunction |foldr1 ( .\/. ) (map notCpl ns <> ps)|.
   The list |ns| and |ps| are not guaranteed to be sorted or duplicate-free.
-}

instance Eq Conjunct where
  a == b = compare a b == EQ
instance Unique Conjunct where
  showUnique = rc_id
instance Ord Conjunct where
  compare = compare `on` rc_id

type Relations = Set.Set Relation
data Relation = Relation
      { decnm :: Text              -- ^ the name of the relation
      , decsgn :: Signature          -- ^ the source and target concepts of the relation
       --properties returns decprps_calc, when it has been calculated. So if you only need the user defined properties do not use 'properties' but 'decprps'.
      , decprps :: Props            -- ^ the user defined multiplicity properties (Uni, Tot, Sur, Inj) and algebraic properties (Sym, Asy, Trn, Rfx)
      , decprps_calc :: Maybe Props -- ^ the calculated and user defined multiplicity properties (Uni, Tot, Sur, Inj) and algebraic properties (Sym, Asy, Trn, Rfx, Irf). Note that calculated properties are made by adl2fspec, so in the A-structure decprps and decprps_calc yield exactly the same answer.
      , decprL :: Text             -- ^ three strings, which form the pragma. E.g. if pragma consists of the three strings: "Person ", " is married to person ", and " in Vegas."
      , decprM :: Text             -- ^    then a tuple ("Peter","Jane") in the list of links means that Person Peter is married to person Jane in Vegas.
      , decprR :: Text
      , decMean :: [Meaning]          -- ^ the meaning of a relation, for each language supported by Ampersand.
      , decfpos :: Origin            -- ^ the position in the Ampersand source file where this declaration is declared. Not all declarations come from the ampersand souce file.
      , decusr ::  Bool              -- ^ if true, this relation is declared by an author in the Ampersand script; otherwise it was generated by Ampersand.
      , decpat ::  Maybe Text      -- ^ If the relation is declared inside a pattern, the name of that pattern.
      , dechash :: Int
      } deriving (Typeable, Data)

instance Eq Relation where
  a == b = compare a b == EQ
instance Ord Relation where
  compare a b = compare (name a, sign a) (name b, sign b)
instance Unique Relation where
  showUnique d =
    name d<>showUnique (decsgn d)
instance Hashable Relation where
   hashWithSalt s Relation{dechash = v} = s `hashWithSalt` v
instance Show Relation where  -- For debugging purposes only (and fatal messages)
  show decl
   = T.unpack $ name decl<>showSign (sign decl)

showRel :: Relation -> Text
showRel rel = name rel<>"["<>tshow (source rel) <> "*"<> tshow (target rel)<>"]"

newtype Meaning = Meaning { ameaMrk ::Markup} deriving (Show, Eq, Ord, Typeable, Data)
instance Unique Meaning where
  showUnique = tshow

instance Named Relation where
  name = decnm
instance HasSignature Relation where
  sign = decsgn
instance Traced Relation where
  origin = decfpos

data IdentityDef = Id { idPos :: Origin        -- ^ position of this definition in the text of the Ampersand source file (filename, line number and column number).
                      , idLbl :: Text        -- ^ the name (or label) of this Identity. The label has no meaning in the Compliant Service Layer, but is used in the generated user interface. It is not an empty string.
                      , idCpt :: A_Concept     -- ^ this expression describes the instances of this object, related to their context
                      , idPat :: Maybe Text  -- ^ if defined within a pattern, then the name of that pattern.
                      , identityAts :: NE.NonEmpty IdentitySegment  -- ^ the constituent attributes (i.e. name/expression pairs) of this identity.
                      } deriving (Show)
instance Named IdentityDef where
  name = idLbl
instance Traced IdentityDef where
  origin = idPos
instance Unique IdentityDef where
  showUnique = name
instance Ord IdentityDef where
  compare a b = name a `compare` name b
instance Eq IdentityDef where
  a == b = compare a b == EQ
newtype IdentitySegment = IdentityExp 
         { segment :: ObjectDef
         } deriving (Eq, Show)  -- TODO: refactor to a list of terms

data ViewDef = ViewDef
    { vdpos :: Origin
    -- ^ position of this definition in the text of the Ampersand source file (filename, line number and column number).
    , vdlbl :: Text
    -- ^ the name (or label) of this View. The label has no meaning in the Compliant Service Layer, but is used in the generated user interface. It is not an empty string.
    , vdcpt :: A_Concept
    -- ^ the concept for which this view is applicable
    , vdIsDefault :: Bool
    -- ^ whether or not this is the default view for the concept
    , vdhtml :: Maybe HtmlTemplateSpec
    -- ^ the html template for this view (not required since we may have other kinds of views as well in the future)
    , vdats :: [ViewSegment]
    -- ^ the constituent attributes (i.e. name/expression pairs) of this view.
    } deriving (Show)
instance Named ViewDef where
  name = vdlbl
instance Traced ViewDef where
  origin = vdpos
instance Unique ViewDef where
  showUnique vd = name vd<>"_"<>name (vdcpt vd) 
instance Eq ViewDef where
  a == b = compare a b == EQ
instance Ord ViewDef where
  a `compare` b = (vdlbl a,vdcpt a) `compare` (vdlbl b, vdcpt b)
data ViewSegment = ViewSegment
     { vsmpos :: Origin
     , vsmlabel :: Maybe Text
     , vsmSeqNr :: Integer
     , vsmLoad  :: ViewSegmentPayLoad
     } deriving (Show, Data)
instance Traced ViewSegment where
  origin = vsmpos
data ViewSegmentPayLoad
                 = ViewExp { vsgmExpr :: Expression
                           }
                 | ViewText{ vsgmTxt  :: Text
                           }deriving (Eq, Show, Data)


-- | data structure AClassify contains the CLASSIFY statements from an Ampersand script
--   CLASSIFY Employee ISA Person   translates to Isa (C "Person") (C "Employee")
--   CLASSIFY Workingstudent IS Employee/\Student   translates to IsE orig (C "Workingstudent") [C "Employee",C "Student"]
data AClassify = 
             Isa { genpos :: Origin
                 , genspc :: A_Concept      -- ^ specific concept
                 , gengen :: A_Concept      -- ^ generic concept
                 }
           | IsE { genpos :: Origin
                 , genspc :: A_Concept      -- ^ specific concept
                 , genrhs :: NE.NonEmpty A_Concept    -- ^ concepts of which the conjunction is equivalent to the specific concept
                 } deriving (Typeable)
instance Ord AClassify where
-- subjective choice: Isa > IsE
 compare a b = case (a,b) of
   (Isa{},Isa{}) -> compare (genspc a,gengen a) (genspc b,gengen b)
   (Isa{},IsE{}) -> GT  
   (IsE{},IsE{}) -> let fun = NE.nub . NE.sort . genrhs 
                    in compare (genspc a,fun a) (genspc b,fun b)
   (IsE{},Isa{}) -> LT 
instance Eq AClassify where
  a == b = compare a b == EQ
instance Traced AClassify where
  origin = genpos
instance Unique AClassify where
  showUnique a =
    case a of
      Isa{} -> showUnique (genspc a)<>" ISA "<>showUnique (gengen a)
      IsE{} -> showUnique (genspc a)<>" IS "<>T.intercalate " /\\ " (NE.toList . fmap showUnique $ genrhs a)
instance Show AClassify where
  -- This show is used in error messages. It should therefore not display the term's type
  show g =
    case g of
     Isa{} -> "CLASSIFY "<>show (genspc g)<>" ISA "<>show (gengen g)
     IsE{} -> "CLASSIFY "<>show (genspc g)<>" IS "<>L.intercalate " /\\ " (NE.toList . fmap show $ genrhs g)
instance Hashable AClassify where
    hashWithSalt s g = 
      s `hashWithSalt` genspc g
        `hashWithSalt` (case g of 
                         Isa{} -> [genspc g]
                         IsE{} -> NE.toList . NE.sort $ genrhs g 
                       )

data Interface = Ifc { ifcIsAPI ::    Bool          -- is this interface of type API?
                     , ifcname ::     Text        -- all roles for which an interface is available (empty means: available for all roles)
                     , ifcRoles ::    [Role]        -- all roles for which an interface is available (empty means: available for all roles)
                     , ifcObj ::      ObjectDef     -- NOTE: this top-level ObjectDef is contains the interface itself (ie. name and expression)
                     , ifcControls :: [Conjunct]    -- All conjuncts that must be evaluated after a transaction
                     , ifcPos ::      Origin        -- The position in the file (filename, line- and column number)
                     , ifcPrp ::      Text        -- The purpose of the interface
                     } deriving Show

instance Eq Interface where
  a == b = compare a b == EQ
instance Ord Interface where
  compare a b = compare (name a) (name b)
instance Named Interface where
  name = ifcname
instance Traced Interface where
  origin = ifcPos
instance Unique Interface where
  showUnique = name
-- Utility function for looking up interface refs
getInterfaceByName :: [Interface] -> Text -> Interface
getInterfaceByName interfaces' nm = case [ ifc | ifc <- interfaces', name ifc == nm ] of
                                []    -> fatal $ "getInterface by name: no interfaces named "<>tshow nm
                                [ifc] -> ifc
                                _     -> fatal $ "getInterface by name: multiple interfaces named "<>tshow nm


class Object a where
 concept ::   a -> A_Concept      -- the type of the object
 fields ::    a -> [ObjectDef]    -- the objects defined within the object
 contextOf :: a -> Expression     -- the context expression

instance Object ObjectDef where
 concept obj = target (objExpression obj)
 fields  obj = case objmsub obj of
                 Nothing       -> []
                 Just InterfaceRef{} -> []
                 Just b@Box{}    -> map objE . filter isObjExp $ siObjs b
    where
      isObjExp :: BoxItem -> Bool
      isObjExp BxExpr{} = True
      isObjExp BxTxt{}  = False
 contextOf   = objExpression

data BoxItem = 
        BxExpr {objE :: ObjectDef}
      | BxTxt {objT :: BoxTxt}
      deriving (Eq, Show)
instance Unique BoxItem where
  showUnique = tshow
instance Traced BoxItem where
  origin o 
    = case o of
        BxExpr{} -> origin . objE $ o
        BxTxt{} -> origin . objT $ o
data BoxTxt =
    BoxTxt { objnm  :: Text         -- ^ view name of the object definition. The label has no meaning in the Compliant Service Layer, but is used in the generated user interface if it is not an empty string.
           , objpos :: Origin
           , objtxt :: Text
           } deriving (Show)
instance Ord BoxTxt where
 compare a b = case compare (name a,objtxt a) (name b,objtxt b) of
     EQ -> fromMaybe (fatal . T.intercalate "\n" $
                        ["BoxTxt should have a non-fuzzy Origin."
                        , tshow (origin a)
                        , tshow (origin b)
                        ])
                     (maybeOrdering (origin a) (origin b))
     x -> x  
instance Eq BoxTxt where
 a == b = compare a b == EQ
data ObjectDef = ObjectDef
           { objnm    :: !Text
           -- ^ view name of the object definition. The label has no meaning in the Compliant Service Layer, but is used in the generated user interface if it is not an empty string.
           , objpos   :: !Origin
           -- ^ position of this definition in the text of the Ampersand source file (filename, line number and column number)
           , objExpression :: !Expression
           -- ^ this expression describes the instances of this object, related to their context.
           , objcrud  :: !Cruds
           -- ^ CRUD as defined by the user 
           , objmView :: Maybe ViewUsage
           -- ^ The view that should be used for this object
           , objmsub  :: Maybe SubInterface
           -- ^ the fields, which are object definitions themselves.
           } deriving (Show) -- just for debugging (zie ook instance Show BoxItem)
instance Named ObjectDef where
  name   = objnm
instance Traced ObjectDef where
  origin = objpos
instance Unique ObjectDef where
  showUnique = tshow
instance Ord ObjectDef where
 compare a b = case compare (name a) (name b) of
     EQ -> fromMaybe (fatal . T.intercalate "\n" $
                        ["ObjectDef should have a non-fuzzy Origin."
                        , tshow (origin a)
                        , tshow (origin b)
                        ])
                     (maybeOrdering (origin a) (origin b))
     x -> x
instance Eq ObjectDef where
  a == b = compare a b == EQ
instance Named BoxTxt where
  name   = objnm
instance Traced BoxTxt where
  origin = objpos
data Cruds = Cruds { crudOrig :: Origin
                   , crudC :: Bool
                   , crudR :: Bool
                   , crudU :: Bool
                   , crudD :: Bool
                   } deriving (Show)
data SubInterface = Box { pos       :: !Origin
                        , siConcept :: !A_Concept
                        , siHeader  :: !HTMLTemplateUsage
                        , siObjs    :: [BoxItem] 
                        }
                  | InterfaceRef 
                        { pos      :: !Origin
                        , siIsLink :: !Bool
                        , siIfcId  :: !Text  --id of the interface that is referenced to
                        } deriving (Show)



-- | Explanation is the intended constructor. It explains the purpose of the object it references.
--   The enrichment process of the parser must map the names (from PPurpose) to the actual objects
data Purpose  = Expl { explPos :: Origin     -- ^ The position in the Ampersand script of this purpose definition
                     , explObj :: ExplObj    -- ^ The object that is explained.
                     , explMarkup :: Markup   -- ^ This field contains the text of the explanation including language and markup info.
                     , explUserdefd :: Bool       -- ^ Is this purpose defined in the script?
                     , explRefIds :: [Text]     -- ^ The references of the explaination
                     } deriving (Show, Typeable)
--instance Eq Purpose where
--  a == b = compare a b == EQ

instance Ord Purpose where
 compare a b = case compare (explObj a) (explObj b) of
     EQ -> fromMaybe (fatal . T.intercalate "\n" $
                        ["Purpose should have a non-fuzzy Origin."
                        , tshow (origin a)
                        , tshow (origin b)
                        ])
                     (maybeOrdering (origin a) (origin b))
     x -> x  
instance Eq Purpose where
  a == b = compare a b == EQ
instance Unique Purpose where
  showUnique p = uniqueShowWithType (explMarkup p)
              <> tshow (typeOf x) <>"_" <> tshow x
    where x = origin p
instance Traced Purpose where
  origin = explPos

data Population -- The user defined populations
  = ARelPopu { popdcl :: Relation
             , popps ::  AAtomPairs     -- The user-defined pairs that populate the relation
             , popsrc :: A_Concept -- potentially more specific types than the type of Relation
             , poptgt :: A_Concept
             }
  | ACptPopu { popcpt :: A_Concept
             , popas ::  [AAtomValue]  -- The user-defined atoms that populate the concept
             } deriving (Eq,Ord)

instance Unique Population where
  showUnique pop@ARelPopu{} = (uniqueShowWithType.popdcl) pop <> (showUnique.popps) pop
  showUnique pop@ACptPopu{} = (uniqueShowWithType.popcpt) pop <> (showUnique.popas) pop

type AAtomPairs = Set.Set AAtomPair
data AAtomPair
  = APair { apLeft  :: AAtomValue
          , apRight :: AAtomValue
          } deriving(Eq,Ord)
mkAtomPair :: AAtomValue -> AAtomValue -> AAtomPair
mkAtomPair = APair

instance Unique AAtomPair where
  showUnique apair = "("<>(showUnique.apLeft) apair <>","<> (showUnique.apRight) apair<>")"

type AAtomValues = Set.Set AAtomValue
data AAtomValue
  = AAVString  { aavhash :: Int
               , aavtyp :: TType
               , aavtxt :: Text
               }
  | AAVInteger { aavtyp :: TType
               , aavint :: Integer
               }
  | AAVFloat   { aavtyp :: TType
               , aavflt :: Double
               }
  | AAVBoolean { aavtyp :: TType
               , aavbool :: Bool
               }
  | AAVDate { aavtyp :: TType
            , aadateDay ::  Day
            }
  | AAVDateTime { aavtyp :: TType
                , aadatetime ::  UTCTime
                }
  | AtomValueOfONE deriving (Eq,Ord, Show)

instance Unique AAtomValue where   -- FIXME:  this in incorrect! (AAtomValue should probably not be in Unique at all. We need to look into where this is used for.)
  showUnique pop@AAVString{}   = (tshow.aavhash) pop
  showUnique pop@AAVInteger{}  = (tshow.aavint) pop
  showUnique pop@AAVFloat{}    = (tshow.aavflt) pop
  showUnique pop@AAVBoolean{}  = (tshow.aavbool) pop
  showUnique pop@AAVDate{}     = (tshow.aadateDay) pop
  showUnique pop@AAVDateTime{} = (tshow.aadatetime) pop
  showUnique AtomValueOfONE    = "ONE"

showValSQL :: AAtomValue -> Text
showValSQL val =
  case val of
   AAVString{}  -> singleQuote . f . aavtxt $ val
     where 
       f :: Text -> Text
       f txt = case T.uncons txt of
         Nothing -> mempty
         Just (h,tl)
          | h `elem` ['\'','\\'] 
                      -> T.cons h (T.cons h (f tl))
          | otherwise -> T.cons h (f tl)
   AAVInteger{} -> tshow (aavint val)
   AAVBoolean{} -> tshow (aavbool val)
   AAVDate{}    -> singleQuote . T.pack $ showGregorian (aadateDay val)
   AAVDateTime {} -> singleQuote . T.pack $ formatTime defaultTimeLocale "%F %T" (aadatetime val) --NOTE: MySQL 5.5 does not comply to ISO standard. This format is MySQL specific
     --formatTime SL.defaultTimeLocale "%FT%T%QZ" (aadatetime val)
   AAVFloat{}   -> tshow (aavflt val)
   AtomValueOfONE{} -> "1"
singleQuote :: Text -> Text
singleQuote str = "'"<>str<>"'"

showValADL :: AAtomValue -> Text
showValADL val =
  case val of
   AAVString{}  -> aavtxt val
   AAVInteger{} -> tshow (aavint val)
   AAVBoolean{} -> tshow (aavbool val)
   AAVDate{}    -> T.pack $ showGregorian (aadateDay val)
   AAVDateTime {} -> T.pack $ formatTime defaultTimeLocale "%FT%T%QZ" (aadatetime val)
   AAVFloat{}   -> tshow (aavflt val)
   AtomValueOfONE{} -> "1"

data ExplObj = ExplConcept A_Concept
             | ExplRelation Relation
             | ExplRule Text
             | ExplIdentityDef Text
             | ExplViewDef Text
             | ExplPattern Text
             | ExplInterface Text
             | ExplContext Text
          deriving (Show ,Eq, Typeable, Ord)
instance Unique ExplObj where
  showUnique e = "Explanation of "<>
    case e of
     (ExplConcept cpt)   -> uniqueShowWithType cpt
     (ExplRelation rel)  -> uniqueShowWithType rel
     (ExplRule s)        -> "a Rule named "<>s
     (ExplIdentityDef s) -> "an Ident named "<>s
     (ExplViewDef s)     -> "a View named "<>s
     (ExplPattern s)     -> "a Pattern named "<>s
     (ExplInterface s)   -> "an Interface named "<>s
     (ExplContext s)     -> "a Context named "<>s

data Expression
      = EEqu (Expression,Expression)   -- ^ equivalence             =
      | EInc (Expression,Expression)   -- ^ inclusion               |-
      | EIsc (Expression,Expression)   -- ^ intersection            /\
      | EUni (Expression,Expression)   -- ^ union                   \/
      | EDif (Expression,Expression)   -- ^ difference              -
      | ELrs (Expression,Expression)   -- ^ left residual           /
      | ERrs (Expression,Expression)   -- ^ right residual          \
      | EDia (Expression,Expression)   -- ^ diamond                 <>
      | ECps (Expression,Expression)   -- ^ composition             ;
      | ERad (Expression,Expression)   -- ^ relative addition       !
      | EPrd (Expression,Expression)   -- ^ cartesian product       *
      | EKl0 Expression                -- ^ Rfx.Trn closure         *  (Kleene star)
      | EKl1 Expression                -- ^ Transitive closure      +  (Kleene plus)
      | EFlp Expression                -- ^ conversion (flip, wok)  ~
      | ECpl Expression                -- ^ Complement
      | EBrk Expression                -- ^ bracketed expression ( ... )
      | EDcD Relation                  -- ^ simple relation
      | EDcI A_Concept                 -- ^ Identity relation
      | EEps A_Concept Signature       -- ^ Epsilon relation (introduced by the system to ensure we compare concepts by equality only.
      | EDcV Signature                 -- ^ Cartesian product relation
      | EMp1 PAtomValue A_Concept      -- ^ constant PAtomValue, because when building the Expression, the TType of the concept isn't known yet.
      deriving (Eq, Ord, Show, Typeable, Generic, Data)
instance Hashable Expression where
   hashWithSalt s expr =
     s `hashWithSalt`
       case expr of
        EEqu (a,b) -> ( 0::Int) `hashWithSalt` a `hashWithSalt` b
        EInc (a,b) -> ( 1::Int) `hashWithSalt` a `hashWithSalt` b
        EIsc (a,b) -> ( 2::Int) `hashWithSalt` a `hashWithSalt` b
        EUni (a,b) -> ( 3::Int) `hashWithSalt` a `hashWithSalt` b
        EDif (a,b) -> ( 4::Int) `hashWithSalt` a `hashWithSalt` b
        ELrs (a,b) -> ( 5::Int) `hashWithSalt` a `hashWithSalt` b
        ERrs (a,b) -> ( 6::Int) `hashWithSalt` a `hashWithSalt` b
        EDia (a,b) -> ( 7::Int) `hashWithSalt` a `hashWithSalt` b
        ECps (a,b) -> ( 8::Int) `hashWithSalt` a `hashWithSalt` b
        ERad (a,b) -> ( 9::Int) `hashWithSalt` a `hashWithSalt` b
        EPrd (a,b) -> (10::Int) `hashWithSalt` a `hashWithSalt` b
        EKl0 e     -> (11::Int) `hashWithSalt` e
        EKl1 e     -> (12::Int) `hashWithSalt` e
        EFlp e     -> (13::Int) `hashWithSalt` e
        ECpl e     -> (14::Int) `hashWithSalt` e
        EBrk e     -> (15::Int) `hashWithSalt` e
        EDcD d     -> (16::Int) `hashWithSalt` d
        EDcI c     -> (17::Int) `hashWithSalt` c
        EEps c sgn -> (18::Int) `hashWithSalt` c `hashWithSalt` sgn
        EDcV sgn   -> (19::Int) `hashWithSalt` sgn
        EMp1 val c -> (21::Int) `hashWithSalt` show val `hashWithSalt` c

instance Unique Expression where
  showUnique = tshow -- showA is not good enough: epsilons are disguised, so there can be several different
                    -- expressions with the same showA. 

instance Unique (PairView Expression) where
  showUnique = tshow
instance Unique (PairViewSegment Expression) where
  showUnique = tshow


(.==.), (.|-.), (./\.), (.\/.), (.-.), (./.), (.\.), (.<>.), (.:.), (.!.), (.*.) :: Expression -> Expression -> Expression
infixl 1 .==.   -- equivalence
infixl 1 .|-.   -- inclusion
infixl 2 ./\.   -- intersection
infixl 2 .\/.   -- union
infixl 4 .-.    -- difference
infixl 6 ./.    -- left residual
infixl 6 .\.    -- right residual
infixl 6 .<>.   -- diamond
infixl 8 .:.    -- composition    -- .;. was unavailable, because Haskell's scanner does not recognize it as an operator.
infixl 8 .!.    -- relative addition
infixl 8 .*.    -- cartesian product

-- SJ 20130118: The fatals are superfluous, but only if the type checker works correctly. For that reason, they are not being removed. Not even for performance reasons.
l .==. r = if source l/=source r ||  target l/=target r then fatal ("Cannot equate (with operator \"==\") expression l of type "<>tshow (sign l)<>"\n   "<>tshow l<>"\n   with expression r of type "<>tshow (sign r)<>"\n   "<>tshow r<>".") else
           EEqu (l,r)
l .|-. r = if source l/=source r ||  target l/=target r then fatal ("Cannot include (with operator \"|-\") expression l of type "<>tshow (sign l)<>"\n   "<>tshow l<>"\n   with expression r of type "<>tshow (sign r)<>"\n   "<>tshow r<>".") else
           EInc (l,r)
l ./\. r = if source l/=source r ||  target l/=target r then fatal ("Cannot intersect (with operator \"/\\\") expression l of type "<>tshow (sign l)<>"\n   "<>tshow l<>"\n   with expression r of type "<>tshow (sign r)<>"\n   "<>tshow r<>".") else
           EIsc (l,r)
l .\/. r = if source l/=source r ||  target l/=target r then fatal ("Cannot unite (with operator \"\\/\") expression l of type "<>tshow (sign l)<>"\n   "<>tshow l<>"\n   with expression r of type "<>tshow (sign r)<>"\n   "<>tshow r<>".") else
           EUni (l,r)
l .-. r  = if source l/=source r ||  target l/=target r then fatal ("Cannot subtract (with operator \"-\") expression l of type "<>tshow (sign l)<>"\n   "<>tshow l<>"\n   with expression r of type "<>tshow (sign r)<>"\n   "<>tshow r<>".") else
           EDif (l,r)
l ./. r  = if target l/=target r then fatal ("Cannot residuate (with operator \"/\") expression l of type "<>tshow (sign l)<>"\n   "<>tshow l<>"\n   with expression r of type "<>tshow (sign r)<>"\n   "<>tshow r<>".") else
           ELrs (l,r)
l .\. r  = if source l/=source r then fatal ("Cannot residuate (with operator \"\\\") expression l of type "<>tshow (sign l)<>"\n   "<>tshow l<>"\n   with expression r of type "<>tshow (sign r)<>"\n   "<>tshow r<>".") else
           ERrs (l,r)
l .<>. r = if source r/=target l then fatal ("Cannot use diamond operator \"<>\") expression l of type "<>tshow (sign l)<>"\n   "<>tshow l<>"\n   with expression r of type "<>tshow (sign r)<>"\n   "<>tshow r<>".") else
           EDia (l,r)
l .:. r  = if source r/=target l then fatal ("Cannot compose (with operator \";\") expression l of type "<>tshow (sign l)<>"\n   "<>tshow l<>"\n   with expression r of type "<>tshow (sign r)<>"\n   "<>tshow r<>".") else
           ECps (l,r)
l .!. r  = if source r/=target l then fatal ("Cannot add (with operator \"!\") expression l of type "<>tshow (sign l)<>"\n   "<>tshow l<>"\n   with expression r of type "<>tshow (sign r)<>"\n   "<>tshow r<>".") else
           ERad (l,r)
l .*. r  = -- SJC: always fits! No fatal here..
           EPrd (l,r)
{- For the operators /, \, ;, ! and * we must not check whether the intermediate types exist.
   Suppose the user says GEN Student ISA Person and GEN Employee ISA Person, then Student `join` Employee has a name (i.e. Person), but Student `meet` Employee
   does not. In that case, -(r!s) (with target r=Student and source s=Employee) is defined, but -r;-s is not.
   So in order to let -(r!s) be equal to -r;-s we must not check for the existence of these types, for the Rotterdam paper already shows that this is fine.
-}

instance Flippable Expression where
  flp expr = case expr of
               EEqu (l,r) -> EEqu (flp l, flp r)
               EInc (l,r) -> EInc (flp l, flp r)
               EIsc (l,r) -> EIsc (flp l, flp r)
               EUni (l,r) -> EUni (flp l, flp r)
               EDif (l,r) -> EDif (flp l, flp r)
               ELrs (l,r) -> ERrs (flp r, flp l)
               ERrs (l,r) -> ELrs (flp r, flp l)
               EDia (l,r) -> EDia (flp r, flp l)
               ECps (l,r) -> ECps (flp r, flp l)
               ERad (l,r) -> ERad (flp r, flp l)
               EPrd (l,r) -> EPrd (flp r, flp l)
               EFlp e     -> e
               ECpl e     -> ECpl (flp e)
               EKl0 e     -> EKl0 (flp e)
               EKl1 e     -> EKl1 (flp e)
               EBrk f     -> EBrk (flp f)
               EDcD{}     -> EFlp expr
               EDcI{}     -> expr
               EEps i sgn -> EEps i (flp sgn)
               EDcV sgn   -> EDcV (flp sgn)
               EMp1{}     -> expr

instance HasSignature Expression where
 sign (EEqu (l,r)) = Sign (source l) (target r)
 sign (EInc (l,r)) = Sign (source l) (target r)
 sign (EIsc (l,r)) = Sign (source l) (target r)
 sign (EUni (l,r)) = Sign (source l) (target r)
 sign (EDif (l,r)) = Sign (source l) (target r)
 sign (ELrs (l,r)) = Sign (source l) (source r)
 sign (ERrs (l,r)) = Sign (target l) (target r)
 sign (EDia (l,r)) = Sign (source l) (target r)
 sign (ECps (l,r)) = Sign (source l) (target r)
 sign (ERad (l,r)) = Sign (source l) (target r)
 sign (EPrd (l,r)) = Sign (source l) (target r)
 sign (EKl0 e)     = sign e
 sign (EKl1 e)     = sign e
 sign (EFlp e)     = flp (sign e)
 sign (ECpl e)     = sign e
 sign (EBrk e)     = sign e
 sign (EDcD d)     = sign d
 sign (EDcI c)     = Sign c c
 sign (EEps _ sgn) = sgn
 sign (EDcV sgn)   = sgn
 sign (EMp1 _ c)   = Sign c c

showSign :: HasSignature a => a -> Text
showSign x = let Sign s t = sign x in "["<>name s<>"*"<>name t<>"]"

-- We allow editing on basic relations (Relations) that may have been flipped, or narrowed/widened by composing with I.
-- Basically, we have a relation that may have several epsilons to its left and its right, and the source/target concepts
-- we use are the concepts in the innermost epsilon, or the source/target concept of the relation, in absence of epsilons.
-- This is used to determine the type of the atoms provided by the outside world through interfaces.
getExpressionRelation :: Expression -> Maybe (A_Concept, Relation, A_Concept, Bool)
getExpressionRelation expr = case getRelation expr of
   Just (s,Just d,t,isFlipped)  -> Just (s,d,t,isFlipped)
   _                            -> Nothing
 where
    -- If the expression represents an editable relation, the relation is returned together with the narrowest possible source and target
    -- concepts, as well as a boolean that states whether the relation is flipped.
    getRelation :: Expression -> Maybe (A_Concept, Maybe Relation, A_Concept, Bool)
    getRelation (ECps (e, EDcI{})) = getRelation e
    getRelation (ECps (EDcI{}, e)) = getRelation e
    getRelation (ECps (e1, e2))
      = case (getRelation e1, getRelation e2) of --note: target e1==source e2
         (Just (_,Nothing,i1,_), Just (i2,Nothing,_,_)) 
             | i1==target e1 && i2==source e2 -> Just (i1, Nothing, i2, False)
             | i1==target e1 && i2/=source e2 -> Just (i2, Nothing, i2, False)
             | i1/=target e1 && i2==source e2 -> Just (i1, Nothing, i1, False)
             | otherwise                      -> Nothing
         (Just (_,Nothing,i,_), Just (s,d,t,isFlipped)) 
             | i==target e1                   -> Just (s,d,t,isFlipped)
             | i/=target e1 && s==target e1   -> Just (i,d,t,isFlipped)
             | otherwise                      -> Nothing
         (Just (s,d,t,isFlipped), Just (i,Nothing,_,_))
             | i==source e2                   -> Just (s,d,t,isFlipped)
             | i/=source e2 && t==source e2   -> Just (s,d,i,isFlipped)
             | otherwise                      -> Nothing
         _                                    -> Nothing
    getRelation (EFlp e)
     = case getRelation e of
         Just (s,d,t,isFlipped) -> Just (t,d,s,not isFlipped)
         Nothing                -> Nothing
    getRelation (EDcD d)   = Just (source d, Just d, target d, False)
    getRelation (EEps i _) = Just (i, Nothing, i, False)
    getRelation _ = Nothing


-- The following definition of concept is used in the type checker only.
-- It is called Concept, meaning "type checking concept"

data A_Concept
   = PlainConcept { aliases :: NE.NonEmpty Text 
                    -- ^ List of names that the concept is refered to, in random order
                  }
   | ONE -- ^ The universal Singleton: 'I'['Anything'] = 'V'['Anything'*'Anything']
    deriving (Typeable,Data,Ord,Eq)
type A_Concepts = Set.Set A_Concept
{- -- this is faster, so if you think Eq on concepts is taking a long time, try this..
instance Ord A_Concept where
  compare (PlainConcept{cpthash=v1}) (PlainConcept{cpthash=v2}) = compare v1 v2
  compare ONE ONE = EQ
  compare ONE (PlainConcept{}) = LT
  compare (PlainConcept{}) ONE = GT

instance Eq A_Concept where
  a == b = compare a b == EQ

-}
  
instance Unique A_Concept where
  showUnique = tshow
instance Hashable A_Concept where
  hashWithSalt s cpt =
     s `hashWithSalt` (case cpt of
                        PlainConcept{} -> (0::Int) `hashWithSalt` NE.sort (aliases cpt)
                        ONE          -> 1::Int
                      )
instance Named A_Concept where
  name PlainConcept{aliases = names} = NE.head names
  name ONE = "ONE"

instance Show A_Concept where
  show = T.unpack . name
-- | special type of Show, for types that can have aliases. Its purpose is
--   to use when giving feedback to the ampersand modeler, in cases aliases 
--   are used. 
class Show a => ShowWithAliases a where
  showWithAliases :: a -> Text
  -- Default is to just use show. This makes it easier to use showAliases 
  -- at more places, even if there is not a specific implementation 
  -- for it
  showWithAliases = tshow
instance ShowWithAliases A_Concept where
  showWithAliases ONE = name ONE
  showWithAliases cpt@PlainConcept{aliases = names} =
     case NE.tail names of
       [] ->  name cpt
       xs -> name cpt <> "("<>T.intercalate ", " xs<>")"

instance Unique (A_Concept, PAtomValue) where
  showUnique (c,val) = tshow val<>"["<>showUnique c<>"]"

data Signature = Sign A_Concept A_Concept deriving (Eq, Ord, Typeable, Generic, Data)
instance Hashable Signature
instance Show Signature where
  show (Sign s t) =
     "[" <> show s <> "*" <> show t <> "]"
instance ShowWithAliases Signature where
  showWithAliases (Sign s t) =
     "[" <> showWithAliases s <> "*" <> showWithAliases t <> "]"
instance Unique Signature where
  showUnique (Sign s t) = "[" <> showUnique s <> "*" <> showUnique t <> "]"
instance HasSignature Signature where
  source (Sign s _) = s
  target (Sign _ t) = t
  sign sgn = sgn

instance Flippable Signature where
 flp (Sign s t) = Sign t s

class HasSignature rel where
  source, target :: rel -> A_Concept      -- e.g. Relation -> Concept
  source x        = source (sign x)
  target x        = target (sign x)
  sign :: rel -> Signature
  isEndo :: rel  -> Bool
  isEndo s        = source s == target s


-- Convenient data structure to hold information about concepts and their representations
--  in a context.
data ContextInfo =
  CI { ctxiGens         :: [AClassify]      -- The generalisation relations in the context
     , representationOf :: A_Concept -> TType -- a list containing all user defined Representations in the context
     , multiKernels     :: [Typology] -- a list of typologies, based only on the CLASSIFY statements. Single-concept typologies are not included
     , reprList         :: [Representation] -- a list of all Representations
     , declDisambMap    :: Map.Map Text (Map.Map SignOrd Expression) -- a map of declarations and the corresponding types
     , soloConcs        :: Set.Set Type -- types not used in any declaration
     , gens_efficient   :: Op1EqualitySystem Type -- generalisation relations again, as a type system (including phantom types)
     , conceptMap       :: ConceptMap -- a map that must be used to convert P_Concept to A_Concept
     } 
                       
instance Named Type where
  name v = case typeOrConcept dummy v of
                Right (Just x) -> "Built-in type "<>tshow x
                Right Nothing  -> "The Generic Built-in type"
                Left  x -> "Concept: "<>name x
    where dummy = makeConceptMap []
typeOrConcept :: ConceptMap -> Type -> Either A_Concept (Maybe TType)
typeOrConcept fun (BuiltIn TypeOfOne)  = Left . fun . mkPConcept $ "ONE"
typeOrConcept fun (UserConcept s)      = Left . fun . mkPConcept $ s
typeOrConcept _   (BuiltIn x)          = Right (Just x)
typeOrConcept _   RepresentSeparator   = Right Nothing

data Type = UserConcept Text
          | BuiltIn TType
          | RepresentSeparator
          deriving (Eq,Ord,Show)

-- for faster comparison
newtype SignOrd = SignOrd Signature
instance Ord SignOrd where
  compare (SignOrd (Sign a b)) (SignOrd (Sign c d)) = compare (a, b) (c, d)
instance Eq SignOrd where
  a == b = compare a b == EQ
   

-- | This function is meant to convert the PSingleton inside EMp1 to an AAtomValue,
--   after the expression has been built inside an A_Context. Only at that time
--   the TType is known, enabling the correct transformation.
--   To ensure that this function is not used too early, ContextInfo is required,
--   which only exsists after disambiguation.
safePSingleton2AAtomVal :: ContextInfo -> A_Concept -> PAtomValue -> AAtomValue
safePSingleton2AAtomVal ci c val =
   case unsafePAtomVal2AtomValue typ (Just c) val of
     Left _ -> fatal . T.intercalate "\n  " $
                  [ "This should be impossible: after checking everything an unhandled singleton value found!"
                  , "Concept: "<>tshow c
                  , "TType: "<>tshow typ
                  , "Origin: "<>tshow (origin val)
                  , "PAtomValue: "<>case val of
                                      (PSingleton _ _ v) -> "PSingleton ("<>tshow v<>")"
                                      (ScriptString _ v) -> "ScriptString ("<>tshow v<>")"
                                      (XlsxString _ v)   -> "XlsxString ("<>tshow v<>")"
                                      (ScriptInt _ v)    -> "ScriptInt ("<>tshow v<>")"
                                      (ScriptFloat _ v)  -> "ScriptFloat ("<>tshow v<>")"
                                      (XlsxDouble _ v)   -> "XlsxDouble ("<>tshow v<>")"
                                      (ComnBool _ v)     -> "ComnBool ("<>tshow v<>")"
                                      (ScriptDate _ v)   -> "ScriptDate ("<>tshow v<>")"
                                      (ScriptDateTime _ v) -> "ScriptDateTime ("<>tshow v<>")"
                  ]
     Right x -> x
  where typ = representationOf ci c

-- SJC: Note about this code:
-- error messages are written here, and later turned into error messages via mkIncompatibleAtomValueError
-- Ideally, this module would import Ampersand.Input.ADL1.CtxError
-- that way, unsafePAtomVal2AtomValue could create a 'Origin -> Guarded AAtomValue' instead.
unsafePAtomVal2AtomValue :: TType -> Maybe A_Concept -> PAtomValue -> Either Text AAtomValue
unsafePAtomVal2AtomValue typ mCpt pav =
  case unsafePAtomVal2AtomValue' of
    Left err -> Left err
    Right rawVal -> Right roundedVal
      where roundedVal =
             case rawVal of
              AAVDateTime t x -> -- Rounding is needed, to maximize the number of databases
                                 -- on wich this runs. (MySQL 5.5 only knows seconds)
                                 AAVDateTime t (truncateByFormat x)
                                  where
                                    truncateByFormat :: UTCTime  -> UTCTime
                                    truncateByFormat = f (parseTimeOrError True) . f formatTime
                                      where
                                        format = iso8601DateFormat (Just "%H:%M:%S")
                                    --    f:: TimeLocale -> Text -> typ
                                        f fun = fun defaultTimeLocale format
              _          -> rawVal
  where
    unsafePAtomVal2AtomValue' :: Either Text AAtomValue
    unsafePAtomVal2AtomValue'
      = case pav of
          PSingleton o str mval
             -> case typ of
                 Alphanumeric     -> Right (AAVString (hash str) typ str)
                 BigAlphanumeric  -> Right (AAVString (hash str) typ str)
                 HugeAlphanumeric -> Right (AAVString (hash str) typ str)
                 Password         -> Right (AAVString (hash str) typ str)
                 Object           -> Right (AAVString (hash str) typ str)
                 _                -> case mval of
                                       Nothing -> Left (message o str)
                                       Just x -> unsafePAtomVal2AtomValue typ mCpt x
          ScriptString o str
             -> case typ of
                 Alphanumeric     -> Right (AAVString (hash str) typ str)
                 BigAlphanumeric  -> Right (AAVString (hash str) typ str)
                 HugeAlphanumeric -> Right (AAVString (hash str) typ str)
                 Password         -> Right (AAVString (hash str) typ str)
                 Binary           -> Left "Binary cannot be populated in an ADL script"
                 BigBinary        -> Left "Binary cannot be populated in an ADL script"
                 HugeBinary       -> Left "Binary cannot be populated in an ADL script"
                 Date             -> Left (message o str)
                 DateTime         -> Left (message o str)
                 Boolean          -> Left (message o str)
                 Integer          -> Left (message o str)
                 Float            -> Left (message o str)
                 TypeOfOne        -> Left "ONE has a population of it's own, that cannot be modified"
                 Object           -> Right (AAVString (hash str) typ str)
          XlsxString o str
             -> case typ of
                 Alphanumeric     -> Right (AAVString (hash str) typ str)
                 BigAlphanumeric  -> Right (AAVString (hash str) typ str)
                 HugeAlphanumeric -> Right (AAVString (hash str) typ str)
                 Password         -> Right (AAVString (hash str) typ str)
                 Binary           -> Left "Binary cannot be populated in an ADL script"
                 BigBinary        -> Left "Binary cannot be populated in an ADL script"
                 HugeBinary       -> Left "Binary cannot be populated in an ADL script"
                 Date             -> Left (message o str)
                 DateTime         -> Left (message o str)
                 Boolean          -> let table =
                                            [("TRUE", True), ("FALSE" , False)
                                            ,("YES" , True), ("NO"    , False)
                                            ,("WAAR", True), ("ONWAAR", False)
                                            ,("JA"  , True), ("NEE"   , False)
                                            ,("WEL" , True), ("NIET"  , False)
                                            ]
                                     in case lookup (T.toUpper str) table of
                                        Just b -> Right (AAVBoolean typ b)
                                        Nothing -> Left $ "permitted Booleans: "<>(tshow . fmap (camelCase . fst) $ table)
                                       where camelCase :: Text -> Text
                                             camelCase txt = case T.uncons txt of
                                               Nothing -> mempty
                                               Just(h,tl) -> T.cons (toUpper h) (T.toLower tl)
                 Integer          -> case readMaybe . T.unpack $ str  of
                                           Just i  -> Right (AAVInteger typ i)
                                           Nothing -> Left (message o str)
                 Float            -> case readMaybe . T.unpack $ str of
                                           Just r  -> Right (AAVFloat typ r)
                                           Nothing -> Left (message o str)
                 TypeOfOne        -> Left "ONE has a population of it's own, that cannot be modified"
                 Object           -> Right (AAVString (hash str) typ str)
          ScriptInt o i
             -> case typ of
                 Alphanumeric     -> Left (message o i)
                 BigAlphanumeric  -> Left (message o i)
                 HugeAlphanumeric -> Left (message o i)
                 Password         -> Left (message o i)
                 Binary           -> Left "Binary ca)not be populated in an ADL script"
                 BigBinary        -> Left "Binary cannot be populated in an ADL script"
                 HugeBinary       -> Left "Binary cannot be populated in an ADL script"
                 Date             -> Left (message o i)
                 DateTime         -> Left (message o i)
                 Boolean          -> Left (message o i)
                 Integer          -> Right (AAVInteger typ i)
                 Float            -> Right (AAVFloat typ (fromInteger i)) -- must convert, because `34.000` is lexed as Integer
                 TypeOfOne        -> Left "ONE has a population of it's own, that cannot be modified"
                 Object           -> Left (message o i)
          ScriptFloat o x
             -> case typ of
                 Alphanumeric     -> Left (message o x)
                 BigAlphanumeric  -> Left (message o x)
                 HugeAlphanumeric -> Left (message o x)
                 Password         -> Left (message o x)
                 Binary           -> Left "Binary cannot be populated in an ADL script"
                 BigBinary        -> Left "Binary cannot be populated in an ADL script"
                 HugeBinary       -> Left "Binary cannot be populated in an ADL script"
                 Date             -> Left (message o x)
                 DateTime         -> Left (message o x)
                 Boolean          -> Left (message o x)
                 Integer          -> Left (message o x)
                 Float            -> Right (AAVFloat typ x)
                 TypeOfOne        -> Left "ONE has a population of it's own, that cannot be modified"
                 Object           -> Left (message o x)
          XlsxDouble o d
             -> case typ of
                 Alphanumeric     -> relaxXLSXInput d    
                 BigAlphanumeric  -> relaxXLSXInput d
                 HugeAlphanumeric -> relaxXLSXInput d
                 Password         -> relaxXLSXInput d
                 Binary           -> Left "Binary cannot be populated in an ADL script"
                 BigBinary        -> Left "Binary cannot be populated in an ADL script"
                 HugeBinary       -> Left "Binary cannot be populated in an ADL script"
                 Date             -> Right AAVDate {aavtyp = typ
                                                   ,aadateDay = addDays (floor d) dayZeroExcel
                                                   }
                 DateTime         -> Right AAVDateTime {aavtyp = typ
                                                       ,aadatetime = UTCTime (addDays daysSinceZero dayZeroExcel)
                                                                             (picosecondsToDiffTime.floor $ fractionOfDay*picosecondsPerDay)
                                                       }
                                         where picosecondsPerDay = 24*60*60*1000000000000
                                               (daysSinceZero, fractionOfDay) = properFraction d
                 Boolean          -> Left (message o d)
                 Integer          -> if frac == 0
                                     then Right (AAVInteger typ int)
                                     else Left (message o d)
                                      where
                                        (int,frac) = properFraction d
                 Float            -> Right (AAVFloat typ d)
                 TypeOfOne        -> Left "ONE has a population of it's own, that cannot be modified"
                 Object           -> relaxXLSXInput d
          ComnBool o b
             -> if typ == Boolean
                then Right (AAVBoolean typ b)
                else Left (message o b)
          ScriptDate o x
             -> if typ == Date
                then Right (AAVDate typ x)
                else Left (message o x)
          ScriptDateTime o x
             -> if typ == DateTime
                then Right (AAVDateTime typ x)
                else Left (message o x)
            
       where
         relaxXLSXInput :: Double -> Either Text AAtomValue
         relaxXLSXInput v = Right . AAVString (hash v) typ . neat . tshow $ v
           where neat :: Text -> Text
                 neat s 
                   | onlyZeroes dotAndAfter = beforeDot
                   | otherwise = s
                   where (beforeDot, dotAndAfter) = T.span (/= '.') s
                         onlyZeroes s' = case T.uncons s' of
                           Nothing -> True
                           Just ('.',afterDot) ->  T.all (== '0') afterDot
                           _ -> False
         message :: Show x => Origin -> x -> Text
         message orig x = T.intercalate "\n    " $
                          ["Representation mismatch"
                          , "Found: `"<>tshow x<>"` ("<>tshow orig<>"),"
                          , "as representation of an atom in concept `"<>name c<>"`."
                          , "However, the representation-type of that concept is "<>implicitly
                          , "defined as "<>tshow typ<>". The found value does not match that type."
                          ]<> example
            where
              c = fromMaybe (fatal "Representation mismatch without concept known should not happen.") mCpt
              implicitly = if typ == Object then "(implicitly) " else ""
              example :: [Text]
              example = case typ of
                  Alphanumeric     -> ["ALPHANUMERIC types are texts (max 255 chars) surrounded with double quotes (\"-characters)."]
                  BigAlphanumeric  -> ["BIGALPHANUMERIC types are texts (max 64k chars) surrounded with double quotes (\"-characters)."]
                  Boolean          -> ["BOOLEAN types can have the value TRUE or FALSE (without surrounding quotes)."]
                  Date             -> ["DATE types are defined by ISO8601, e.g. 2013-07-04 (without surrounding quotes)."]
                  DateTime         -> ["DATETIME types follow ISO 8601 format, e.g. 2013-07-04T11:11:11+00:00 or 2015-06-03T13:21:58Z (without surrounding quotes)."]
                  Float            -> ["FLOAT type are floating point numbers. There should be a dot character (.) in it."]
                  HugeAlphanumeric -> ["HUGEALPHANUMERIC types are texts (max 16M chars) surrounded with double quotes (\"-characters)."]
                  Integer          -> ["INTEGER types are decimal numbers (max 20 positions), e.g. 4711 or -4711 (without surrounding quotes)"]
                  Password         -> ["PASSWORD types are texts (max 255 chars) surrounded with double quotes (\"-characters)."]
                  Object           -> ["OBJECT types are non-scalar atoms represented by an identifier (max 255 chars) surrounded with double quotes (\"-characters)."]
                  _                -> fatal $ "There is no example denotational syntax for a value of type `"<>tshow typ<>"`." 
         dayZeroExcel = addDays (-2) (fromGregorian 1900 1 1) -- Excel documentation tells that counting starts a jan 1st, however, that isn't totally true.
     


-- | The typology of a context is the partioning of the concepts in that context into 
--   sets such that (isa\/isa~)*;typology |- typology
--   Note, that with isa we only refer to the relations defined by CLASSIFY statements, 
--   not named relations with the same properties ( {UNI,INJ,TOT} or {UNI,INJ,SUR} )
data Typology = Typology { tyroot :: A_Concept -- the most generic concept in the typology 
                         , tyCpts :: [A_Concept] -- all concepts, from generic to specific
                         } deriving Show

-- | Since we can have concepts with several aliasses, we need to have a 
--   way to resolve these aliasses. In the A-structure, we do not want to
--   bother: if `foo` is an alias of `bar`, there should only be one A_Concept
--   that represents both `foo` and `bar`. We should be able to use a map
--   whenever we need to know the A_Concept for a P_Concept.  
type ConceptMap = P_Concept -> A_Concept

makeConceptMap :: [PClassify] -> ConceptMap
makeConceptMap gs = mapFunction
   where
     mapFunction :: P_Concept -> A_Concept
     mapFunction pCpt = case L.nub . concat . filter inCycle $ getCycles edges of
                          xs -> mkConcept pCpt xs
       where
         inCycle xs = pCpt `elem` xs
     mkConcept :: P_Concept -> [P_Concept] -> A_Concept
     mkConcept pCpt aliasses = 
       case pCpt of
         P_ONE  -> ONE
         PCpt{} -> PlainConcept 
           { aliases = sorted
           }
        where sorted = NE.nub . NE.sort $ ( name pCpt NE.:| map name aliasses)    
     edges :: [(P_Concept, [P_Concept])]
     edges = L.nub . map mkEdge . eqCl specific $ gs
     mkEdge :: NonEmpty PClassify -> (P_Concept, [P_Concept])
     mkEdge x = ( from , to's)
       where from = specific . NE.head $ x
             to's = L.nub . concatMap (toList . generics) $ x

