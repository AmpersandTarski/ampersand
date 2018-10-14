{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE UndecidableInstances #-}
module Ampersand.Core.AbstractSyntaxTree (
   A_Context(..)
 , Typology(..)
 , Meta(..)
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
 , A_Gen(..)
 , Interface(..)
 , getInterfaceByName
 , SubInterface(..)
 , BoxItem(..),ObjectDef(..),BoxTxt(..),isObjExp
 , Object(..)
 , Cruds(..)
 , Default(..)
 , Purpose(..)
 , ExplObj(..)
 , Expression(..)
 , getExpressionRelation
 , A_Concept(..), A_Concepts
 , Meaning(..)
 , A_RoleRule(..)
 , A_RoleRelation(..)
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
 , makeConcept
 , aavstr
 ) where
import           Ampersand.Basics
import           Ampersand.Core.ParseTree 
    ( Meta(..)
    , Role(..)
    , ConceptDef
    , Origin(..)
    , Traced(..)
    , ViewHtmlTemplate(..)
    , PairView(..)
    , PairViewSegment(..)
    , Prop(..), Props
    , Representation(..), TType(..), PAtomValue(..), PSingleton
    )
import           Data.Char          (toUpper,toLower)
import           Data.Data          (Typeable,Data)
import           Data.Default       (Default(..))
import           Data.Function      (on)
import           Data.Hashable      (Hashable(..),hashWithSalt)
import           Data.List          (nub,intercalate,sort)
import           Data.Maybe         (fromMaybe,listToMaybe)
import qualified Data.Set as Set
import           Data.Text          (Text,unpack,pack)
import           Data.Time.Calendar (showGregorian,Day, fromGregorian, addDays)
import           Data.Time.Clock    (UTCTime(UTCTime),picosecondsToDiffTime)
import qualified Data.Time.Format as DTF 
                          (formatTime,parseTimeOrError,defaultTimeLocale,iso8601DateFormat)
import           GHC.Generics       (Generic)
import qualified Data.Map as Map
import           Ampersand.ADL1.Lattices (Op1EqualitySystem)

data A_Context
   = ACtx{ ctxnm :: String           -- ^ The name of this context
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
         , ctxRRels :: [A_RoleRelation] -- ^ The assignment of roles to Relations (which role mayEdit what relations)
         , ctxreprs :: A_Concept -> TType
         , ctxvs :: [ViewDef]        -- ^ The view definitions defined in this context, outside the scope of patterns
         , ctxgs :: [A_Gen]          -- ^ The specialization statements defined in this context, outside the scope of patterns
         , ctxgenconcs :: [[A_Concept]] -- ^ A partitioning of all concepts: the union of all these concepts contains all atoms, and the concept-lists are mutually distinct in terms of atoms in one of the mentioned concepts
         , ctxifcs :: [Interface]    -- ^ The interfaces defined in this context
         , ctxps :: [Purpose]        -- ^ The purposes of objects defined in this context, outside the scope of patterns and processes
         , ctxmetas :: [Meta]        -- ^ used for Pandoc authors (and possibly other things)
         , ctxInfo :: ContextInfo
         } deriving (Typeable)              --deriving (Show) -- voor debugging
instance Show A_Context where
  showsPrec _ c = showString (ctxnm c)
instance Eq A_Context where
  c1 == c2  =  name c1 == name c2
instance Unique A_Context where
  showUnique = optionalQuote . name
instance Named A_Context where
  name  = ctxnm

data A_RoleRelation
   = RR { rrRoles :: [Role]     -- ^ name of a role
        , rrRels :: [Relation]   -- ^ name of a Relation
        , rrPos :: Origin       -- ^ position in the Ampersand script
        } deriving Show
instance Traced A_RoleRelation where
   origin = rrPos

data Pattern
   = A_Pat { ptnm :: String         -- ^ Name of this pattern
           , ptpos :: Origin        -- ^ the position in the file in which this pattern was declared.
           , ptend :: Origin        -- ^ the end position in the file, elements with a position between pos and end are elements of this pattern.
           , ptrls :: Rules        -- ^ The user defined rules in this pattern
           , ptgns :: [A_Gen]       -- ^ The generalizations defined in this pattern
           , ptdcs :: Relations     -- ^ The relations that are declared in this pattern
           , ptups :: [Population]  -- ^ The user defined populations in this pattern
           , ptids :: [IdentityDef] -- ^ The identity definitions defined in this pattern
           , ptvds :: [ViewDef]     -- ^ The view definitions defined in this pattern
           , ptxps :: [Purpose]     -- ^ The purposes of elements defined in this pattern
           }   deriving (Typeable)    -- Show for debugging purposes
instance Eq Pattern where
  p==p' = ptnm p==ptnm p'
instance Unique Pattern where
  showUnique = optionalQuote . name

instance Named Pattern where
 name = ptnm
instance Traced Pattern where
 origin = ptpos


data A_RoleRule = A_RoleRule { arRoles :: [Role]
                             , arRules ::  [String] -- the names of the rules
                             , arPos ::   Origin
                             } deriving (Show)
data RuleOrigin = UserDefined     -- This rule was specified explicitly as a rule in the Ampersand script
                | Multiplicity    -- This rule follows implicitly from the Ampersand script (Because of a property) and generated by a computer
                | Identity        -- This rule follows implicitly from the Ampersand script (Because of a identity) and generated by a computer
                deriving (Show, Eq)
type Rules = Set.Set Rule
data Rule =
     Ru { rrnm ::     String                      -- ^ Name of this rule
        , formalExpression :: Expression          -- ^ The expression that should be True
        , rrfps ::    Origin                      -- ^ Position in the Ampersand file
        , rrmean ::   [Meaning]                  -- ^ Ampersand generated meaning (for all known languages)
        , rrmsg ::    [Markup]                    -- ^ User-specified violation messages, possibly more than one, for multiple languages.
        , rrviol ::   Maybe (PairView Expression) -- ^ Custom presentation for violations, currently only in a single language
        , rrdcl ::    Maybe (Prop,Relation)    -- ^ The property, if this rule originates from a property on a Relation
        , rrpat ::    Maybe String                -- ^ If the rule is defined in the context of a pattern, the name of that pattern.
        , r_usr ::    RuleOrigin                  -- ^ Where does this rule come from?
        , isSignal :: Bool                        -- ^ True if this is a signal; False if it is an invariant
        } deriving Typeable
instance Eq Rule where
  r==r' = name r==name r' && origin r==origin r' -- Origin should be here too: A check that they all have unique names is done after typechecking.
instance Unique Rule where
  showUnique = rrnm
instance Ord Rule where
  compare = compare `on` rrnm
instance Show Rule where
  showsPrec _ x
   = showString $ "RULE "++ (if null (name x) then "" else name x++": ")++ show (formalExpression x)
instance Traced Rule where
  origin = rrfps
instance Named Rule where
  name   = rrnm
instance Hashable Rule where
  hashWithSalt s rul = s 
    `hashWithSalt` (name rul)
    `hashWithSalt` (formalExpression rul)

data Conjunct = Cjct { rc_id ::         String -- string that identifies this conjunct ('id' rather than 'name', because
                                               -- this is an internal id that has no counterpart at the ADL level)
                     , rc_orgRules ::   Rules -- All rules this conjunct originates from
                     , rc_conjunct ::   Expression
                     , rc_dnfClauses :: [DnfClause]
                     } deriving (Show,Typeable)

data DnfClause = Dnf { antcs :: [Expression]
                     , conss :: [Expression]
                     }  deriving (Show, Eq) -- Show is for debugging purposes only.

{- The intended semantics of |Dnf ns ps| is the disjunction |foldr1 ( .\/. ) (map notCpl ns ++ ps)|.
   The list |ns| and |ps| are not guaranteed to be sorted or duplicate-free.
-}

instance Eq Conjunct where
  rc==rc' = rc_id rc==rc_id rc'
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
      , decprL :: String             -- ^ three strings, which form the pragma. E.g. if pragma consists of the three strings: "Person ", " is married to person ", and " in Vegas."
      , decprM :: String             -- ^    then a tuple ("Peter","Jane") in the list of links means that Person Peter is married to person Jane in Vegas.
      , decprR :: String
      , decMean :: [Meaning]          -- ^ the meaning of a relation, for each language supported by Ampersand.
      , decfpos :: Origin            -- ^ the position in the Ampersand source file where this relation is declared. Not all decalartions come from the ampersand souce file.
      , decusr ::  Bool              -- ^ if true, this relation is declared by an author in the Ampersand script; otherwise it was generated by Ampersand.
      , decpat ::  Maybe String      -- ^ If the relation is declared inside a pattern, the name of that pattern.
      , dechash :: Int
      } deriving (Typeable, Data)

instance Eq Relation where
  d == d' = dechash d == dechash d' && decnm d == decnm d' && decsgn d==decsgn d'

instance Ord Relation where
  compare a b =
    if name a == name b
    then compare (sign a) (sign b)
    else compare (name a) (name b)
instance Unique Relation where
  showUnique d =
    name d++uniqueShow False (decsgn d)
instance Hashable Relation where
   hashWithSalt s Relation{dechash = v} = s `hashWithSalt` v
instance Show Relation where  -- For debugging purposes only (and fatal messages)
  showsPrec _ decl
   = showString (name decl++showSign (sign decl))

showRel :: Relation -> String
showRel rel = name rel++"["++show (source rel) ++ "*"++ show (target rel)++"]"

data Meaning = Meaning { ameaMrk ::Markup} deriving (Show, Eq, Ord, Typeable, Data)
instance Unique Meaning where
  showUnique x = uniqueShow True x++" in "++(show.amLang.ameaMrk) x

instance Named Relation where
  name d = unpack (decnm d)
instance HasSignature Relation where
  sign = decsgn
instance Traced Relation where
  origin = decfpos

data IdentityDef = Id { idPos :: Origin        -- ^ position of this definition in the text of the Ampersand source file (filename, line number and column number).
                      , idLbl :: String        -- ^ the name (or label) of this Identity. The label has no meaning in the Compliant Service Layer, but is used in the generated user interface. It is not an empty string.
                      , idCpt :: A_Concept     -- ^ this expression describes the instances of this object, related to their context
                      , idPat :: Maybe String  -- ^ if defined within a pattern, then the name of that pattern.
                      , identityAts :: [IdentitySegment]  -- ^ the constituent attributes (i.e. name/expression pairs) of this identity.
                      } deriving (Eq,Show)
instance Named IdentityDef where
  name = idLbl
instance Traced IdentityDef where
  origin = idPos
instance Unique IdentityDef where
  showUnique = idLbl

data IdentitySegment = IdentityExp ObjectDef deriving (Eq, Show)  -- TODO: refactor to a list of terms

data ViewDef = Vd { vdpos :: Origin          -- ^ position of this definition in the text of the Ampersand source file (filename, line number and column number).
                  , vdlbl :: String          -- ^ the name (or label) of this View. The label has no meaning in the Compliant Service Layer, but is used in the generated user interface. It is not an empty string.
                  , vdcpt :: A_Concept       -- ^ the concept for which this view is applicable
                  , vdIsDefault :: Bool      -- ^ whether or not this is the default view for the concept
                  , vdhtml :: Maybe ViewHtmlTemplate -- ^ the html template for this view (not required since we may have other kinds of views as well in the future)
--                  , vdtext :: Maybe ViewText -- Future extension
                  , vdats :: [ViewSegment]   -- ^ the constituent attributes (i.e. name/expression pairs) of this view.
                  } deriving (Show)
instance Named ViewDef where
  name = vdlbl
instance Traced ViewDef where
  origin = vdpos
instance Unique ViewDef where
  showUnique vd = vdlbl vd++"_"++name (vdcpt vd) 
instance Eq ViewDef where
  a == b = vdlbl a == vdlbl b && vdcpt a == vdcpt b 
data ViewSegment = ViewSegment
     { vsmpos :: Origin
     , vsmlabel :: Maybe String
     , vsmSeqNr :: Integer
     , vsmLoad  :: ViewSegmentPayLoad
     } deriving Show
instance Traced ViewSegment where
  origin = vsmpos
data ViewSegmentPayLoad
                 = ViewExp { vsgmExpr :: Expression
                           }
                 | ViewText{ vsgmTxt  :: String
                           }deriving (Eq, Show)


-- | data structure A_Gen contains the CLASSIFY statements from an Ampersand script
--   CLASSIFY Employee ISA Person   translates to Isa (C "Person") (C "Employee")
--   CLASSIFY Workingstudent IS Employee/\Student   translates to IsE orig (C "Workingstudent") [C "Employee",C "Student"]
data A_Gen = Isa { genpos :: Origin
                 , genspc :: A_Concept      -- ^ specific concept
                 , gengen :: A_Concept      -- ^ generic concept
                 }
           | IsE { genpos :: Origin
                 , genspc :: A_Concept      -- ^ specific concept
                 , genrhs :: [A_Concept]    -- ^ concepts of which the conjunction is equivalent to the specific concept
                 } deriving (Typeable, Eq)
instance Traced A_Gen where
  origin = genpos
instance Unique A_Gen where
  showUnique a =
    case a of
      Isa{} -> uniqueShow False (genspc a)++" ISA "++uniqueShow False (gengen a)
      IsE{} -> uniqueShow False (genspc a)++" IS "++intercalate " /\\ " (map (uniqueShow False) (genrhs a))
instance Show A_Gen where
  -- This show is used in error messages. It should therefore not display the term's type
  showsPrec _ g =
    case g of
     Isa{} -> showString ("CLASSIFY "++show (genspc g)++" ISA "++show (gengen g))
     IsE{} -> showString ("CLASSIFY "++show (genspc g)++" IS "++intercalate " /\\ " (map show (genrhs g)))
instance Hashable A_Gen where
    hashWithSalt s g = 
      s `hashWithSalt` (genspc g)
        `hashWithSalt` (case g of 
                         Isa{} -> [genspc g]
                         IsE{} -> sort $ genrhs g 
                       )

data Interface = Ifc { ifcname ::     String        -- all roles for which an interface is available (empty means: available for all roles)
                     , ifcRoles ::    [Role]        -- all roles for which an interface is available (empty means: available for all roles)
                     , ifcObj ::      ObjectDef     -- NOTE: this top-level ObjectDef is contains the interface itself (ie. name and expression)
                     , ifcControls :: [Conjunct]    -- All conjuncts that must be evaluated after a transaction
                     , ifcPos ::      Origin        -- The position in the file (filename, line- and column number)
                     , ifcPrp ::      String        -- The purpose of the interface
                     } deriving Show

instance Eq Interface where
  s==s' = name s==name s'
instance Named Interface where
  name = ifcname
instance Traced Interface where
  origin = ifcPos
instance Unique Interface where
  showUnique = optionalQuote . name
-- Utility function for looking up interface refs
getInterfaceByName :: [Interface] -> String -> Interface
getInterfaceByName interfaces' nm = case [ ifc | ifc <- interfaces', name ifc == nm ] of
                                []    -> fatal $ "getInterface by name: no interfaces named "++show nm
                                [ifc] -> ifc
                                _     -> fatal $ "getInterface by name: multiple interfaces named "++show nm


class Object a where
 concept ::   a -> A_Concept      -- the type of the object
 fields ::    a -> [ObjectDef]       -- the objects defined within the object
 contextOf :: a -> Expression     -- the context expression

instance Object ObjectDef where
 concept obj = target (objExpression obj)
 fields  obj = case objmsub obj of
                 Nothing       -> []
                 Just InterfaceRef{} -> []
                 Just b@Box{}    -> map objE . filter isObjExp $ siObjs b
 contextOf   = objExpression

data BoxItem = 
        BxExpr {objE :: ObjectDef}
      | BxTxt {objT :: BoxTxt}
      deriving (Eq, Show)
instance Traced BoxItem where
  origin o 
    = case o of
        BxExpr{} -> origin . objE $ o
        BxTxt{} -> origin . objT $ o
isObjExp :: BoxItem -> Bool
isObjExp BxExpr{} = True
isObjExp BxTxt{} = False
data ObjectDef = 
    ObjectDef { objnm    :: String         -- ^ view name of the object definition. The label has no meaning in the Compliant Service Layer, but is used in the generated user interface if it is not an empty string.
           , objpos   :: Origin         -- ^ position of this definition in the text of the Ampersand source file (filename, line number and column number)
           , objExpression :: Expression -- ^ this expression describes the instances of this object, related to their context.
           , objcrud  :: Cruds          -- ^ CRUD as defined by the user 
           , objmView :: Maybe String   -- ^ The view that should be used for this object
           , objmsub  :: Maybe SubInterface -- ^ the fields, which are object definitions themselves.
           } deriving (Eq, Show)        -- just for debugging (zie ook instance Show BoxItem)
data BoxTxt =
    BoxTxt { objnm  :: String         -- ^ view name of the object definition. The label has no meaning in the Compliant Service Layer, but is used in the generated user interface if it is not an empty string.
           , objpos :: Origin
           , objtxt :: String
           } deriving (Eq, Show)
instance Named ObjectDef where
  name   = objnm
instance Traced ObjectDef where
  origin = objpos
instance Unique ObjectDef where
  showUnique = showUnique . origin
instance Named BoxTxt where
  name   = objnm
instance Traced BoxTxt where
  origin = objpos
instance Unique BoxItem where
  showUnique = showUnique . origin
data Cruds = Cruds { crudOrig :: Origin
                   , crudC :: Bool
                   , crudR :: Bool
                   , crudU :: Bool
                   , crudD :: Bool
                   } deriving (Eq, Show)
data SubInterface = Box { siConcept :: A_Concept
                        , siMClass  :: Maybe String
                        , siObjs    :: [BoxItem] 
                        }
                  | InterfaceRef 
                        { siIsLink :: Bool
                        , siIfcId  :: String  --id of the interface that is referenced to
                        } deriving (Eq, Show)



-- | Explanation is the intended constructor. It explains the purpose of the object it references.
--   The enrichment process of the parser must map the names (from PPurpose) to the actual objects
data Purpose  = Expl { explPos :: Origin     -- ^ The position in the Ampersand script of this purpose definition
                     , explObj :: ExplObj    -- ^ The object that is explained.
                     , explMarkup :: Markup   -- ^ This field contains the text of the explanation including language and markup info.
                     , explUserdefd :: Bool       -- ^ Is this purpose defined in the script?
                     , explRefIds :: [String]     -- ^ The references of the explaination
                     } deriving (Show, Typeable)
instance Eq Purpose where
  x0 == x1  =  explObj x0 == explObj x1 &&  
               origin x0  == origin x1 &&
               (amLang . explMarkup) x0 == (amLang . explMarkup) x1
instance Unique Purpose where
  showUnique p = showUnique (explMarkup p)
                   ++ uniqueShow True (explPos p)
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
  showUnique pop@ARelPopu{} = (showUnique.popdcl) pop ++ (showUnique.popps) pop
  showUnique pop@ACptPopu{} = (showUnique.popcpt) pop ++ (showUnique.popas) pop

type AAtomPairs = Set.Set AAtomPair
data AAtomPair
  = APair { apLeft  :: AAtomValue
          , apRight :: AAtomValue
          } deriving(Eq,Ord)
mkAtomPair :: AAtomValue -> AAtomValue -> AAtomPair
mkAtomPair = APair

instance Unique AAtomPair where
  showUnique apair = "("++(showUnique.apLeft) apair ++","++ (showUnique.apRight) apair++")"

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

instance Unique AAtomValue where   -- TODO:  this in incorrect! (AAtomValue should probably not be in Unique at all. We need to look into where this is used for.)
  showUnique pop@AAVString{}   = (show.aavhash) pop
  showUnique pop@AAVInteger{}  = (show.aavint) pop
  showUnique pop@AAVFloat{}    = (show.aavflt) pop
  showUnique pop@AAVBoolean{}  = (show.aavbool) pop
  showUnique pop@AAVDate{}     = (show.aadateDay) pop
  showUnique pop@AAVDateTime{} = (show.aadatetime) pop
  showUnique AtomValueOfONE    = "ONE"

aavstr :: AAtomValue -> String
aavstr = unpack.aavtxt

showValSQL :: AAtomValue -> String
showValSQL val =
  case val of
   AAVString{}  -> singleQuote . f . aavstr $ val
     where 
       f [] = []
       f (c:cs) 
         | c `elem` ['\'','\\'] 
                     = c : c : f cs
         | otherwise = c     : f cs
   AAVInteger{} -> show (aavint val)
   AAVBoolean{} -> show (aavbool val)
   AAVDate{}    -> singleQuote $ showGregorian (aadateDay val)
   AAVDateTime {} -> singleQuote $ DTF.formatTime DTF.defaultTimeLocale "%F %T" (aadatetime val) --NOTE: MySQL 5.5 does not comply to ISO standard. This format is MySQL specific
     --formatTime SL.defaultTimeLocale "%FT%T%QZ" (aadatetime val)
   AAVFloat{}   -> show (aavflt val)
   AtomValueOfONE{} -> "1"
singleQuote :: String -> String
singleQuote str = "'"++str++"'"

showValADL :: AAtomValue -> String
showValADL val =
  case val of
   AAVString{}  ->       aavstr val
   AAVInteger{} -> show (aavint val)
   AAVBoolean{} -> show (aavbool val)
   AAVDate{}    -> showGregorian (aadateDay val)
   AAVDateTime {} -> DTF.formatTime DTF.defaultTimeLocale "%FT%T%QZ" (aadatetime val)
   AAVFloat{}   -> show (aavflt val)
   AtomValueOfONE{} -> "1"

data ExplObj = ExplConceptDef ConceptDef
             | ExplRelation Relation
             | ExplRule String
             | ExplIdentityDef String
             | ExplViewDef String
             | ExplPattern String
             | ExplInterface String
             | ExplContext String
          deriving (Show ,Eq, Typeable)
instance Unique ExplObj where
  showUnique e = "Explanation of "++
    case e of
     (ExplConceptDef cd) -> uniqueShow True cd
     (ExplRelation d)    -> uniqueShow True d
     (ExplRule s)        -> "a Rule named "++s
     (ExplIdentityDef s) -> "an Ident named "++s
     (ExplViewDef s)     -> "a View named "++s
     (ExplPattern s)     -> "a Pattern named "++s
     (ExplInterface s)   -> "an Interface named "++s
     (ExplContext s)     -> "a Context named "++s

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
      | EMp1 PSingleton A_Concept      -- ^ constant PAtomValue, because when building the Expression, the TType of the concept isn't known yet.
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
  showUnique = show -- showA is not good enough: epsilons are disguised, so there can be several different
                    -- expressions with the same showA. 

instance Unique (PairView Expression) where
  showUnique = show
instance Unique (PairViewSegment Expression) where
  showUnique = show


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
l .==. r = if source l/=source r ||  target l/=target r then fatal ("Cannot equate (with operator \"==\") expression l of type "++show (sign l)++"\n   "++show l++"\n   with expression r of type "++show (sign r)++"\n   "++show r++".") else
           EEqu (l,r)
l .|-. r = if source l/=source r ||  target l/=target r then fatal ("Cannot include (with operator \"|-\") expression l of type "++show (sign l)++"\n   "++show l++"\n   with expression r of type "++show (sign r)++"\n   "++show r++".") else
           EInc (l,r)
l ./\. r = if source l/=source r ||  target l/=target r then fatal ("Cannot intersect (with operator \"/\\\") expression l of type "++show (sign l)++"\n   "++show l++"\n   with expression r of type "++show (sign r)++"\n   "++show r++".") else
           EIsc (l,r)
l .\/. r = if source l/=source r ||  target l/=target r then fatal ("Cannot unite (with operator \"\\/\") expression l of type "++show (sign l)++"\n   "++show l++"\n   with expression r of type "++show (sign r)++"\n   "++show r++".") else
           EUni (l,r)
l .-. r  = if source l/=source r ||  target l/=target r then fatal ("Cannot subtract (with operator \"-\") expression l of type "++show (sign l)++"\n   "++show l++"\n   with expression r of type "++show (sign r)++"\n   "++show r++".") else
           EDif (l,r)
l ./. r  = if target l/=target r then fatal ("Cannot residuate (with operator \"/\") expression l of type "++show (sign l)++"\n   "++show l++"\n   with expression r of type "++show (sign r)++"\n   "++show r++".") else
           ELrs (l,r)
l .\. r  = if source l/=source r then fatal ("Cannot residuate (with operator \"\\\") expression l of type "++show (sign l)++"\n   "++show l++"\n   with expression r of type "++show (sign r)++"\n   "++show r++".") else
           ERrs (l,r)
l .<>. r = if source r/=target l then fatal ("Cannot use diamond operator \"<>\") expression l of type "++show (sign l)++"\n   "++show l++"\n   with expression r of type "++show (sign r)++"\n   "++show r++".") else
           EDia (l,r)
l .:. r  = if source r/=target l then fatal ("Cannot compose (with operator \";\") expression l of type "++show (sign l)++"\n   "++show l++"\n   with expression r of type "++show (sign r)++"\n   "++show r++".") else
           ECps (l,r)
l .!. r  = if source r/=target l then fatal ("Cannot add (with operator \"!\") expression l of type "++show (sign l)++"\n   "++show l++"\n   with expression r of type "++show (sign r)++"\n   "++show r++".") else
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

showSign :: HasSignature a => a -> String
showSign x = let Sign s t = sign x in "["++name s++"*"++name t++"]"

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
   = PlainConcept { cpthash :: Int
                  , cptnm :: Text  -- ^PlainConcept nm represents the set of instances cs by name nm.
                  }
   | ONE  -- ^The universal Singleton: 'I'['Anything'] = 'V'['Anything'*'Anything']
    deriving (Typeable,Data,Ord,Eq)
type A_Concepts = Set.Set A_Concept
{- -- this is faster, so if you think Eq on concepts is taking a long time, try this..
instance Ord A_Concept where
  compare (PlainConcept{cpthash=v1}) (PlainConcept{cpthash=v2}) = compare v1 v2
  compare ONE ONE = EQ
  compare ONE (PlainConcept{}) = LT
  compare (PlainConcept{}) ONE = GT

instance Eq A_Concept where
  (==) a b = compare a b == EQ

-- SJC TODO: put "makeConcept" in a monad or something, and number them consecutively to avoid hash collisions
-}
  
makeConcept :: String -> A_Concept
makeConcept "ONE" = ONE
makeConcept v = PlainConcept (hash v) (pack v)

instance Unique A_Concept where
  showUnique = optionalQuote . name
instance Hashable A_Concept where
  hashWithSalt s cpt =
     s `hashWithSalt` (case cpt of
                        PlainConcept{} -> (0::Int) `hashWithSalt` cpthash cpt
                        ONE            -> 1::Int
                      )
instance Named A_Concept where
  name PlainConcept{cptnm = nm} = unpack nm
  name ONE = "ONE"

instance Show A_Concept where
  showsPrec _ c = showString (name c)

instance Unique (A_Concept, PSingleton) where
  showUnique (c,val) = show val++"["++showUnique c++"]"

data Signature = Sign A_Concept A_Concept deriving (Eq, Ord, Typeable, Generic, Data)
instance Hashable Signature
instance Show Signature where
  showsPrec _ (Sign s t) =
     showString (   "[" ++ show s ++ "*" ++ show t ++ "]" )
instance Unique Signature where
  showUnique (Sign s t) = "[" ++ uniqueShow False s ++ "*" ++ uniqueShow False t ++ "]"
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
  CI { ctxiGens         :: [A_Gen]      -- The generalisation relations in the context
     , representationOf :: A_Concept -> TType -- a list containing all user defined Representations in the context
     , multiKernels     :: [Typology] -- a list of typologies, based only on the CLASSIFY statements. Single-concept typologies are not included
     , reprList         :: [Representation] -- a list of all Representations
     , declDisambMap    :: Map.Map String (Map.Map SignOrd Expression) -- a map of declarations and the corresponding types
     , soloConcs        :: [Type] -- types not used in any declaration
     , gens_efficient   :: (Op1EqualitySystem Type) -- generalisation relations again, as a type system (including phantom types)
     } 
                       
instance Named Type where
  name v = case typeOrConcept v of
                Right (Just x) -> "Built-in type "++show x
                Right Nothing  -> "The Generic Built-in type"
                Left  x -> "Concept "++name x

typeOrConcept :: Type -> Either A_Concept (Maybe TType)
typeOrConcept (BuiltIn TypeOfOne)  = Left  ONE
typeOrConcept (UserConcept s)      = Left$ makeConcept s
typeOrConcept (BuiltIn x)          = Right (Just x)
typeOrConcept RepresentSeparator = Right Nothing

data Type = UserConcept String
          | BuiltIn TType
          | RepresentSeparator
          deriving (Eq,Ord,Show)

-- for faster comparison
newtype SignOrd = SignOrd Signature
instance Ord SignOrd where
  compare (SignOrd (Sign a b)) (SignOrd (Sign c d)) = compare (name a,name b) (name c,name d)
instance Eq SignOrd where
  (==) (SignOrd (Sign a b)) (SignOrd (Sign c d)) = (name a,name b) == (name c,name d)
   

-- | This function is meant to convert the PSingleton inside EMp1 to an AAtomValue,
--   after the expression has been built inside an A_Context. Only at that time
--   the TType is known, enabling the correct transformation.
--   To ensure that this function is not used too early, ContextInfo is required,
--   which only exsists after disambiguation.
safePSingleton2AAtomVal :: ContextInfo -> A_Concept -> PSingleton -> AAtomValue
safePSingleton2AAtomVal ci c val =
   case unsafePAtomVal2AtomValue typ (Just c) val of
     Left _ -> fatal . intercalate "\n  " $
                  [ "This should be impossible: after checking everything an unhandled singleton value found!"
                  , "Concept: "++show c
                  , "TType: "++show typ
                  , "Origin: "++show (origin val)
                  , "PAtomValue: "++case val of
                                      (PSingleton _ _ v) -> "PSingleton ("++show v++")"
                                      (ScriptString _ v) -> "ScriptString ("++show v++")"
                                      (XlsxString _ v)   -> "XlsxString ("++show v++")"
                                      (ScriptInt _ v)    -> "ScriptInt ("++show v++")"
                                      (ScriptFloat _ v)  -> "ScriptFloat ("++show v++")"
                                      (XlsxDouble _ v)   -> "XlsxDouble ("++show v++")"
                                      (ComnBool _ v)     -> "ComnBool ("++show v++")"
                                      (ScriptDate _ v)   -> "ScriptDate ("++show v++")"
                                      (ScriptDateTime _ v) -> "ScriptDateTime ("++show v++")"
                  ]
     Right x -> x
  where typ = representationOf ci c

-- SJC: Note about this code:
-- error messages are written here, and later turned into error messages via mkIncompatibleAtomValueError
-- Ideally, this module would import Ampersand.Input.ADL1.CtxError
-- that way, unsafePAtomVal2AtomValue could create a 'Origin -> Guarded AAtomValue' instead.
unsafePAtomVal2AtomValue :: TType -> Maybe A_Concept -> PAtomValue -> Either String AAtomValue
unsafePAtomVal2AtomValue typ mCpt pav =
  case unsafePAtomVal2AtomValue' typ mCpt pav of
    Left err -> Left err
    Right rawVal -> Right roundedVal
      where roundedVal =
             case rawVal of
              AAVDateTime t x -> -- Rounding is needed, to maximize the number of databases
                                 -- on wich this runs. (MySQL 5.5 only knows seconds)
                                 AAVDateTime t (truncateByFormat x)
                                  where
                                    truncateByFormat :: UTCTime  -> UTCTime
                                    truncateByFormat = f (DTF.parseTimeOrError True) . f DTF.formatTime
                                      where
                                        format = DTF.iso8601DateFormat (Just "%H:%M:%S")
                                    --    f:: DTF.TimeLocale -> String -> typ
                                        f fun = fun DTF.defaultTimeLocale format
              _          -> rawVal

unsafePAtomVal2AtomValue' :: TType -> Maybe A_Concept -> PAtomValue -> Either String AAtomValue
unsafePAtomVal2AtomValue' typ mCpt pav
  = case pav of
      PSingleton _ str mval
         -> case typ of
             Alphanumeric     -> Right (AAVString (hash str) typ (pack str))
             BigAlphanumeric  -> Right (AAVString (hash str) typ (pack str))
             HugeAlphanumeric -> Right (AAVString (hash str) typ (pack str))
             Password         -> Right (AAVString (hash str) typ (pack str))
             Object           -> Right (AAVString (hash str) typ (pack str))
             _                -> case mval of
                                   Nothing -> message str
                                   Just x -> unsafePAtomVal2AtomValue typ mCpt x
      ScriptString _ str
         -> case typ of
             Alphanumeric     -> Right (AAVString (hash str) typ (pack str))
             BigAlphanumeric  -> Right (AAVString (hash str) typ (pack str))
             HugeAlphanumeric -> Right (AAVString (hash str) typ (pack str))
             Password         -> Right (AAVString (hash str) typ (pack str))
             Binary           -> Left "Binary cannot be populated in an ADL script"
             BigBinary        -> Left "Binary cannot be populated in an ADL script"
             HugeBinary       -> Left "Binary cannot be populated in an ADL script"
             Date             -> message str
             DateTime         -> message str
             Boolean          -> message str
             Integer          -> message str
             Float            -> message str
             TypeOfOne        -> Left "ONE has a population of it's own, that cannot be modified"
             Object           -> Right (AAVString (hash str) typ (pack str))
      XlsxString _ str
         -> case typ of
             Alphanumeric     -> Right (AAVString (hash str) typ (pack str))
             BigAlphanumeric  -> Right (AAVString (hash str) typ (pack str))
             HugeAlphanumeric -> Right (AAVString (hash str) typ (pack str))
             Password         -> Right (AAVString (hash str) typ (pack str))
             Binary           -> Left "Binary cannot be populated in an ADL script"
             BigBinary        -> Left "Binary cannot be populated in an ADL script"
             HugeBinary       -> Left "Binary cannot be populated in an ADL script"
             Date             -> message str
             DateTime         -> message str
             Boolean          -> let table =
                                        [("TRUE", True), ("FALSE" , False)
                                        ,("YES" , True), ("NO"    , False)
                                        ,("WAAR", True), ("ONWAAR", False)
                                        ,("JA"  , True), ("NEE"   , False)
                                        ,("WEL" , True), ("NIET"  , False)
                                        ]
                                 in case lookup (map toUpper str) table of
                                    Just b -> Right (AAVBoolean typ b)
                                    Nothing -> Left $ "permitted Booleans: "++(show . map (camelCase . fst)) table
                                   where camelCase []     = []
                                         camelCase (c:xs) = toUpper c: map toLower xs

             Integer          -> case maybeRead str  of
                                   Just i  -> Right (AAVInteger typ i)
                                   Nothing -> message str
             Float         -> case maybeRead str of
                                   Just r  -> Right (AAVFloat typ r)
                                   Nothing -> message str
             TypeOfOne        -> Left "ONE has a population of it's own, that cannot be modified"
             Object           -> Right (AAVString (hash str) typ (pack str))
      ScriptInt _ i
         -> case typ of
             Alphanumeric     -> message i
             BigAlphanumeric  -> message i
             HugeAlphanumeric -> message i
             Password         -> message i
             Binary           -> Left "Binary cannot be populated in an ADL script"
             BigBinary        -> Left "Binary cannot be populated in an ADL script"
             HugeBinary       -> Left "Binary cannot be populated in an ADL script"
             Date             -> message i
             DateTime         -> message i
             Boolean          -> message i
             Integer          -> Right (AAVInteger typ i)
             Float            -> Right (AAVFloat typ (fromInteger i)) -- must convert, because `34.000` is lexed as Integer
             TypeOfOne        -> Left "ONE has a population of it's own, that cannot be modified"
             Object           -> message i
      ScriptFloat _ x
         -> case typ of
             Alphanumeric     -> message x
             BigAlphanumeric  -> message x
             HugeAlphanumeric -> message x
             Password         -> message x
             Binary           -> Left "Binary cannot be populated in an ADL script"
             BigBinary        -> Left "Binary cannot be populated in an ADL script"
             HugeBinary       -> Left "Binary cannot be populated in an ADL script"
             Date             -> message x
             DateTime         -> message x
             Boolean          -> message x
             Integer          -> message x
             Float            -> Right (AAVFloat typ x)
             TypeOfOne        -> Left "ONE has a population of it's own, that cannot be modified"
             Object           -> message x
      XlsxDouble _ d
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
             Boolean          -> message d
             Integer          -> if frac == 0
                                 then Right (AAVInteger typ int)
                                 else message d
                                  where
                                    (int,frac) = properFraction d
             Float            -> Right (AAVFloat typ d)
             TypeOfOne        -> Left "ONE has a population of it's own, that cannot be modified"
             Object           -> relaxXLSXInput d
      ComnBool _ b
         -> if typ == Boolean
            then Right (AAVBoolean typ b)
            else message b
      ScriptDate _ x
         -> if typ == Date
            then Right (AAVDate typ x)
            else message x
      ScriptDateTime _ x
         -> if typ == DateTime
            then Right (AAVDateTime typ x)
            else message x

   where
     relaxXLSXInput :: Double -> Either String AAtomValue
     relaxXLSXInput v = Right (AAVString (hash v) typ (pack (neat (show v))))
       where neat :: String -> String
             neat s 
               | onlyZeroes dotAndAfter = beforeDot
               | otherwise = s
               where (beforeDot, dotAndAfter) = span (/= '.') s
                     onlyZeroes s' =
                      case s' of 
                       [] -> True
                       '.':zeros ->  nub zeros == "0"
                       _ -> False
     message :: Show x => x -> Either String a
     message x = Left . intercalate "\n    " $
                 ["Representation mismatch"
                 , "Found: `"++show x++"`,"
                 , "as representation of an atom in concept `"++name c++"`."
                 , "However, the representation-type of that concept is "++implicitly
                 , "defined as "++show expected++". The found value does not match that type."
                 ]++ example
        where
          c = fromMaybe (fatal "Representation mismatch without concept known should not happen.") mCpt
          expected = if typ == Object then Alphanumeric else typ
          implicitly = if typ == Object then "(implicitly) " else ""
          example :: [String]
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
              _                -> fatal $ "There is no example denotational syntax for a value of type `"++show typ++"`." 
     dayZeroExcel = addDays (-2) (fromGregorian 1900 1 1) -- Excel documentation tells that counting starts a jan 1st, however, that isn't totally true.
     maybeRead :: Read a => String -> Maybe a
     maybeRead = fmap fst . listToMaybe . reads


-- | The typology of a context is the partioning of the concepts in that context into 
--   sets such that (isa\/isa~)*;typology |- typology
--   Note, that with isa we only refer to the relations defined by CLASSIFY statements, 
--   not named relations with the same properties ( {UNI,INJ,TOT} or {UNI,INJ,SUR} )
data Typology = Typology { tyroot :: A_Concept -- the most generic concept in the typology 
                         , tyCpts :: [A_Concept] -- all concepts, from generic to specific
                         } deriving Show

