{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings#-}
module Ampersand.Core.ParseTree (
     P_Context(..), mergeContexts
   , Meta(..)
   , P_RoleRule(..)
   , Role(..)
   , P_Pattern(..)
   , P_Relation(..), mergeRels
   , Term(..), TermPrim(..), P_NamedRel(..)
   , PairView(..), PairViewSegment(..), PairViewTerm(..), PairViewSegmentTerm(..)
   , HTMLTemplateUsage(..), TemplateKeyValue(..)
   , ViewUsage(..)
   , SrcOrTgt(..)
   , P_Rule(..)
   , ConceptDef(..)
   , Representation(..), TType(..)
   , P_Population(..)
   , PAtomPair(..), PAtomValue(..), mkPair, makePSingleton
   , P_BoxItemTermPrim, P_SubInterface, P_Interface(..), P_IClass(..), P_BoxItem(..), P_SubIfc(..)
   , P_Cruds(..)
   , P_IdentDef, P_IdentDf(..) , P_IdentSegment, P_IdentSegmnt(..)
   , P_ViewDef , P_ViewSegment(..), HtmlTemplateSpec(..)
   , P_ViewD(..) , P_ViewSegmtPayLoad(..)

   , PPurpose(..),PRef2Obj(..),PMeaning(..),PMessage(..)

   , P_Concept(..), P_Sign(..)
   , mkPConcept
   , PClassify(..)

   , P_Markup(..)

   , Prop(..), Props
   -- Inherited stuff:
   , module Ampersand.Input.ADL1.FilePos
  ) where
import           Ampersand.Basics hiding (foldr, sequence, concatMap)
import           Ampersand.Input.ADL1.FilePos
import           Data.Foldable (concatMap)
import qualified RIO.List as L
import qualified RIO.NonEmpty as NE
import qualified RIO.Set as Set
import qualified RIO.Text as T
import           RIO.Time
import           Data.Traversable
import           Data.Typeable (typeOf)

data P_Context
   = PCtx{ ctx_nm ::     Text             -- ^ The name of this context
         , ctx_pos ::    [Origin]           -- ^ The origins of the context. A context can be a merge of a file including other files c.q. a list of Origin.
         , ctx_lang ::   Maybe Lang         -- ^ The language specified on the top-level context. If omitted, English will be the default.
         , ctx_markup :: Maybe PandocFormat -- ^ The default markup format for free text in this context
         , ctx_pats ::   [P_Pattern]        -- ^ The patterns defined in this context
         , ctx_rs ::     [P_Rule TermPrim]  -- ^ All user defined rules in this context, but outside patterns and outside processes
         , ctx_ds ::     [P_Relation]       -- ^ The relations defined in this context, outside the scope of patterns
         , ctx_cs ::     [ConceptDef]       -- ^ The concept definitions defined in this context, outside the scope of patterns
         , ctx_ks ::     [P_IdentDef]       -- ^ The identity definitions defined in this context, outside the scope of patterns
         , ctx_rrules :: [P_RoleRule]       -- ^ The MAINTAIN definitions defined in this context, outside the scope of patterns
         , ctx_reprs ::  [Representation]
         , ctx_vs ::     [P_ViewDef]        -- ^ The view definitions defined in this context, outside the scope of patterns
         , ctx_gs ::     [PClassify]        -- ^ The gen definitions defined in this context, outside the scope of patterns
         , ctx_ifcs ::   [P_Interface]      -- ^ The interfaces defined in this context
         , ctx_ps ::     [PPurpose]         -- ^ The purposes defined in this context, outside the scope of patterns and processes
         , ctx_pops ::   [P_Population]     -- ^ The populations defined in this context (without patterns), from POPULATION statements as well as from Relation declarations
         , ctx_metas ::  [Meta]             -- ^ generic meta information (name/value pairs) that can be used for experimenting without having to modify the adl syntax
         } deriving Show --for QuickCheck

instance Eq P_Context where
  c1 == c2  =  name c1 == name c2
instance Named P_Context where
  name = ctx_nm

-- for declaring name/value pairs with information that is built in to the adl syntax yet
data Meta = Meta { pos :: Origin
              , mtName :: Text
              , mtVal :: Text
              } deriving (Show)
instance Traced Meta where
  origin = pos

-- | A RoleRule r means that a role called 'mRoles r' must maintain the process rule called 'mRules r'
data P_RoleRule
   = Maintain
     { pos :: Origin      -- ^ position in the Ampersand script
     , mRoles :: NE.NonEmpty Role    -- ^ names of a role
     , mRules :: NE.NonEmpty Text  -- ^ names of a Rule
     } deriving (Show) -- deriving (Show) is just for debugging
instance Traced P_RoleRule where
 origin = pos

data Role = Role Text
          | Service Text
           deriving (Show, Typeable, Data)   -- deriving (Show) is just for debugging
instance Ord Role where
  compare a b = compare (name a) (name b)
instance Eq Role where
  a == b = compare a b == EQ
instance Named Role where
 name (Role nm) = nm
 name (Service nm) = nm
instance Unique Role where
 showUnique = name

data P_Pattern
   = P_Pat { pos ::      Origin            -- ^ the starting position in the file in which this pattern was declared.
           , pt_nm ::    Text            -- ^ Name of this pattern
           , pt_rls ::   [P_Rule TermPrim] -- ^ The user defined rules in this pattern
           , pt_gns ::   [PClassify]       -- ^ The generalizations defined in this pattern
           , pt_dcs ::   [P_Relation]      -- ^ The relations that are declared in this pattern
           , pt_RRuls :: [P_RoleRule]      -- ^ The assignment of roles to rules.
           , pt_cds ::   [ConceptDef]      -- ^ The concept definitions defined in this pattern
           , pt_Reprs :: [Representation]  -- ^ The type into which concepts is represented
           , pt_ids ::   [P_IdentDef]      -- ^ The identity definitions defined in this pattern
           , pt_vds ::   [P_ViewDef]       -- ^ The view definitions defined in this pattern
           , pt_xps ::   [PPurpose]        -- ^ The purposes of elements defined in this pattern
           , pt_pop ::   [P_Population]    -- ^ The populations that are local to this pattern
           , pt_end ::   Origin            -- ^ the end position in the file in which this pattern was declared.
           } deriving Show -- for QuickCheck

instance Ord P_Pattern where
 compare a b = case compare (name a) (name b) of
     EQ -> fromMaybe (fatal . T.intercalate "\n" $
                        ["P_Pattern should have a non-fuzzy Origin."
                        , tshow (origin a)
                        , tshow (origin b)
                        ])
                     (maybeOrdering (origin a) (origin b))
     x -> x  
instance Eq P_Pattern where
  a == b = compare a b == EQ
instance Named P_Pattern where
 name = pt_nm

instance Traced P_Pattern where
 origin = pos

data ConceptDef
   = Cd  { pos :: Origin   -- ^ The position of this definition in the text of the Ampersand source (filename, line number and column number).
         , cdcpt :: Text   -- ^ The name of the concept for which this is the definition. If there is no such concept, the conceptdefinition is ignored.
         , cddef :: Text   -- ^ The textual definition of this concept.
         , cdref :: Text   -- ^ A label meant to identify the source of the definition. (useful as LaTeX' symbolic reference)
         , cdfrom:: Text   -- ^ The name of the pattern or context in which this concept definition was made
         }   deriving (Show,Typeable)
instance Ord ConceptDef where
 compare a b = case compare (name a) (name b) of
     EQ -> fromMaybe (fatal . T.intercalate "\n" $
                        ["ConceptDef should have a non-fuzzy Origin."
                        , tshow (origin a)
                        , tshow (origin b)
                        ])
                     (maybeOrdering (origin a) (origin b))
     x -> x  
instance Eq ConceptDef where
  a == b = compare a b == EQ
instance Unique ConceptDef where
  showUnique cd = cdcpt cd<>"At"<>tshow (typeOf x) <>"_" <> tshow x
    where x = origin cd
instance Traced ConceptDef where
 origin = pos
instance Named ConceptDef where
 name = cdcpt
data Representation
  = Repr { pos  :: Origin
         , reprcpts  :: NE.NonEmpty P_Concept  -- ^ the concepts
         , reprdom :: TType     -- the type of the concept the atom is in
         } deriving (Show)
instance Traced Representation where
 origin = pos

data TType
  = Alphanumeric | BigAlphanumeric | HugeAlphanumeric | Password
  | Binary | BigBinary | HugeBinary
  | Date | DateTime
  | Boolean | Integer | Float | Object
  | TypeOfOne --special type for the special concept ONE.
     deriving (Eq, Ord, Typeable, Enum, Bounded)
instance Unique TType where
 showUnique = tshow
instance Show TType where
  show tt = case tt of
    Alphanumeric      ->   "ALPHANUMERIC"
    BigAlphanumeric   ->   "BIGALPHANUMERIC"
    HugeAlphanumeric  ->   "HUGEALPHANUMERIC"
    Password          ->   "PASSWORD"
    Binary            ->   "BINARY"
    BigBinary         ->   "BIGBINARY"
    HugeBinary        ->   "HUGEBINARY"
    Date              ->   "DATE"
    DateTime          ->   "DATETIME"
    Boolean           ->   "BOOLEAN"
    Integer           ->   "INTEGER"
    Float             ->   "FLOAT"
    Object            ->   "OBJECT"
    TypeOfOne         ->   "TYPEOFONE"
data P_Relation =
      P_Relation { dec_nm :: Text    -- ^ the name of the relation
            , dec_sign :: P_Sign    -- ^ the type. Parser must guarantee it is not empty.
            , dec_prps :: Props     -- ^ the user defined multiplicity properties (Uni, Tot, Sur, Inj) and algebraic properties (Sym, Asy, Trn, Rfx)
            , dec_pragma :: [Text]  -- ^ Three strings, which form the pragma. E.g. if pragma consists of the three strings: "Person ", " is married to person ", and " in Vegas."
                                      -- ^    then a tuple ("Peter","Jane") in the list of links means that Person Peter is married to person Jane in Vegas.
            , dec_Mean :: [PMeaning]  -- ^ the optional meaning of a relation, possibly more than one for different languages.
            , pos :: Origin    -- ^ the position in the Ampersand source file where this relation is declared. Not all relations come from the ampersand souce file.
            } deriving (Show) --For QuickCheck error messages only!

-- | Equality on P_Relation
--   Normally, equality on relations means equality of both name (dec_nm) and signature (dec_sign).
--   However, in the parser, we need to distinguish between two relations with the same name and signature when they are in different locations.
--   That occurs for example if we need to locate a copy of a relation for generating an error message.
--   For this reason, equality in the P-structure is defined on origin.
--   It is easy to see that if the locations are the same, then the relations must be the same.
--   But is that true all the time? ... No. If one or both origins are unknown, we revert to comparing name and signature.
--   This is still not true for MEATGRINDER stuff!
--   So,
-- DO NOT USE ORD and EQ on P_Relation!
instance Named P_Relation where
 name = dec_nm
instance Traced P_Relation where
 origin = pos

-- | The union of relations requires the conservation of properties of relations, so it is called 'merge' rather than 'union'.
--   Relations with the same signature are merged. Relations with different signatures are left alone.
mergeRels :: [P_Relation] -> [P_Relation]
mergeRels rs = map fun (eqCl signat rs) -- each equiv. class contains at least 1 element, so foldr1 is just right!
  where
    fun :: NonEmpty P_Relation -> P_Relation
    fun rels
     = P_Relation { dec_nm     = name r0
             , dec_sign   = dec_sign r0
             , dec_prps   = Set.unions (fmap dec_prps rels)
             , dec_pragma = case NE.filter (not . T.null . T.concat . dec_pragma) rels of
                              []  -> dec_pragma r0
                              h:_ -> dec_pragma h
             , dec_Mean   = L.nub $ concatMap dec_Mean rels
             , pos        = case NE.filter (not . isFuzzyOrigin . origin) rels of
                              []  -> origin r0
                              h:_ -> origin h
             }
         where (r0 :| _ ) = rels
    signat rel = (name rel, pSrc (dec_sign rel), pTgt (dec_sign rel))

data PAtomPair
  = PPair { pos :: Origin
          , ppLeft  :: PAtomValue
          , ppRight :: PAtomValue
          } deriving (Show) -- Show is for QuickCheck error messages and/or input redundancy removal only!
instance Ord PAtomPair where
  compare a b = compare (ppLeft a, ppRight a) (ppLeft b, ppRight b)
instance Eq PAtomPair where
  a == b = compare a b == EQ
instance Traced PAtomPair where
  origin = pos
instance Flippable PAtomPair where
  flp pr = pr{ppLeft = ppRight pr
             ,ppRight = ppLeft pr}
--data PSingleton
--  = PSingleton { pos :: Origin
--               , psRaw  :: Text
--               , psInterprets :: [PAtomValue]
--               }
--instance Show PSingleton where
-- show = psRaw
--instance Eq PSingleton where
--   a == b = compare a b == EQ
--instance Ord PSingleton where
-- compare a b = compare (psRaw a) (psRaw b)
--instance Traced PSingleton where
-- origin = pos
--type PSingleton = PAtomValue
makePSingleton :: Text -> PAtomValue
makePSingleton s = PSingleton (Origin "ParseTree.hs") s Nothing
--   PSingleton { psOrig =Origin "ParseTree.hs"
--              , psRaw = s
--              , psInterprets = fatal "Probably no need to make something up..."
--              }
data PAtomValue
  = PSingleton Origin Text (Maybe PAtomValue)
  | ScriptString Origin Text -- string from script char to enquote with when printed
  | XlsxString Origin Text
  | ScriptInt Origin Integer
  | ScriptFloat Origin Double
  | XlsxDouble Origin Double
  | ComnBool Origin Bool
  | ScriptDate Origin Day
  | ScriptDateTime Origin UTCTime
   deriving (Typeable, Data)
instance Show PAtomValue where -- Used for showing in Expressions as PSingleton
 show pav =
  case pav of
    PSingleton   _ s _ -> show s
    ScriptString   _ s -> show s
    XlsxString     _ s -> show s
    ScriptInt      _ i -> show i
    ScriptFloat    _ d -> show d
    XlsxDouble     o d -> fatal ("We got a value "<>tshow d<>" from "<>tshow o<>", which has to be shown in an expression, however the technicaltype is not known.")
    ComnBool       _ b -> show b
    ScriptDate     _ x -> show x
    ScriptDateTime _ x -> show x
  
instance Eq PAtomValue where
  a == b = compare a b == EQ

instance Ord PAtomValue where
  compare a b =
   case (a,b) of
    (PSingleton  _ x _ , PSingleton   _ x' _) -> compare x x'
    (PSingleton{}      , _                  ) -> GT
    (ScriptString   _ x, ScriptString   _ x') -> compare x x'
    (ScriptString{}    , _                  ) -> GT
    (XlsxString     _ x, XlsxString     _ x') -> compare x x'
    (XlsxString{}      , _                  ) -> GT
    (ScriptInt      _ x, ScriptInt      _ x') -> compare x x'
    (ScriptInt{}       , _                  ) -> GT
    (ScriptFloat    _ x, ScriptFloat    _ x') -> compare x x'
    (ScriptFloat{}     , _                  ) -> GT
    (XlsxDouble     _ x, XlsxDouble     _ x') -> compare x x'
    (XlsxDouble{}      , _                  ) -> GT
    (ScriptDate     _ x, ScriptDate     _ x') -> compare x x'
    (ScriptDate{}      , _                  ) -> GT
    (ScriptDateTime _ x, ScriptDateTime _ x') -> compare x x'
    (ScriptDateTime{}  , _                  ) -> GT
    (ComnBool       _ x, ComnBool       _ x') -> compare x x'
    (ComnBool{}        , _                  ) -> GT
instance Traced PAtomValue where
  origin pav =
   case pav of
    PSingleton   o _ _ -> o
    ScriptString   o _ -> o
    XlsxString     o _ -> o
    ScriptInt      o _ -> o
    ScriptFloat    o _ -> o
    XlsxDouble     o _ -> o
    ComnBool       o _ -> o
    ScriptDate     o _ -> o
    ScriptDateTime o _ -> o
instance Unique PAtomValue where
  showUnique = tshow
mkPair :: Origin -> PAtomValue -> PAtomValue -> PAtomPair
mkPair o l r
   = PPair { pos   = o
           , ppLeft  = l
           , ppRight = r}

data TermPrim
   = PI Origin                              -- ^ identity element without a type
                                            --   At parse time, there may be zero or one element in the list of concepts.
                                            --   Reason: when making eqClasses, the least element of that class is used as a witness of that class
                                            --   to know whether an eqClass represents a concept, we only look at its witness
                                            --   By making Pid the first in the data decleration, it becomes the least element for "deriving Ord".
   | Pid Origin P_Concept                   -- ^ identity element restricted to a type
   | Patm Origin PAtomValue (Maybe P_Concept)   -- ^ a singleton atom, possibly with a type. The list contains denotational equivalent values
                                                  --   eg, when `123` is found by the parser, the list will contain both interpretations as
                                                  --   the Text "123" or as Integer 123.
                                                  --   Since everything between the single quotes can allways be interpretated as a Text,
                                                  --   it is quaranteed that the list contains the interpretation as Text, and thus cannot
                                                  --   be empty.
   | PVee Origin                            -- ^ the complete relation, of which the type is yet to be derived by the type checker.
   | Pfull Origin P_Concept P_Concept       -- ^ the complete relation, restricted to a type.
                                            --   At parse time, there may be zero, one or two elements in the list of concepts.
   | PNamedR P_NamedRel
   deriving (Show) --For QuickCheck error messages only!

data P_NamedRel = PNamedRel { pos :: Origin, p_nrnm :: Text, p_mbSign :: Maybe P_Sign }
   deriving Show

instance Eq P_NamedRel where
     nr==nr'
      = case (p_mbSign nr, p_mbSign nr') of
             (Just sgn, Just sgn')  -> p_nrnm nr == p_nrnm nr' && sgn == sgn'
             _                      -> False

data Term a
   = Prim a
   | PEqu Origin (Term a) (Term a)  -- ^ equivalence             =
   | PInc Origin (Term a) (Term a)  -- ^ inclusion               |-
   | PIsc Origin (Term a) (Term a)  -- ^ intersection            /\
   | PUni Origin (Term a) (Term a)  -- ^ union                   \/
   | PDif Origin (Term a) (Term a)  -- ^ difference              -
   | PLrs Origin (Term a) (Term a)  -- ^ left residual           /
   | PRrs Origin (Term a) (Term a)  -- ^ right residual          \
   | PDia Origin (Term a) (Term a)  -- ^ diamond                 <>
   | PCps Origin (Term a) (Term a)  -- ^ composition             ;
   | PRad Origin (Term a) (Term a)  -- ^ relative addition       !
   | PPrd Origin (Term a) (Term a)  -- ^ cartesian product       #
   | PKl0 Origin (Term a)           -- ^ Rfx.Trn closure         *  (Kleene star)
   | PKl1 Origin (Term a)           -- ^ Transitive closure      +  (Kleene plus)
   | PFlp Origin (Term a)           -- ^ conversion (flip, wok)  ~
   | PCpl Origin (Term a)           -- ^ Complement
   | PBrk Origin (Term a)           -- ^ bracketed expression ( ... )
   deriving (Show) -- deriving Show for debugging purposes
instance Functor Term where fmap = fmapDefault
instance Foldable Term where foldMap = foldMapDefault
instance Traversable Term where
 traverse f' x
  = case x of
    Prim a -> Prim <$> f' a
    PEqu o a b -> PEqu o <$> f a <*> f b
    PInc o a b -> PInc o <$> f a <*> f b
    PIsc o a b -> PIsc o <$> f a <*> f b
    PUni o a b -> PUni o <$> f a <*> f b
    PDif o a b -> PDif o <$> f a <*> f b
    PLrs o a b -> PLrs o <$> f a <*> f b
    PRrs o a b -> PRrs o <$> f a <*> f b
    PDia o a b -> PDia o <$> f a <*> f b
    PCps o a b -> PCps o <$> f a <*> f b
    PRad o a b -> PRad o <$> f a <*> f b
    PPrd o a b -> PPrd o <$> f a <*> f b
    PKl0 o a   -> PKl0 o <$> f a
    PKl1 o a   -> PKl1 o <$> f a
    PFlp o a   -> PFlp o <$> f a
    PCpl o a   -> PCpl o <$> f a
    PBrk o a   -> PBrk o <$> f a
  where f = traverse f'

instance Functor P_SubIfc where fmap = fmapDefault
instance Foldable P_SubIfc where foldMap = foldMapDefault
instance Traversable P_SubIfc where
  traverse _ (P_InterfaceRef o a b) = pure (P_InterfaceRef o a b)
  traverse f (P_Box o c lst) = P_Box o c <$> traverse (traverse f) lst

instance Traced (P_SubIfc a) where
 origin = pos

instance Functor P_BoxItem where fmap = fmapDefault
instance Foldable P_BoxItem where foldMap = foldMapDefault
instance Traversable P_BoxItem where
 traverse f (P_BxExpr nm pos' ctx mCrud mView msub)
  = (\ctx' msub'-> P_BxExpr nm pos' ctx' mCrud mView msub') 
       <$> traverse f ctx
       <*> traverse (traverse f) msub
 traverse _ (P_BxTxt  nm pos' str) = pure (P_BxTxt  nm pos' str)

instance Traced TermPrim where
 origin e = case e of
   PI orig        -> orig
   Pid orig _     -> orig
   Patm orig _ _  -> orig
   PVee orig      -> orig
   Pfull orig _ _ -> orig
   PNamedR r      -> origin r

--instance Named TermPrim where
-- name e = case e of
--   PI _        -> "I"
--   Pid _ _     -> "I"
--   Patm _ s _  -> s
--   PVee _      -> "V"
--   Pfull _ _ _ -> "V"
--   PNamedR r   -> name r
--
instance Traced P_NamedRel where
  origin (PNamedRel o _ _) = o

instance Named P_NamedRel where
  name (PNamedRel _ nm _) = nm

instance Traced a => Traced (Term a) where
 origin e = case e of
   Prim a         -> origin a
   PEqu orig _ _  -> orig
   PInc orig _ _  -> orig
   PIsc orig _ _  -> orig
   PUni orig _ _  -> orig
   PDif orig _ _  -> orig
   PLrs orig _ _  -> orig
   PRrs orig _ _  -> orig
   PDia orig _ _  -> orig
   PCps orig _ _  -> orig
   PRad orig _ _  -> orig
   PPrd orig _ _  -> orig
   PKl0 orig _    -> orig
   PKl1 orig _    -> orig
   PFlp orig _    -> orig
   PCpl orig _    -> orig
   PBrk orig _    -> orig

data SrcOrTgt = Src | Tgt deriving (Show, Eq, Ord, Generic, Enum, Bounded)
instance Hashable SrcOrTgt
instance Flippable SrcOrTgt where
  flp Src = Tgt
  flp Tgt = Src

newtype PairView a = PairView { ppv_segs :: NE.NonEmpty (PairViewSegment a) } deriving (Show, Typeable, Eq, Generic)
instance Hashable a => Hashable (PairView a)
instance Traced a => Traced (PairView a) where
  origin = origin . NE.head . ppv_segs
data PairViewSegment a =
    PairViewText{ pos :: Origin
                , pvsStr :: Text
                }
  | PairViewExp { pos :: Origin
                , pvsSoT :: SrcOrTgt
                , pvsExp :: a
                } deriving (Show, Typeable, Generic)
instance Eq (PairViewSegment a) where
 p1 == p2 = compare p1 p2 == EQ
instance Ord (PairViewSegment a) where
 compare a b = fromMaybe 
    (fatal . T.intercalate "\n" $
       ["P_Rule a should have a non-fuzzy Origin."
       , tshow (origin a)
       , tshow (origin b)
       ])
    (maybeOrdering (origin a) (origin b))
instance Hashable a => Hashable (PairViewSegment a)
instance Traced (PairViewSegment a) where
  origin = pos

-- | the newtype to make it possible for a PairView to be disambiguatable: it must be of the form "d a" instead of "d (Term a)"
newtype PairViewTerm a = PairViewTerm (PairView (Term a))
newtype PairViewSegmentTerm a = PairViewSegmentTerm (PairViewSegment (Term a))

instance Traversable PairViewSegmentTerm where
  traverse f (PairViewSegmentTerm x) = PairViewSegmentTerm <$> traverse (traverse f) x
instance Functor PairViewSegmentTerm where fmap = fmapDefault
instance Foldable PairViewSegmentTerm where foldMap = foldMapDefault
instance Traversable PairViewTerm where
  traverse f (PairViewTerm x) = PairViewTerm <$> traverse (traverse f) x
instance Functor PairViewTerm where fmap = fmapDefault
instance Foldable PairViewTerm where foldMap = foldMapDefault
instance Traversable PairViewSegment where
  traverse _ (PairViewText ori s) = pure (PairViewText ori s)
  traverse f (PairViewExp ori st x) = PairViewExp ori st <$> f x
instance Functor PairViewSegment where fmap = fmapDefault
instance Foldable PairViewSegment where foldMap = foldMapDefault
instance Traversable PairView where
  traverse f (PairView s) = PairView <$> traverse (traverse f) s
instance Functor PairView where fmap = fmapDefault
instance Foldable PairView where foldMap = foldMapDefault

data P_Rule a  = P_Rule 
        { pos ::  Origin            -- ^ Position in the Ampersand file
        , rr_nm ::   Text            -- ^ Name of this rule
        , rr_exp ::  Term a            -- ^ The rule expression
        , rr_mean :: [PMeaning]        -- ^ User-specified meanings, possibly more than one, for multiple languages.
        , rr_msg ::  [PMessage]        -- ^ User-specified violation messages, possibly more than one, for multiple languages.
        , rr_viol :: Maybe (PairView (Term a))  -- ^ Custom presentation for violations, currently only in a single language
        } deriving Show
instance Ord (P_Rule a) where
 compare a b = case compare (name a) (name b) of
     EQ -> fromMaybe (fatal . T.intercalate "\n" $
                        ["P_Rule a should have a non-fuzzy Origin."
                        , tshow (origin a)
                        , tshow (origin b)
                        ])
                     (maybeOrdering (origin a) (origin b))
     x -> x  
instance Eq (P_Rule a) where --Required for merge of P_Contexts
  a == b = compare a b == EQ
instance Traced (P_Rule a) where
 origin = pos
instance Functor P_Rule where fmap = fmapDefault
instance Foldable P_Rule where foldMap = foldMapDefault
instance Traversable P_Rule where
 traverse f (P_Rule fps nm expr mean msg viol)
  = (\e v -> P_Rule fps nm e mean msg v) <$> traverse f expr <*> traverse (traverse (traverse f)) viol

instance Named (P_Rule a) where
 name = rr_nm

newtype PMeaning = PMeaning P_Markup
         deriving (Show, Eq)
newtype PMessage = PMessage P_Markup
         deriving Show
data P_Markup =
    P_Markup  { mLang   :: Maybe Lang
              , mFormat :: Maybe PandocFormat
              , mString :: Text
              } deriving (Show,Eq) -- for debugging only

data P_Population
  = P_RelPopu { p_src   :: Maybe P_Concept -- a separate src and tgt instead of "Maybe Sign", such that it is possible to specify only one of these.
              , p_tgt   :: Maybe P_Concept -- these src and tgt must be more specific than the P_NamedRel
              , pos     :: Origin       -- the origin
              , p_nmdr  :: P_NamedRel   -- the named relation
              , p_popps :: [PAtomPair]  -- the contents
              }
  | P_CptPopu { pos     :: Origin  -- the origin
              , p_cpt   :: P_Concept  -- the concept the population belongs to
              , p_popas :: [PAtomValue]  -- atoms in the initial population of that concept
              }
   deriving (Show) --For QuickCheck error messages only!
--NOTE :: Do NOT make instance Eq P_Population, for this is causing problems with merging. 

instance Named P_Population where
    name P_RelPopu{p_nmdr = rel} = name rel
    name P_CptPopu{p_cpt  = cpt} = name cpt

instance Traced P_Population where
 origin = pos
data P_Interface =
     P_Ifc { ifc_IsAPI :: Bool      -- ^ The interface is of type API
           , ifc_Name :: Text           -- ^ the name of the interface
           , ifc_Roles :: [Role]        -- ^ a list of roles that may use this interface
           , ifc_Obj :: P_BoxItemTermPrim       -- ^ the context expression (mostly: I[c])
           , pos :: Origin
           , ifc_Prp :: Text
           } deriving (Show) --For QuickCheck error messages only!

instance Ord P_Interface where --Required for merge of P_Contexts
 compare a b = case compare (name a) (name b) of
     EQ -> fromMaybe (fatal . T.intercalate "\n" $
                        ["P_Interface should have a non-fuzzy Origin."
                        , tshow (origin a)
                        , tshow (origin b)
                        ])
                     (maybeOrdering (origin a) (origin b))
     x -> x
instance Eq P_Interface where
  a == b = compare a b == EQ
instance Named P_Interface where
 name = ifc_Name

instance Traced P_Interface where
 origin = pos

newtype P_IClass = P_IClass { iclass_name :: Text } deriving (Eq, Ord, Show)

type P_SubInterface = P_SubIfc TermPrim
data P_SubIfc a
              = P_Box          { pos :: !Origin
                               , si_header :: !HTMLTemplateUsage
                               , si_box :: [P_BoxItem a] }
              | P_InterfaceRef { pos :: !Origin
                               , si_isLink :: !Bool --True iff LINKTO is used. (will display as hyperlink)
                               , si_str :: !Text  -- Name of the interface that is reffered to
                               } 
                deriving (Show)

-- | Key-value pairs used to supply attributes into an HTML template that is used to render a subinterface
data HTMLTemplateUsage = HTMLTemplateUsage
    { pos :: !Origin
    , btType :: !Text  
    -- ^ Type of the HTML template that is used for rendering
    , btKeys :: [TemplateKeyValue] 
    -- ^ Key-value pairs 
    } deriving (Show,Data)


instance Traced HTMLTemplateUsage where
  origin = pos

data TemplateKeyValue = TemplateKeyValue
    { pos :: !Origin
    , tkkey :: !Text
    -- ^ Name of the attribute  
    , tkval :: !(Maybe Text)
    -- ^ value of the attribute. (when no value, the attribute is handled like a switch)
    } deriving (Show,Data)
instance Named TemplateKeyValue where
  name = tkkey
instance Traced TemplateKeyValue where
  origin = pos

type P_BoxItemTermPrim = P_BoxItem TermPrim
data P_BoxItem a =
     P_BxExpr { obj_nm :: Text          -- ^ view name of the object definition. The label has no meaning in the Compliant Service Layer, but is used in the generated user interface if it is not an empty string.
           , pos :: Origin         -- ^ position of this definition in the text of the Ampersand source file (filename, line number and column number)
           , obj_ctx :: Term a         -- ^ this expression describes the instances of this object, related to their context.
           , obj_crud :: Maybe P_Cruds  -- ^ the CRUD actions as required by the user  
           , obj_mView :: Maybe ViewUsage -- ^ The view that should be used for this object
           , obj_msub :: Maybe (P_SubIfc a)  -- ^ the attributes, which are object definitions themselves.
           }
   | P_BxTxt  { obj_nm :: Text          -- ^ view name of the object definition. The label has no meaning in the Compliant Service Layer, but is used in the generated user interface if it is not an empty string.
           , pos :: Origin
           , obj_txt :: Text
           } deriving (Show)       -- just for debugging (zie ook instance Show BoxItem)
instance Ord (P_BoxItem a) where
 compare a b = fromMaybe (fatal . T.intercalate "\n" $
                        ["P_BoxItem a should have a non-fuzzy Origin."
                        , tshow (origin a)
                        , tshow (origin b)
                        ])
                     (maybeOrdering (origin a) (origin b))
instance Eq (P_BoxItem a) where
  a == b = compare a b == EQ
instance Named (P_BoxItem a) where
  name = obj_nm
instance Traced (P_BoxItem a) where
 origin = pos

data ViewUsage = ViewUsage 
    { pos :: !Origin
    , vuView :: !Text
    -- ^ Name of the VIEW that is referenced by this ViewUsage.
    , vuKeys :: [TemplateKeyValue] 
    -- ^ Key-value pairs 
    } deriving (Show,Data)


data P_Cruds = P_Cruds Origin Text deriving Show
type P_IdentDef = P_IdentDf TermPrim -- this is what is returned by the parser, but we need to change the "TermPrim" for disambiguation
data P_IdentDf a = -- so this is the parametric data-structure
         P_Id { pos :: Origin         -- ^ position of this definition in the text of the Ampersand source file (filename, line number and column number).
              , ix_lbl :: Text         -- ^ the name (or label) of this Identity. The label has no meaning in the Compliant Service Layer, but is used in the generated user interface. It is not an empty string.
              , ix_cpt :: P_Concept      -- ^ this expression describes the instances of this object, related to their context
              , ix_ats :: NE.NonEmpty (P_IdentSegmnt a) -- ^ the constituent segments of this identity. TODO: refactor to a list of terms
              } deriving (Show)
instance Named (P_IdentDf a) where
 name = ix_lbl
instance Ord (P_IdentDf a) where
 compare a b = fromMaybe (fatal . T.intercalate "\n" $
                        ["P_IdentDf a should have a non-fuzzy Origin."
                        , tshow (origin a)
                        , tshow (origin b)
                        ])
                     (maybeOrdering (origin a) (origin b))
instance Eq (P_IdentDf a) where 
  a == b = compare a b == EQ
instance Traced (P_IdentDf a) where
 origin = pos
instance Functor P_IdentDf where fmap = fmapDefault
instance Foldable P_IdentDf where foldMap = foldMapDefault
instance Traversable P_IdentDf where
  traverse f (P_Id a b c lst) = P_Id a b c <$> traverse (traverse f) lst
instance Functor P_IdentSegmnt where fmap = fmapDefault
instance Foldable P_IdentSegmnt where foldMap = foldMapDefault
instance Traversable P_IdentSegmnt where
  traverse f (P_IdentExp x) = P_IdentExp <$> traverse f x

type P_IdentSegment = P_IdentSegmnt TermPrim
newtype P_IdentSegmnt a
              = P_IdentExp  { ks_obj :: P_BoxItem a}
                deriving (Eq, Ord, Show)

type P_ViewDef = P_ViewD TermPrim
data P_ViewD a =
         P_Vd { pos :: Origin            -- ^ position of this definition in the text of the Ampersand source file (filename, line number and column number).
              , vd_lbl :: Text            -- ^ the name (or label) of this View. The label has no meaning in the Compliant Service Layer, but is used in the generated user interface. It is not an empty string.
              , vd_cpt :: P_Concept         -- ^ the concept for which this view is applicable
              , vd_isDefault :: Bool        -- ^ whether or not this is the default view for the concept
              , vd_html :: Maybe HtmlTemplateSpec -- ^ the html template for this view (not required since we may have other kinds of views as well in the future)
--              , vd_text :: Maybe P_ViewText -- Future extension
              , vd_ats :: [P_ViewSegment a] -- ^ the constituent segments of this view.
              } deriving (Show)
instance Ord (P_ViewD a) where
 compare a b = case compare (name a) (name b) of
     EQ -> fromMaybe (fatal . T.intercalate "\n" $
                        ["P_ViewD a should have a non-fuzzy Origin."
                        , tshow (origin a)
                        , tshow (origin b)
                        ])
                     (maybeOrdering (origin a) (origin b))
     x -> x
instance Eq (P_ViewD a) where --Required for merge of P_Contexts
  a == b = compare a b == EQ
instance Traced (P_ViewD a) where
 origin = pos
instance Named (P_ViewD a) where
 name = vd_lbl
instance Functor P_ViewD where fmap = fmapDefault
instance Foldable P_ViewD where foldMap = foldMapDefault
instance Traversable P_ViewD where
 traverse fn (P_Vd a b c d e f) = P_Vd a b c d e <$> traverse (traverse fn) f

data P_ViewSegment a = 
     P_ViewSegment { vsm_labl :: Maybe Text
                   , pos :: Origin
                   , vsm_load :: P_ViewSegmtPayLoad a
                   } deriving Show
instance Traced (P_ViewSegment a) where
  origin = pos
instance Functor P_ViewSegment where fmap = fmapDefault
instance Foldable P_ViewSegment where foldMap = foldMapDefault
instance Traversable P_ViewSegment where
 traverse fn (P_ViewSegment a b c) = P_ViewSegment a b <$> traverse fn c
data P_ViewSegmtPayLoad a  
                    = P_ViewExp  { vs_expr :: Term a }
                    | P_ViewText { vs_txt :: Text }
                      deriving (Show)

data HtmlTemplateSpec = HtmlTemplateSpec
   { pos :: !Origin
   , vhtFile :: !FilePath
   , vhtKeyVals :: [TemplateKeyValue]
   } deriving (Show,Data)


instance Functor P_ViewSegmtPayLoad where fmap = fmapDefault
instance Foldable P_ViewSegmtPayLoad where foldMap = foldMapDefault
instance Traversable P_ViewSegmtPayLoad where
 traverse f (P_ViewExp a) = P_ViewExp <$> traverse f a
 traverse _ (P_ViewText a) = pure (P_ViewText a)


-- PPurpose is a parse-time constructor. It contains the name of the object it explains.
-- It is a pre-explanation in the sense that it contains a reference to something that is not yet built by the compiler.
--                       Constructor      name          RefID  Explanation
data PRef2Obj = PRef2ConceptDef Text
              | PRef2Relation P_NamedRel
              | PRef2Rule Text
              | PRef2IdentityDef Text
              | PRef2ViewDef Text
              | PRef2Pattern Text
              | PRef2Interface Text
              | PRef2Context Text
              deriving Show -- only for fatal error messages

instance Named PRef2Obj where
  name pe = case pe of
     PRef2ConceptDef str -> str
     PRef2Relation (PNamedRel _ nm mSgn) -> nm<>maybe "" tshow mSgn
     PRef2Rule str -> str
     PRef2IdentityDef str -> str
     PRef2ViewDef str -> str
     PRef2Pattern str -> str
     PRef2Interface str -> str
     PRef2Context str -> str

data PPurpose = PRef2 { pos :: Origin      -- the position in the Ampersand script of this purpose definition
                      , pexObj :: PRef2Obj    -- the reference to the object whose purpose is explained
                      , pexMarkup:: P_Markup  -- the piece of text, including markup and language info
                      , pexRefIDs :: [Text] -- the references (for traceability)
                      } deriving Show
instance Ord PPurpose where --Required for merge of P_Contexts
 compare a b = case compare (name a) (name b) of
     EQ -> fromMaybe (fatal . T.intercalate "\n" $
                        ["PPurpose a should have a non-fuzzy Origin."
                        , tshow (origin a)
                        , tshow (origin b)
                        ])
                     (maybeOrdering (origin a) (origin b))
     x -> x
instance Eq PPurpose where --Required for merge of P_Contexts
  a == b = compare a b == EQ

instance Named PPurpose where
 name pe = name (pexObj pe)

instance Traced PPurpose where
 origin = pos

data P_Concept
   = PCpt{ p_cptnm :: Text }  -- ^The name of this Concept
   | P_ONE  -- ^The universal Singleton: 'I'['Anything'] = 'V'['Anything'*'Anything']
      deriving (Eq,Ord)
-- (Stef June 17th, 2016)   P_Concept is defined Eq, because P_Relation must be Eq on name and signature.
-- (Sebastiaan 16 jul 2016) P_Concept has been defined Ord, only because we want to maintain sets of concepts in the type checker for quicker lookups.
mkPConcept :: Text -> P_Concept
mkPConcept "ONE" = P_ONE
mkPConcept nm = PCpt {p_cptnm = nm}
instance Named P_Concept where
 name PCpt {p_cptnm = nm} = nm
 name P_ONE = "ONE"

instance Show P_Concept where
 show = T.unpack . name

data P_Sign = P_Sign {pSrc :: P_Concept, pTgt :: P_Concept } deriving (Ord,Eq)
-- (Stef June 17th, 2016)   P_Sign is defined Ord,Eq, because P_Relation must be Ord,Eq on name and signature.

instance Show P_Sign where
  show sgn = "[" <> show (pSrc sgn)<>"*"<>show (pTgt sgn) <> "]"
instance Flippable P_Sign where
  flp sgn = P_Sign { pSrc = pTgt sgn
                   , pTgt = pSrc sgn
                   }

data PClassify =  PClassify
  { pos      :: Origin
  , specific :: P_Concept                    -- ^ Left hand side concept expression
  , generics :: NE.NonEmpty P_Concept       -- ^ Right hand side concept expression
  } deriving (Show)

-- (Stef April 29th, 2020) Eq PClassify is used to generate P-contexts without duplicate PClassify's in it.
instance Eq PClassify where
 p == q = specific p==specific q && generics p==generics q

instance Traced PClassify where
 origin = pos

type Props = Set.Set Prop

data Prop      = Uni          -- ^ univalent
               | Inj          -- ^ injective
               | Sur          -- ^ surjective
               | Tot          -- ^ total
               | Sym          -- ^ symmetric
               | Asy          -- ^ antisymmetric
               | Trn          -- ^ transitive
               | Rfx          -- ^ reflexive
               | Irf          -- ^ irreflexive
               | Prop         -- ^ PROP keyword, later replaced by [Sym, Asy]
                 deriving (Eq, Ord, Enum, Bounded,Typeable, Data)

instance Show Prop where
 show Uni = "UNI"
 show Inj = "INJ"
 show Sur = "SUR"
 show Tot = "TOT"
 show Sym = "SYM"
 show Asy = "ASY"
 show Trn = "TRN"
 show Rfx = "RFX"
 show Irf = "IRF"
 show Prop = "PROP"

instance Unique Prop where
 showUnique = tshow

instance Flippable Prop where
 flp Uni = Inj
 flp Tot = Sur
 flp Sur = Tot
 flp Inj = Uni
 flp x = x

mergeContexts :: P_Context -> P_Context -> P_Context
mergeContexts ctx1 ctx2 =
  PCtx{ ctx_nm     = case (filter (not.T.null) . map ctx_nm) contexts of
                        []    -> ""
                        (x:_) -> x
      , ctx_pos    = fromContextsKeepDoubles ctx_pos
      , ctx_lang   = ctx_lang ctx1 -- By taking the first, we end up with the language of the top-level context
      , ctx_markup = foldl' orElse Nothing $ map ctx_markup contexts
      , ctx_pats   = fromContextsKeepDoubles ctx_pats
      , ctx_rs     = fromContextsRemoveDoubles ctx_rs
      , ctx_ds     = mergeRels (ctx_ds ctx1<>ctx_ds ctx2)
      , ctx_cs     = fromContextsKeepDoubles ctx_cs
      , ctx_ks     = fromContextsKeepDoubles ctx_ks
      , ctx_rrules = fromContextsKeepDoubles ctx_rrules
      , ctx_reprs  = fromContextsKeepDoubles ctx_reprs
      , ctx_vs     = fromContextsRemoveDoubles ctx_vs
      , ctx_gs     = fromContextsKeepDoubles ctx_gs
      , ctx_ifcs   = fromContextsRemoveDoubles ctx_ifcs
      , ctx_ps     = fromContextsKeepDoubles ctx_ps
      , ctx_pops   = mergePops (ctx_pops ctx1<>ctx_pops ctx2)
      , ctx_metas  = fromContextsKeepDoubles ctx_metas
      }
    where
      -- NOTE:
      -- In the P_Structure we want to limit nub as much as possible. 
      -- this is to ensure that no information is lost because we do
      -- not know a proper origin of some element. Sometimes the origin
      -- is used to distinquish between two elements. That is not 
      -- usefull here, and might lead to information lost.
      fromContextsKeepDoubles :: (P_Context -> [a]) -> [a]         
      fromContextsKeepDoubles fun = concatMap fun contexts          
      contexts = [ctx1,ctx2]
      fromContextsRemoveDoubles :: Ord b => (P_Context -> [b]) -> [b]
      fromContextsRemoveDoubles f = 
         Set.toList . Set.unions . map (Set.fromList . f) $ contexts
      mergePops :: [P_Population] -> [P_Population]
      mergePops = map mergePopsSameType . NE.groupBy groupCondition
         where
             groupCondition :: P_Population -> P_Population -> Bool
             groupCondition a b = 
               case (a,b) of
                 (P_RelPopu{},P_RelPopu{}) -> p_src a == p_src b 
                                           && p_tgt a == p_tgt b
                                           && sameNamedRels (p_nmdr a) (p_nmdr b)
                 (P_CptPopu{},P_CptPopu{}) -> p_cpt a == p_cpt b
                 _  -> False
               where
                 sameNamedRels :: P_NamedRel -> P_NamedRel -> Bool
                 sameNamedRels x y = p_nrnm x == p_nrnm y 
                                  && p_mbSign x == p_mbSign y
             mergePopsSameType :: NE.NonEmpty P_Population -> P_Population
             mergePopsSameType (h :| tl) = case h of
                P_RelPopu{} -> h {p_popps = Set.toList . Set.unions . map (Set.fromList . p_popps) $ (h:tl)}
                P_CptPopu{} -> h {p_popas = Set.toList . Set.unions . map (Set.fromList . p_popas) $ (h:tl)}

      -- | Left-biased choice on maybes
      orElse :: Maybe a -> Maybe a -> Maybe a
      x `orElse` y = case x of
                       Just _  -> x
                       Nothing -> y
