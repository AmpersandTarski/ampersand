{-# LANGUAGE DeriveDataTypeable #-}
{- The purpose of class Unique is to identify a Haskell object by means of a string.
E.g.
instance Unique Pattern where
 showUnique = optionalQuote . name
-}

module Ampersand.Basics.Unique 
  (Unique(..),Named(..))
where
import           Ampersand.Basics.Prelude
import           Data.Char
import           Data.List
import qualified Data.Set as Set
import           Data.Typeable

-- | anything could have some label, can't it?
class Named a where
  name :: a->String

-- | In the context of the haskell code, things can be Unique. 
class (Typeable e, Eq e) => Unique e where 
  -- | a representation of a unique thing
  self :: e -> UniqueObj e
  self a = UniqueObj { theThing = a
                 --    , theShow  = showUnique
                     }
  -- | representation of a Unique thing into a string.  
  uniqueShowWithType :: 
              e    ->  --  the thing to show
              String
  uniqueShowWithType x = show (typeOf x) ++"_" ++ showUnique x

  -- | A function to show a unique instance. It is the responsability
  --   of the instance definition to make sure that for every a, b of 
  --   an individual type:
  --        a == b  <==> showUnique a == showUnique b
  showUnique :: e -> String
  {-# MINIMAL showUnique #-}
  

-- | this is the implementation of the abstract data type. It mustn't be exported
data UniqueObj a = 
       UniqueObj { theThing :: a
                 } deriving (Typeable)

instance Unique a => Unique [a] where
   showUnique [] = "[]"
   showUnique xs = "["++intercalate ", " (map showUnique xs)++"]"
instance Unique a => Unique (Set.Set a) where
   showUnique = showUnique . Set.elems

instance Unique Bool where
 showUnique = map toLower . show 
