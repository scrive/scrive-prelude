{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Slightly customized replacement of Prelude.
module Prelude
  ( module Control.Applicative
  , module Control.Monad
  , module Data.Algebra.Boolean
  , module Data.Either
  , module Data.Foldable
  , module Data.Functor
  , module Data.List
  , module Data.Maybe
  , module Data.Monoid
  , module Data.Monoid.Utils
  , module O
  , module P
  , Generic
  , Text
  , MonadFail (..)
  , (!!)
  -- optics
  , (&)
  , (%)
  , (?~)
  , view
  , (^.)
  , preview
  , (^?)
  , toListOf
  , (^..)
  , over
  , (%~)
  , set
  , (.~)
  , copy
  -- prelude
  , expectJust
  , for
  , fromJust
  , head
  , identity
  , last
  , maximum
  , maybeRead
  , minimum
  , read
  , readEither
  , showt
  , showtp
  , tail
  , throwLeft
  , toMaybe
  , unexpectedError
  , whenJust
  , whenNothing
  )
where

import Control.Applicative
import Control.Exception (Exception)
import Control.Monad hiding (fail)
import Control.Monad.Catch (MonadThrow (..))
import Control.Monad.Extra hiding (fail)
import Control.Monad.Fail (MonadFail (..))
import Data.Algebra.Boolean
import Data.Either
import qualified Data.Either.Optics as O
import Data.Foldable (asum, foldMap, traverse_)
import Data.Functor hiding (unzip)
import Data.List hiding
  ( all
  , and
  , any
  , head
  , last
  , maximum
  , minimum
  , or
  , tail
  , (!!)
  )
import Data.Maybe hiding (fromJust)
import qualified Data.Maybe.Optics as O
import Data.Monoid
import Data.Monoid.Utils
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Tuple.Optics as O
import GHC.Generics (Generic)
import qualified GHC.Read as Read
import GHC.Stack (HasCallStack, withFrozenCallStack)
import Optics
import qualified Optics as O
  ( A_Fold
  , A_Getter
  , A_Lens
  , A_Prism
  , A_ReversedLens
  , A_ReversedPrism
  , A_Review
  , A_Setter
  , A_Traversal
  , AffineFold
  , AffineTraversal
  , AffineTraversal'
  , An_AffineFold
  , An_AffineTraversal
  , An_Iso
  , Fold
  , Getter
  , Iso
  , Iso'
  , IxAffineFold
  , IxAffineTraversal
  , IxAffineTraversal'
  , IxFold
  , IxGetter
  , IxLens
  , IxLens'
  , IxSetter
  , IxSetter'
  , IxTraversal
  , IxTraversal'
  , Lens
  , Lens'
  , NoIx
  , Optic
  , Optic'
  , Prism
  , Prism'
  , ReversedLens
  , ReversedLens'
  , ReversedPrism
  , ReversedPrism'
  , Review
  , Setter
  , Setter'
  , Traversal
  , Traversal'
  , WithIx
  )
import Text.JSON.FromJSValue
import Text.JSON.ToJSValue
import qualified Text.ParserCombinators.ReadP as Read
import qualified Text.ParserCombinators.ReadPrec as Read
import Text.Pretty.Simple
  ( OutputOptions (..)
  , defaultOutputOptionsDarkBg
  , pShowOpt
  )
import "base" Prelude hiding
  ( all
  , and
  , any
  , error
  , fail
  , head
  , id
  , last
  , maximum
  , minimum
  , not
  , or
  , read
  , tail
  , (!!)
  , (&&)
  , (||)
  )
import qualified "base" Prelude as P hiding (fail)

#if !MIN_VERSION_cond(0,5,0)
-- | Boolean algebra of functions.
instance Boolean b => Boolean (a -> b) where
  true = const true
  false = const false
  not f = not . f
  (&&) = liftA2 (&&)
  (||) = liftA2 (||)
  xor = liftA2 xor
  (-->) = liftA2 (-->)
  (<-->) = liftA2 (<-->)
#endif

instance FromJSValue Text where
  fromJSValue = fmap T.pack . fromJSValue

instance ToJSValue Text where
  toJSValue = toJSValue . T.unpack

----------------------------------------
-- Additional optics utilities.

{-# ANN copy ("HLint: ignore Eta reduce" :: String) #-}

-- | Copy the field value from an object of the same type.
copy :: (Is k A_Setter, Is k A_Getter) => Optic k is s s a a -> s -> s -> s
copy x fromThis toThat = set x (view x fromThis) toThat

----------------------------------------

-- | Just @flip fmap@.
for :: Functor f => f a -> (a -> b) -> f b
for = flip fmap

-- | Read a value and return 'Nothing' if an error occurs during parsing.
maybeRead :: Read a => Text -> Maybe a
maybeRead s =
  case [x | (x, "") <- Read.readPrec_to_S read' Read.minPrec $ T.unpack s] of
    [x] -> Just x
    _ -> Nothing
  where
    read' = do
      x <- Read.readPrec
      Read.lift Read.skipSpaces
      pure x

-- | Returns Just if the precondition is true.
toMaybe :: Bool -> a -> Maybe a
toMaybe True x = Just x
toMaybe False _ = Nothing

----------------------------------------

-- | Replacement for 'id' that doesn't collide with "identifier".
identity :: a -> a
identity = P.id

-- | Replacement for 'P.!!' that provides useful information on failure.
(!!) :: HasCallStack => [a] -> Int -> a
xs !! n
  | n < 0 = negativeIndexError "!!"
  | otherwise =
      foldr
        ( \x r k -> case k of
            0 -> x
            _ -> r (k - 1)
        )
        (indexOutOfBoundsError "!!")
        xs
        n

-- | Replacement for 'P.head' that provides useful information on failure.
head :: HasCallStack => [a] -> a
head = emptyList P.head $ emptyListError "head"

-- | Replacement for 'P.tail' that provides useful information on failure.
tail :: HasCallStack => [a] -> [a]
tail = emptyList P.tail $ emptyListError "tail"

-- | Replacement for 'P.last' that provides useful information on failure.
last :: HasCallStack => [a] -> a
last = emptyList P.last $ emptyListError "last"

-- | Replacement for 'P.maximum' that provides useful information on failure.
maximum :: (HasCallStack, Ord a) => [a] -> a
maximum = emptyList P.maximum $ emptyListError "maximum"

-- | Replacement for 'P.minimum' that provides useful information on failure.
minimum :: (HasCallStack, Ord a) => [a] -> a
minimum = emptyList P.minimum $ emptyListError "minimum"

-- | Replacement for 'P.read' that provides useful information on failure.
read :: (HasCallStack, Read a, Show a) => Text -> a
read = either unexpectedError identity . readEither

readEither :: (Read a, Show a) => Text -> Either Text a
readEither s =
  case [x | (x, "") <- Read.readPrec_to_S read' Read.minPrec $ T.unpack s] of
    [x] -> Right x
    xs ->
      Left $
        "reading failed (input was '"
          <> s
          <> "', reads returned '"
          <> showt xs
          <> "')"
  where
    read' = do
      x <- Read.readPrec
      Read.lift Read.skipSpaces
      pure x

-- | General version of 'fromJust' with a custom error message
expectJust :: HasCallStack => Text -> Maybe a -> a
expectJust msg = \case
  Nothing -> unexpectedError msg
  Just a -> a

-- | Replacement for 'Data.Maybe.fromJust' that provides useful
-- information on failure.
fromJust :: HasCallStack => Maybe a -> a
fromJust = expectJust "fromJust received Nothing"

-- | whenNothing mA fa = maybe fa return mA
whenNothing :: Applicative f => Maybe a -> f a -> f a
whenNothing (Just x) _ = pure x
whenNothing Nothing m = m

-- | Like 'error', but with a more conspicous name.
unexpectedError :: HasCallStack => Text -> a
unexpectedError errMsg = withFrozenCallStack (P.error $ T.unpack errMsg)

throwLeft :: (MonadThrow m, Exception e) => Either e a -> m a
throwLeft = either throwM pure

---- internal stuff below ----

emptyList :: ([a] -> t) -> t -> [a] -> t
emptyList f err v = if null v then err else f v

emptyListError :: HasCallStack => Text -> a
emptyListError fname = unexpectedError $ fname <> " received an empty list"

indexOutOfBoundsError :: HasCallStack => Text -> a
indexOutOfBoundsError fname =
  unexpectedError $ fname <> " received an out-of-bounds index"

negativeIndexError :: HasCallStack => Text -> a
negativeIndexError fname = unexpectedError $ fname <> " received a negative index"

showt :: Show a => a -> Text
showt = T.pack . show

-- | Pretty print anything with a `Show` instance.
showtp :: Show a => a -> T.Text
showtp =
  TL.toStrict
    . pShowOpt
      ( defaultOutputOptionsDarkBg
          { outputOptionsCompact = True
          , outputOptionsPageWidth = 180
          }
      )
