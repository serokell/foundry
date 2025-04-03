{- |

Non-negative numbers:

@
ghci> import Source.Layout.NonNegative
ghci> 2 + 3 :: NonNegative Double
5.0
ghci> 2 - 3 :: NonNegative Double
*** Exception: arithmetic underflow
@

-}

module Source.Layout.NonNegative
  ( NonNegative(),
    getNonNegative,
    toNonNegative,
    unsafeToNonNegative
  ) where

import Control.Exception
import Data.Coerce (coerce)
import Data.Maybe (mapMaybe)
import Foreign.Storable (Storable)
import Text.Printf (PrintfArg)
import Source.Layout.Inj

-- | An opaque newtype around a number @n@ that asserts that @n >= 0@.
-- The constructor is not exported to maintain the invariant.
newtype NonNegative a = NonNegative a
  deriving newtype (Eq, Ord, Show, Real, Integral, RealFrac, Semigroup, Monoid, Storable, PrintfArg)

-- | Unwrap the newtype.
getNonNegative :: NonNegative a -> a
getNonNegative (NonNegative a) = a

-- | Throws 'Underflow'.
instance (Inj p a, Ord a, Num a) => Inj p (NonNegative a) where
  inj = unsafeToNonNegative . inj

-- | Check if a number is non-negative and return 'Nothing' if it is negative.
toNonNegative :: (Ord a, Num a) => a -> Maybe (NonNegative a)
toNonNegative d =
  if d >= 0 then Just (NonNegative d) else Nothing

-- | Check if a number is non-negative and throw 'Underflow' if it is negative.
unsafeToNonNegative :: (Ord a, Num a) => a -> NonNegative a
unsafeToNonNegative d =
  if d >= 0 then NonNegative d else throw Underflow

-- | Throws 'Underflow'.
instance (Ord a, Num a) => Num (NonNegative a) where
  (+) = coerce ((+) @a)
  NonNegative a - NonNegative b = unsafeToNonNegative (a - b)
  (*) = coerce ((*) @a)
  negate _ = throw Underflow
  abs = id
  signum = coerce (signum @a)
  fromInteger = unsafeToNonNegative . fromInteger

-- | Throws 'Underflow'.
instance (Ord a, Fractional a) => Fractional (NonNegative a) where
  (/) = coerce ((/) @a)
  recip = coerce (recip @a)
  fromRational = unsafeToNonNegative . fromRational

-- | Throws 'Underflow'.
instance (Ord a, Num a, Enum a) => Enum (NonNegative a) where
  succ = coerce (succ @a)
  pred (NonNegative a) = unsafeToNonNegative (pred a)
  toEnum = unsafeToNonNegative . toEnum
  fromEnum = coerce (fromEnum @a)
  enumFrom = coerce (enumFrom @a)
  enumFromThen (NonNegative n) (NonNegative n')
    | n' < n = coerce (takeWhile (>=0) (enumFromThen n n'))
    | otherwise = coerce (enumFromThen n n')
  enumFromTo = coerce (enumFromTo @a)
  enumFromThenTo = coerce (enumFromThenTo @a)

-- | Throws 'Underflow'.
instance (Ord a, Num a, Floating a) => Floating (NonNegative a) where
  pi = coerce (pi @a)
  exp = coerce (exp @a)
  log (NonNegative a) = unsafeToNonNegative (log a)
  sqrt = coerce (sqrt @a)
  (**) = coerce ((**) @a)
  logBase (NonNegative b) (NonNegative a) = unsafeToNonNegative (logBase b a)
  sin (NonNegative a) = unsafeToNonNegative (sin a)
  cos (NonNegative a) = unsafeToNonNegative (cos a)
  tan (NonNegative a) = unsafeToNonNegative (tan a)
  asin (NonNegative a) = unsafeToNonNegative (asin a)
  acos (NonNegative a) = unsafeToNonNegative (acos a)
  atan (NonNegative a) = unsafeToNonNegative (atan a)
  sinh (NonNegative a) = unsafeToNonNegative (sinh a)
  cosh (NonNegative a) = unsafeToNonNegative (cosh a)
  tanh (NonNegative a) = unsafeToNonNegative (tanh a)
  asinh (NonNegative a) = unsafeToNonNegative (asinh a)
  acosh (NonNegative a) = unsafeToNonNegative (acosh a)
  atanh (NonNegative a) = unsafeToNonNegative (atanh a)

-- | Throws 'Underflow'.
instance (Ord a, Num a, RealFloat a) => RealFloat (NonNegative a) where
  floatRadix = coerce (floatRadix @a)
  floatDigits = coerce (floatDigits @a)
  floatRange = coerce (floatRange @a)
  decodeFloat = coerce (decodeFloat @a)
  encodeFloat s e = unsafeToNonNegative (encodeFloat s e)
  exponent = coerce (exponent @a)
  significand = coerce (significand @a)
  scaleFloat = coerce (scaleFloat @a)
  isNaN = coerce (isNaN @a)
  isInfinite = coerce (isInfinite @a)
  isDenormalized = coerce (isDenormalized @a)
  isNegativeZero = coerce (isNegativeZero @a)
  isIEEE = coerce (isIEEE @a)
  atan2 (NonNegative y) (NonNegative x) = unsafeToNonNegative (atan2 y x)

instance (Ord a, Num a, Read a) => Read (NonNegative a) where
  readsPrec n s = mapMaybe (_1 toNonNegative) (readsPrec n s)
  readList s = mapMaybe (_1 (traverse toNonNegative)) (readList s)

_1 :: Functor f => (a -> f a') -> (a, b) -> f (a', b)
_1 f (a, b) = (\a' -> (a', b)) <$> f a
