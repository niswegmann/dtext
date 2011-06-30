{-|
License     :  CC0 1.0 Universal Public Domain Dedication
Maintainer  :  niswegmann@gmail.com
Stability   :  provisional
Portability :  portable (Haskell 2010)

A difference text represents texts as a function /f/, which when given a text
/t/, returns the text that /f/ represents, prepended to /x/.

Difference texts supports /O(1)/ append, making them useful for append-heavy uses,
e.g. as logging and pretty-printing.
-}

module Data.DText
  ( DText (..)
  -- * Creation and elimination
  , fromText
  , toText
  , empty
  -- * Basic interface
  , cons
  , snoc
  , head
  , tail
--  , init
--  , last
  , null
  , length
  , uncons
  -- * Concatenation
  , append
  , concat  
  -- * Transformations  
  , map
  , reverse
  -- * Catamorphisms
  , foldr
  -- * Anamorphisms
  , replicate
  , unfoldr
  ) where

import Data.Function (on)
import qualified Data.List as List
import Data.Monoid (Monoid (..))
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Prelude hiding
  (concat, foldr, head, length, map, null, replicate, reverse, tail)

--------------------------------------------------------------------------------

newtype DText = DT { unDT :: Text -> Text }

--------------------------------------------------------------------------------

instance Eq DText where
  (==) = (==) `on` toText

instance Ord DText where
  compare = compare `on` toText

instance Show DText where
  show = show . toText

instance IsString DText where
  fromString = fromText . Text.pack

instance Monoid DText where
  mempty  = empty
  mappend = append

--------------------------------------------------------------------------------

-- | /O(1)/, Converts a text to a difference text.
{-# INLINE fromText #-}
fromText :: Text -> DText
fromText = DT . Text.append

-- | /O(n)/, Converts a difference text to a text.
{-# INLINE toText #-}
toText :: DText -> Text
toText = ($Text.empty) . unDT

-- | /O(1)/, Creates an empty difference text.
{-# INLINE empty #-}
empty :: DText
empty = DT id

--------------------------------------------------------------------------------

-- | /O(1)/, Prepends a single character to a difference text.
{-# INLINE cons #-}
infixr `cons`
cons :: Char -> DText -> DText
cons x xs = DT ((Text.cons x) . unDT xs)

-- | /O(1)/, Appends a single character at a difference text.
{-# INLINE snoc #-}
infixl `snoc`
snoc :: DText -> Char -> DText
snoc xs x = DT (unDT xs . (Text.cons x))

-- | Returns the head of the difference text.
{-# INLINE head #-}
head :: DText -> Char
head = elim (error "Data.DText.head: empty elim") const

-- | Returns the tail of the difference text.
{-# INLINE tail #-}
tail :: DText -> DText
tail = elim (error "Data.DText.tail: empty elim") (flip const)

-- | /O(1)/, Tests whether a difference text is empty or not.
{-# INLINE null #-}
null :: DText -> Bool
null = Text.null . toText

-- | /O(n)/, Returns the length of a difference text.
{-# INLINE length #-}
length :: DText -> Int
length = Text.length . toText

-- | Returns the head and tail of a difference text, or Nothing if empty.
uncons :: DText -> Maybe (Char, DText)
uncons dt =
  case Text.uncons (toText dt) of
    Nothing       -> Nothing
    Just (ch, ts) -> Just (ch, fromText ts)

--------------------------------------------------------------------------------

-- | /O(1)/, Appends two difference texts.
{-# INLINE append #-}
append :: DText -> DText -> DText
append xs ys = DT (unDT xs . unDT ys)

-- | /O(spine)/, Concatenates a list of difference texts.
{-# INLINE concat #-}
concat :: [DText] -> DText
concat = List.foldr append empty

--------------------------------------------------------------------------------

-- | /O(n)/, Mapping on difference texts.
{-# INLINE map #-}
map :: (Char -> Char) -> DText -> DText
map f = foldr (cons . f) empty

-- | /O(n)/, Reverses a difference text.
{-# INLINE reverse #-}
reverse :: DText -> DText
reverse = fromText . Text.reverse . toText

--------------------------------------------------------------------------------

-- | /O(n)/, Right-folding on difference texts.
{-# INLINE foldr #-}
foldr :: (Char -> a -> a) -> a -> DText -> a
foldr f b = Text.foldr f b . toText

--------------------------------------------------------------------------------

-- | /O(n)/, /Replicate n ch/ returns a difference text consisting of the input
-- character /ch/ repeated /n/ times.
{-# INLINE replicate #-}
replicate :: Int -> Char -> DText
replicate n ch = DT $ \ xs ->
  let
    go m | m <= 0    = xs
         | otherwise = ch `Text.cons` go (m-1)
  in
    go n

-- | Right-unfolding on difference texts.
unfoldr :: (a -> Maybe (Char, a)) -> a -> DText
unfoldr pf x =
  case pf x of
    Nothing       -> empty
    Just (ch, x') -> cons ch (unfoldr pf x')

--------------------------------------------------------------------------------

-- /O(n)/, Text elimination, head, tail.
elim :: a -> (Char -> DText -> a) -> DText -> a
elim nill consit dt =
  case Text.uncons (toText dt) of
    Nothing       -> nill
    Just (ch, tx) -> consit ch (fromText tx)
