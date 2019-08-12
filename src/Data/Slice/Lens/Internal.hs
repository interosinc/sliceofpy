{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

module Data.Slice.Lens.Internal where

import Control.Lens    ( (%%~)
                       , (&)
                       , (<.)
                       , IndexedTraversal'
                       , Reversing
                       , Traversal'
                       , _2
                       , indexed
                       , indices
                       , partsOf
                       , reversed
                       , traversed
                       , withIndex
                       )
import Data.Char       ( isSpace )
import Data.List       ( dropWhileEnd )
import Data.List.Split ( splitOn )
import Data.Maybe      ( fromMaybe )

class Slice s where
  start :: s -> Maybe Int
  end   :: s -> Maybe Int
  step  :: s -> Maybe Int

instance Slice (Int, Int, Int) where
  start (x, _, _) = Just x
  end   (_, x, _) = Just x
  step  (_, _, x) = Just x

instance Slice (Maybe Int, Maybe Int, Maybe Int) where
  start (x, _, _) = x
  end   (_, x, _) = x
  step  (_, _, x) = x

instance Slice String where
  start s = start (tupleSliceFromString s)
  end   s = end   (tupleSliceFromString s)
  step  s = step  (tupleSliceFromString s)

isliced :: (Slice s, Traversable t) => s -> IndexedTraversal' Int (t a) a
isliced s = isliced' (start s) (end s) (step s)

isliced' :: Traversable t
         => Maybe Int -> Maybe Int -> Maybe Int -> IndexedTraversal' Int (t a) a
isliced' start' end' step' =
  (partsOf (traversed . withIndex) . sliced' start' end' step' . indexBy fst) <. _2
  where
    indexBy :: (a -> i) -> IndexedTraversal' i a a
    indexBy f p a = indexed p (f a) a

sliced :: (Slice s, Traversable t) => s -> Traversal' (t a) a
sliced s = sliced' (start s) (end s) (step s)

sliced' :: Traversable t => Maybe Int -> Maybe Int -> Maybe Int -> Traversal' (t a) a
sliced' start' end' step' = partsOf traversed . slice' start' end' step'

tupleSliceFromString :: String -> (Maybe Int, Maybe Int, Maybe Int)
tupleSliceFromString s = case splitOn ":" s of
  [start', stop']        -> (read' start', read' stop', Just 1)
  [start', stop', step'] -> (read' start', read' stop', read' step')
  other                  -> error $ "invalid string slice: " ++ show other
  where
    read' s'
      | trim s' == "" = Nothing
      | otherwise     = Just $ read s'

    trim = dropWhileEnd isSpace . dropWhile isSpace

slice :: (Slice s, Traversable t, Applicative f, Reversing (t a))
      => s
      -> (a -> f a)
      -> t a
      -> f (t a)
slice s = slice' (start s) (end s) (step s)

slice' :: (Traversable t, Applicative f, Reversing (t a))
       => Maybe Int -> Maybe Int -> Maybe Int
       -> (a -> f a)
       -> t a
       -> f (t a)
slice' startMay endMay stepMay f xs = xs
  & partsOf (traversed . indices pickElements)
  . maybeReversed
  . traversed
  . indices stepper
  %%~ f
  where
    start_ = fromMaybe defStart startMay
    end_   = fromMaybe defEnd   endMay
    step_  = fromMaybe defStep  stepMay

    (defStart, defEnd)
      | step_ < 0 = (size, 0)
      | otherwise = (0, size)
    defStep = 1

    start' | start_ >= 0 = start_
           | otherwise   = size + start_
    end' | end_ >= 0 = end_
         | otherwise = size + end_

    maybeReversed
      | step_ < 0 = reversed
      | otherwise = id

    pickElements i
      | step_ >= 0 = i >= start' && i < end'
      | otherwise  = i >= end'   && i < start'

    stepper i = i `mod` abs step_ == 0

    size = length xs
