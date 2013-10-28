module Bag
( Bag
, fromList
, toList
, size
, insert
, remove
, empty
) where

import qualified Data.Map as Map

type Bag a = Map.Map a Int  -- Maintain invariant: only values >= 1

fromList :: (Ord a) => [a] -> Bag a
fromList list = Map.fromListWith (+) [(a,1) | a <- list]

toList :: (Ord a) => Bag a -> [a]
toList bag = concat [replicate c a | (a,c) <- Map.toList bag]

size :: Bag a -> Int
size bag = sum [c | (_,c) <- Map.toList bag]

insert :: (Ord a) => a -> Bag a -> Bag a
insert a bag = Map.insertWith (+) a 1 bag


-- I think I prefer the "unsafe" error raising here over the error silencing in
-- analogous Map.delete and Set.delete.
remove :: (Ord a) => a -> Bag a -> Bag a
remove a bag = case Map.lookup a bag of
    Nothing -> error "Can't remove character not in bag"
    Just x -> if x == 1 then Map.delete a bag
              else Map.insert a (x-1) bag

empty :: Bag a
empty = Map.empty
