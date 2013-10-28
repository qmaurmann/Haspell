module PrefixTree
( PrefixTree
, fromList
, toList
, size
, empty
, member
, insert
, wordEnd
, children
) where

import qualified Data.Map as Map


data PrefixTree a = PrefixTree { children :: Map.Map a (PrefixTree a)
                               , wordEnd :: Bool
                               } deriving (Eq,Ord,Show)


-- Perform a lookup
member :: (Ord a) => [a] -> PrefixTree a -> Bool
member [] pt = wordEnd pt
member (c:cs) pt = case Map.lookup c (children pt) of
    Nothing -> False
    Just pt' -> member cs pt'

-- Convenient PrefixTree constructor
fromList :: (Ord a) => [[a]] -> PrefixTree a
fromList = foldl (flip insert) empty

toList :: (Ord a) => PrefixTree a -> [[a]]
toList pt = d ++ [c:suffix | (c,pt') <- Map.toList (children pt),
                             suffix <- toList pt']
    where d = if wordEnd pt then [[]] else []

empty :: PrefixTree a
empty = PrefixTree Map.empty False

insert :: (Ord a) => [a] -> PrefixTree a -> PrefixTree a
insert [] pt = pt {wordEnd = True}
insert (c:cs) pt@(PrefixTree children wordEnd) = case Map.lookup c children of
    Nothing -> pt {children = Map.insert c (insert cs empty) children}
    Just pt' -> pt {children = Map.insert c (insert cs pt') children}

-- Size as a PrefixTree is the number of words in it
size :: PrefixTree a -> Int
size pt = d + sum [size pt' | (c,pt') <- Map.toList (children pt)] where
    d = if wordEnd pt then 1 else 0


























