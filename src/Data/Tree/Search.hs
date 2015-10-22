{-# LANGUAGE LambdaCase #-}

module Data.Tree.Search where

import Control.Applicative (Alternative(..))
import Control.Monad       (MonadPlus(..),liftM,ap,msum)
import Data.Monoid         ((<>))
import Data.Function       (on)

data STree a
  = Fail
  | Val !a
  | Node (() -> STree a) (() -> STree a)

-- instances {{{

instance Eq a => Eq (STree a) where
  (==) = \case
    Fail  -> \case
      Fail -> True
      _    -> False
    Val a -> \case
      Val b -> a == b
      _     -> False
    Node l1 r1 -> \case
      Node l2 r2 -> eqBy unit l1 l2 && eqBy unit r1 r2
      _          -> False

instance Ord a => Ord (STree a) where
  compare = \case
    Fail  -> \case
      Fail -> EQ
      _    -> LT
    Val a -> \case
      Val b  -> compare a b
      Fail   -> GT
      Node{} -> LT
    Node l1 r1 -> \case
      Node l2 r2 -> compareBy unit l1 l2 <> compareBy unit r1 r2
      _          -> GT

instance Show a => Show (STree a) where
  showsPrec d = \case
    Fail  -> showString "Fail"
    Val a -> showParen (d > 10)
      $ showString "Val "
      . showsPrec 11 a
    Node l r -> showParen (d > 10)
      $ showString "Node "
      . showsPrec 11 (unit l)
      . showChar ' '
      . showsPrec 11 (unit r)

instance Functor STree where
  fmap = liftM

instance Applicative STree where
  pure  = return
  (<*>) = ap

instance Monad STree where
  return = Val
  m >>= f = case m of
    Fail  -> Fail
    Val a    -> f a
    Node l r -> Node (l `bind` f) (r `bind` f)

instance Alternative STree where
  empty   = Fail
  l <|> r = Node (ret l) (ret r)

instance MonadPlus STree

instance Foldable STree where
  foldMap f = \case
    Fail     -> mempty
    Val a    -> f a
    Node l r -> foldMap f (unit l) <> foldMap f (unit r)

instance Traversable STree where
  traverse f = \case
    Fail     -> pure Fail
    Val a    -> Val <$> f a
    Node l r -> Node <$> (ret <$> l') <*> (ret <$> r')
      where
      l' = traverse f $ unit l
      r' = traverse f $ unit r

-- }}}

-- Preventing Memoization {{{

unit :: (() -> a) -> a
unit e = e ()
{-# INLINE unit #-}

ret :: a -> () -> a
ret e () = e
{-# NOINLINE ret #-}

bind :: Monad m => (() -> m a) -> (a -> m b) -> () -> m b
bind e f () = unit e >>= f
{-# NOINLINE bind #-}

-- }}}

-- Search {{{

breadthSearch :: STree a -> [a]
breadthSearch t = loop [ret t]
  where
  loop :: [() -> STree a] -> [a]
  loop = \case
    []   -> []
    h:ts -> case unit h of
      Fail     -> loop ts
      Val a    -> a : loop ts
      Node l r -> loop $ ts ++ [l,r]

depthSearch :: Int -> STree a -> Maybe [a]
depthSearch d = \case
  Fail       -> Nothing
  Val a
    | d == 0 -> Just [a]
    | True   -> Nothing
  Node l r
    | d == 0 -> Just []
    | True   -> mergeSearch
      (depthSearch (pred d) $ unit l)
      (depthSearch (pred d) $ unit r)

iterDeepening :: STree a -> [a]
iterDeepening t = loop 0
  where
  loop  d = check d $ depthSearch d t
  check d = maybe [] (++ loop (succ d))

mergeSearch :: Maybe [a] -> Maybe [a] -> Maybe [a]
mergeSearch x y = msum [ (++) <$> x <*> y , y , x ]

{-
mergeSearch :: Maybe [a] -> Maybe [a] -> Maybe [a]
mergeSearch x y = case (x,y) of
  (Nothing,_)       -> y
  (_,Nothing)       -> x
  (Just xs,Just ys) -> Just $ xs ++ ys
-}

-- }}}

-- Util {{{

eqBy :: Eq a => (b -> a) -> b -> b -> Bool
eqBy = on (==)

compareBy :: Ord a => (b -> a) -> b -> b -> Ordering
compareBy = on compare

-- }}}

