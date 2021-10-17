{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Merkle.Generic.Merkle where

--------------------------------------------
import           Data.Functor.Compose
--------------------------------------------
import           Merkle.Generic.HRecursionSchemes
import           Merkle.Generic.BlakeHash
--------------------------------------------




data LazyMerkle m f g i
  = LazyMerkle
  { lmHash :: Hash i
  , lmLazy :: m (f g i)
  }

instance (Functor m, HFunctor f) => HFunctor (LazyMerkle m f) where
    hfmap f (LazyMerkle h m) = LazyMerkle h (hfmap f <$> m)


type LM m f = Tagged Hash `HCompose` Compose m `HCompose` f
type LMT m f = Term (LM m f)


fromLM :: LM m f x :-> LazyMerkle m f x
fromLM (HC (Tagged h (HC (Compose m)))) = LazyMerkle h m

fromLMT :: (Functor m, HFunctor f) => LMT m f :-> Term (LazyMerkle m f)
fromLMT = hcata (Term . fromLM)


toLM ::  LazyMerkle m f x :-> LM m f x
toLM (LazyMerkle h m) = HC $ Tagged h $ HC $ Compose m

toLMT :: (Functor m, HFunctor f) => Term (LazyMerkle m f) :-> LMT m f
toLMT = hcata (Term . toLM)
