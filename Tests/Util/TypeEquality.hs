{-# LANGUAGE GADTs, DataKinds, KindSignatures, PolyKinds, ConstraintKinds, TypeOperators #-}
module Tests.Util.TypeEquality where

import GHC.Exts

data (:==:) :: k -> k -> * where
  Refl :: a :==: a  

cong :: (a :==: b) -> (f a :==: f b)
cong Refl = Refl

data Exists :: (a -> *) -> * where
  ExI :: f a -> Exists f

data Exists2 :: (a -> b -> *) -> * where
  ExI2 :: f a b -> Exists2 f



subst :: (a :==: b) -> f a -> f b
subst Refl x = x
