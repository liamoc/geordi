{-# LANGUAGE PolyKinds, KindSignatures, GADTs #-}
module Geordi.Util.Exists where

data Exists :: (x -> *) -> * where
  ExI :: a b -> Exists a
