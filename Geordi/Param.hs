{-# LANGUAGE FlexibleInstances #-}

module Geordi.Param ( Param (..) ) where
import qualified Data.Text.Lazy as T

readMay :: Read a => String -> Maybe a
readMay s = case [x | (x,t) <- reads s, ("","") <- lex t] of
               [x] -> Just x
               _ -> Nothing

class Param a where 
  render :: a -> T.Text
  parse  :: T.Text -> Maybe a


instance Param T.Text where
  render = id
  parse = Just

instance Param Int where
  render = T.pack . show
  parse  = readMay . T.unpack 

instance Param Bool where
  render = T.pack . show
  parse  = readMay . T.unpack 

instance Param [Char] where
  render = T.pack
  parse  = Just . T.unpack
