{-# LANGUAGE GADTs, KindSignatures, DataKinds, TypeOperators, TypeFamilies, OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances, OverlappingInstances #-}
module Geordi.UrlPath where

import Control.Applicative
import Data.List
import Data.Maybe
import Language.Haskell.TH.Quote
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

readMay :: Read a => String -> Maybe a
readMay s = case [x | (x,t) <- reads s, ("","") <- lex t] of
               [x] -> Just x
               _ -> Nothing

data Method = GET | POST

data MethodSingleton :: Method -> * where
  MethodGet  :: MethodSingleton GET
  MethodPost :: MethodSingleton POST

data SegmentType x = UrlParam x | QueryParam x | CookieParam x | StringSeg | PostParam x | FileParam x

class Param a where 
  render :: a -> T.Text
  parse  :: T.Text -> Maybe a

instance Param Int where
  render = T.pack . show
  parse  = readMay . T.unpack 

instance Param Bool where
  render = T.pack . show
  parse  = readMay . T.unpack 

instance Param [Char] where
  render = T.pack
  parse  = Just . T.unpack

type family (:++:) (a :: [SegmentType *])  (b :: [SegmentType *]) :: [SegmentType *]
type instance (:++:) '[] x = x
type instance (:++:) (x ': xs) y = x ': (xs :++: y)


data UrlSegment :: Method -> * -> SegmentType * -> * where
   Param    :: (Param a) => UrlSegment m f (UrlParam a)
   Query    :: (Param a) => T.Text -> UrlSegment m f (QueryParam a)
   Posted   :: (Param a) => T.Text -> UrlSegment POST f (PostParam a)
   Cookie   :: (Param a) => T.Text -> UrlSegment m f (CookieParam a)
   QueryOpt :: (Param a) => T.Text -> UrlSegment m f (QueryParam (Maybe a))
   PostedOpt :: (Param a) => T.Text -> UrlSegment POST f (PostParam (Maybe a))
   CookieOpt :: (Param a) => T.Text -> UrlSegment m f (CookieParam (Maybe a))
   File      :: T.Text -> UrlSegment POST FilePath (FileParam FilePath)
   Stream    :: T.Text -> UrlSegment POST L.ByteString (FileParam L.ByteString)
   FileOpt   :: T.Text -> UrlSegment POST FilePath (FileParam (Maybe FilePath))
   StreamOpt :: T.Text -> UrlSegment POST L.ByteString (FileParam (Maybe L.ByteString))
   Str       :: T.Text -> UrlSegment m f (StringSeg)

infixr 7 :/
-- Indexed by the type of a handler for that particular URLPath
data UrlPath :: Method -> * -> [SegmentType *] -> * where
  Empty :: UrlPath m f '[] 
  (:/) :: UrlSegment m f t -> UrlPath m f ts -> UrlPath m f (t ': ts)

str            :: T.Text -> UrlPath m f '[StringSeg]
str            = (:/ Empty) . Str
query          :: Param p => T.Text ->  UrlPath m f '[QueryParam p]
query          = (:/ Empty) . Query
posted         :: Param p => T.Text ->  UrlPath POST f '[PostParam p]
posted         = (:/ Empty) . Posted
cookie         :: Param p => T.Text ->  UrlPath m f '[CookieParam p]
cookie         = (:/ Empty) . Cookie
param          :: Param p => UrlPath m f '[UrlParam p]
param          = (Param :/ Empty) 
file           :: T.Text -> UrlPath POST FilePath '[FileParam FilePath]
file           = (:/ Empty) . File
stream         :: T.Text -> UrlPath POST L.ByteString '[FileParam L.ByteString]
stream         = (:/ Empty) . Stream
optionalQuery  :: Param p => T.Text ->  UrlPath m f '[QueryParam (Maybe p)]
optionalQuery  = (:/ Empty) . QueryOpt
optionalCookie :: Param p => T.Text -> UrlPath m f '[CookieParam (Maybe p)]
optionalCookie = (:/ Empty) . CookieOpt
optionalPosted :: Param p => T.Text -> UrlPath POST f '[PostParam (Maybe p)]
optionalPosted = (:/ Empty) . PostedOpt
optionalFile   :: T.Text -> UrlPath POST FilePath '[FileParam (Maybe FilePath)]
optionalFile   = (:/ Empty) . FileOpt
optionalStream :: T.Text -> UrlPath POST L.ByteString '[FileParam (Maybe L.ByteString)]
optionalStream = (:/ Empty) . StreamOpt

(//) :: UrlPath m f a -> UrlPath m f b -> UrlPath m f (a :++: b)
Empty     // y = y
(x :/ xs) // y = x :/ (xs // y)

type family   LinkTo (x :: [SegmentType *])
type instance LinkTo '[]                   = T.Text
type instance LinkTo (UrlParam x ': xs)    = x -> LinkTo xs
type instance LinkTo (QueryParam x ': xs)  = x -> LinkTo xs
type instance LinkTo (PostParam x ': xs)   = LinkTo xs
type instance LinkTo (StringSeg ': xs)     = LinkTo xs
type instance LinkTo (CookieParam x ': xs) = LinkTo xs
type instance LinkTo (FileParam x ': xs)   = LinkTo xs

linkUrl :: UrlPath m f ts -> LinkTo ts
linkUrl = link' "" ""
  where 
    (.+) = T.append
    link' :: T.Text -> T.Text -> UrlPath m f ts -> LinkTo ts
    link' acc qs Empty             = acc .+ (if qs == "" then "" else T.cons '?' $ T.tail qs)
    link' acc qs (Str str :/ p)    = link' (acc .+ "/" .+ str) qs p
    link' acc qs (Cookie str :/ p) = link' acc qs  p
    link' acc qs (Posted str :/ p) = link' acc qs  p
    link' acc qs (PostedOpt str :/ p) = link' acc qs  p
    link' acc qs (CookieOpt str :/ p) = link' acc qs  p
    link' acc qs (Param :/ p)      = \v -> link' (acc .+ "/" .+ render v) qs p
    link' acc qs (Query x :/ p)    = \v -> link' acc (qs .+ "&" .+ x .+ "=" .+ render v) p
    link' acc qs (QueryOpt x :/ p) = maybe (link' acc qs p) 
                                           (\v -> link' acc (qs .+ "&" .+ x .+ "=" .+ render v) p) 
    link' acc qs (File x :/ p) = link' acc qs p
    link' acc qs (Stream x :/ p) = link' acc qs p
    link' acc qs (FileOpt x :/ p) = link' acc qs p
    link' acc qs (StreamOpt x :/ p) = link' acc qs p

splitOn c = filter (/= [c]) . groupBy (\x y -> (x == c) == (y == c) ) 

-- Quasiquoter parser
expQuoter :: String -> Q Exp
expQuoter x 
  =  do x1 <- quoter pieceQuoter . splitOn '/' . takeWhile (/= '?')          . filter notSpace $ x
        x2 <- quoter queryQuoter . splitOn '&' . drop 1 . dropWhile (/= '?') . filter notSpace $ x
        Just slashslash <- qLookupName False "//"
        return $ AppE (AppE (VarE slashslash) x1) x2
  where notSpace x = x /= ' ' && x /= '\n' && x /= '\r' && x /= '\t'
        quoter subquoter ls 
          = do Just end        <- qLookupName False "Empty"  
               Just slashslash <- qLookupName False ":/"
               ls' <- mapM subquoter ls
               return $ foldr (\x y -> AppE (AppE (ConE slashslash) x) y) (ConE end) ls'

        queryCon    = ConE . fromJust <$> qLookupName False "Query"
        strCon      = ConE . fromJust <$> qLookupName False "Str"
        queryOptCon = ConE . fromJust <$> qLookupName False "QueryOpt" 
        paramCon    = ConE . fromJust <$> qLookupName False "Param"

        pieceQuoter (':':_) = paramCon
        pieceQuoter n       = flip AppE <$> litE (stringL n) <*> strCon
        queryQuoter ('.':n) = flip AppE <$> litE (stringL n) <*> queryOptCon
        queryQuoter n       = flip AppE <$> litE (stringL n) <*> queryCon

-- Quasiquoter for UrlPath. Better to write [r| /hello/:p1/:p2/ ] 
-- than: "Hello" :/ Param :/ Param :/ Empty, but only a little.
r :: QuasiQuoter
r = QuasiQuoter expQuoter undefined undefined undefined 
