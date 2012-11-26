{-# LANGUAGE GADTs, KindSignatures, DataKinds, TypeOperators, OverloadedStrings, RankNTypes #-}
module Geordi.UrlPattern.Internal where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy as L
import Data.List

import Geordi.FileInfo
import Geordi.Param
import Geordi.UrlPattern.Types


data Method = GET | POST



data UrlSegment :: Method -> * -> SegmentType * -> * where
   Param    :: (Param a) => UrlSegment m f (UrlParam a)
   Query    :: (Param a) => T.Text -> UrlSegment m f (QueryParam a)
   Posted   :: (Param a) => T.Text -> UrlSegment POST f (PostParam a)
   Cookie   :: (Param a) => T.Text -> UrlSegment m f (CookieParam a)
   QueryOpt :: (Param a) => T.Text -> UrlSegment m f (QueryParam (Maybe a))
   PostedOpt :: (Param a) => T.Text -> UrlSegment POST f (PostParam (Maybe a))
   CookieOpt :: (Param a) => T.Text -> UrlSegment m f (CookieParam (Maybe a))
   File      :: T.Text -> UrlSegment POST FilePath (FileParam (FileInfo FilePath))
   Stream    :: T.Text -> UrlSegment POST L.ByteString (FileParam (FileInfo L.ByteString))
   FileOpt   :: T.Text -> UrlSegment POST FilePath (FileParam (Maybe (FileInfo FilePath)))
   StreamOpt :: T.Text -> UrlSegment POST L.ByteString (FileParam (Maybe (FileInfo L.ByteString)))
   Str       :: T.Text -> UrlSegment m f (StringSeg)

infixr 7 :/
-- Indexed by the type of a handler for that particular UrlPattern
data UrlPattern :: Method -> * -> [SegmentType *] -> * where
  Empty :: UrlPattern m f '[] 
  (:/) :: UrlSegment m f t -> UrlPattern m f ts -> UrlPattern m f (t ': ts)

nil            :: UrlPattern m f '[]
nil            = Empty
str            :: T.Text -> UrlPattern m f '[StringSeg]
str            = (:/ Empty) . Str
query          :: Param p => T.Text ->  UrlPattern m f '[QueryParam p]
query          = (:/ Empty) . Query
posted         :: Param p => T.Text ->  UrlPattern POST f '[PostParam p]
posted         = (:/ Empty) . Posted
cookie         :: Param p => T.Text ->  UrlPattern m f '[CookieParam p]
cookie         = (:/ Empty) . Cookie
param          :: Param p => UrlPattern m f '[UrlParam p]
param          = (Param :/ Empty) 
file           :: T.Text -> UrlPattern POST FilePath '[FileParam (FileInfo FilePath)]
file           = (:/ Empty) . File
stream         :: T.Text -> UrlPattern POST L.ByteString '[FileParam (FileInfo L.ByteString)]
stream         = (:/ Empty) . Stream
optionalQuery  :: Param p => T.Text ->  UrlPattern m f '[QueryParam (Maybe p)]
optionalQuery  = (:/ Empty) . QueryOpt
optionalCookie :: Param p => T.Text -> UrlPattern m f '[CookieParam (Maybe p)]
optionalCookie = (:/ Empty) . CookieOpt
optionalPosted :: Param p => T.Text -> UrlPattern POST f '[PostParam (Maybe p)]
optionalPosted = (:/ Empty) . PostedOpt
optionalFile   :: T.Text -> UrlPattern POST FilePath '[FileParam (Maybe (FileInfo FilePath))]
optionalFile   = (:/ Empty) . FileOpt
optionalStream :: T.Text -> UrlPattern POST L.ByteString '[FileParam (Maybe (FileInfo L.ByteString))]
optionalStream = (:/ Empty) . StreamOpt

(//) :: UrlPattern m f a -> UrlPattern m f b -> UrlPattern m f (a :++: b)
Empty     // y = y
(x :/ xs) // y = x :/ (xs // y)


linkUrl :: UrlPattern m f ts -> Types (LinkSegments ts) :--> T.Text
linkUrl = link' [] []
  where 
    link' :: [T.Text] -> [(T.Text, T.Text)] -> UrlPattern m f ts -> Types (LinkSegments ts) :--> T.Text
    link' acc qs Empty             = let ls = ( "/" : intersperse "/" (reverse acc)) 
                                           ++ if qs == [] then [] 
                                              else "?" : intersperse "&" (concatMap (uncurry $ \a b -> [a,"=",b] ) $ reverse qs)
                                      in TL.toStrict (TL.fromChunks ls)
    link' acc qs (Str s      :/ p) = link' (s : acc) qs p
    link' acc qs (Param      :/ p) = \v -> link' (render v : acc) qs p
    link' acc qs (Query x    :/ p) = \v -> link' acc ((x, render v):qs) p
    link' acc qs (QueryOpt x :/ p) = maybe (link' acc qs p) 
                                           (\v -> link' acc ((x, render v):qs) p) 
    link' acc qs (Cookie _    :/ p) = link' acc qs  p
    link' acc qs (Posted _    :/ p) = link' acc qs  p
    link' acc qs (PostedOpt _ :/ p) = link' acc qs  p
    link' acc qs (CookieOpt _ :/ p) = link' acc qs  p
    link' acc qs (File _      :/ p) = link' acc qs p
    link' acc qs (Stream _    :/ p) = link' acc qs p
    link' acc qs (FileOpt _   :/ p) = link' acc qs p
    link' acc qs (StreamOpt _ :/ p) = link' acc qs p

matchUrl :: (T.Text -> Maybe T.Text) -- ^ Lookup query Parameters 
         -> (T.Text -> Maybe T.Text) -- ^ Lookup request body parameters 
         -> (T.Text -> Maybe T.Text) -- ^ Lookup cookie parameters
         -> (T.Text -> Maybe (FileInfo f)) -- ^ Lookup uploaded files
         -> [T.Text] -- ^ URL pieces (before query string)
         -> UrlPattern m f ts  -- ^ Url pattern to match against
         -> (Types ts :--> a) -- ^ Function to apply parsed results to
         -> Maybe a -- ^ Resultant value (iff the pattern matches)
matchUrl q p c f (x:xs) (Str x'     :/ ps) h = if x == x' then matchUrl q p c f xs ps h
                                                       else Nothing
matchUrl q p c f (x:xs) (Param      :/ ps) h = parse x 
                                          >>= matchUrl q p c f xs ps . h 
matchUrl q p c f xs     (Query x    :/ ps) h = q x >>= parse
                                          >>= matchUrl q p c f xs ps . h 
matchUrl q p c f xs     (QueryOpt x :/ ps) h = maybe (Just Nothing) (fmap Just . parse) (q x) 
                                          >>= matchUrl q p c f xs ps . h 
matchUrl q p c f xs     (Posted x    :/ ps) h = p x >>= parse
                                          >>= matchUrl q p c f xs ps . h 
matchUrl q p c f xs     (PostedOpt x :/ ps) h = maybe (Just Nothing) (fmap Just . parse) (p x) 
                                          >>= matchUrl q p c f xs ps . h 
matchUrl q p c f xs     (Cookie x    :/ ps) h = c x >>= parse
                                          >>= matchUrl q p c f xs ps . h 
matchUrl q p c f xs     (CookieOpt x :/ ps) h = maybe (Just Nothing) (fmap Just . parse) (c x) 
                                          >>= matchUrl q p c f xs ps . h 
matchUrl q p c f xs     (File x    :/ ps) h = f x 
                                          >>= matchUrl q p c f xs ps . h 
matchUrl q p c f xs     (FileOpt x :/ ps) h = maybe (Just Nothing) (Just . Just) (f x) 
                                          >>= matchUrl q p c f xs ps . h 
matchUrl q p c f xs     (Stream x  :/ ps) h = f x 
                                          >>= matchUrl q p c f xs ps . h 
matchUrl q p c f xs     (StreamOpt x :/ ps) h = maybe (Just Nothing) (Just . Just) (f x) 
                                          >>= matchUrl q p c f xs ps . h 
matchUrl _ _ _ _ []     (Empty           ) h = Just h 
matchUrl _ _ _ _ _      _                  _ = Nothing

-- | Mostly internal, allows one to switch on which file back-end is in use. 
fileBackend :: b L.ByteString -> b FilePath -> (forall p. b p) -> UrlPattern m f ts -> b f
fileBackend _   fil _   (File _      :/ _ ) = fil
fileBackend _   fil _   (FileOpt _   :/ _ ) = fil
fileBackend srm _   _   (Stream _    :/ _ ) = srm
fileBackend srm _   _   (StreamOpt _ :/ _ ) = srm
fileBackend srm fil nul (_           :/ ps) = fileBackend srm fil nul ps
fileBackend _   _   nul Empty               = nul


