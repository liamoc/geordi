{-# LANGUAGE GADTs, KindSignatures, DataKinds, TypeOperators, OverloadedStrings, RankNTypes, PatternGuards #-}
module Geordi.UrlPattern.Internal where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy as L
import Data.List
import qualified Data.Map as M
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
   File      :: T.Text -> UrlSegment POST f (FileParam (FileInfo f))
   FileOpt   :: T.Text -> UrlSegment POST f (FileParam (Maybe (FileInfo f )))
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
file           :: T.Text -> UrlPattern POST f '[FileParam (FileInfo f)]
file           = (:/ Empty) . File
optionalQuery  :: Param p => T.Text ->  UrlPattern m f '[QueryParam (Maybe p)]
optionalQuery  = (:/ Empty) . QueryOpt
optionalCookie :: Param p => T.Text -> UrlPattern m f '[CookieParam (Maybe p)]
optionalCookie = (:/ Empty) . CookieOpt
optionalPosted :: Param p => T.Text -> UrlPattern POST f '[PostParam (Maybe p)]
optionalPosted = (:/ Empty) . PostedOpt
optionalFile   :: T.Text -> UrlPattern POST f '[FileParam (Maybe (FileInfo f))]
optionalFile   = (:/ Empty) . FileOpt

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
    link' acc qs (FileOpt _   :/ p) = link' acc qs p

matchUrl :: M.Map T.Text [T.Text] -- ^ Query string Parameters 
         -> M.Map T.Text [T.Text] -- ^ Request body parameters 
         -> M.Map T.Text [T.Text] -- ^ Cookie parameters
         -> M.Map T.Text [FileInfo f] -- ^ Uploaded files
         -> [T.Text] -- ^ URL pieces (before query string)
         -> UrlPattern m f ts  -- ^ Url pattern to match against
         -> (Types ts :--> a) -- ^ Function to apply parsed results to
         -> Maybe a -- ^ Resultant value (iff the pattern matches)
matchUrl q p c f (x:xs) (Str x'      :/ ps) h = if x == x' then matchUrl q p c f xs ps h
                                                           else Nothing
matchUrl q p c f (x:xs) (Param       :/ ps) h = parse x >>= matchUrl q p c f xs ps . h 
matchUrl q p c f xs     (Query x     :/ ps) h 
   | (val:vals) <- M.findWithDefault [] x q = parse val >>= matchUrl (M.adjust (const vals) x q) p c f xs ps . h 
   | otherwise                              = Nothing
matchUrl q p c f xs     (QueryOpt x  :/ ps) h 
   | (val:vals) <- M.findWithDefault [] x q = parse val >>= matchUrl (M.adjust (const vals) x q) p c f xs ps . h . Just 
   | otherwise                              = matchUrl q p c f xs ps (h Nothing)
matchUrl q p c f xs     (Posted x    :/ ps) h 
   | (val:vals) <- M.findWithDefault [] x p = parse val >>= matchUrl q (M.adjust (const vals) x p) c f xs ps . h 
   | otherwise                              = Nothing
matchUrl q p c f xs     (PostedOpt x :/ ps) h 
   | (val:vals) <- M.findWithDefault [] x p = parse val >>= matchUrl q (M.adjust (const vals) x p) c f xs ps . h . Just
   | otherwise                              = matchUrl q p c f xs ps (h Nothing) 
matchUrl q p c f xs     (Cookie x    :/ ps) h 
   | (val:vals) <- M.findWithDefault [] x c = parse val >>= matchUrl q p (M.adjust (const vals) x c) f xs ps . h 
   | otherwise                              = Nothing
matchUrl q p c f xs     (CookieOpt x :/ ps) h 
   | (val:vals) <- M.findWithDefault [] x c = parse val >>= matchUrl q p (M.adjust (const vals) x c) f xs ps . h . Just
   | otherwise                              = matchUrl q p c f xs ps (h Nothing) 
matchUrl q p c f xs     (File x      :/ ps) h 
   | (val:vals) <- M.findWithDefault [] x f = matchUrl q p c (M.adjust (const vals) x f) xs ps (h val)
   | otherwise                              = Nothing
matchUrl q p c f xs     (FileOpt x   :/ ps) h 
   | (val:vals) <- M.findWithDefault [] x f = matchUrl q p c (M.adjust (const vals) x f) xs ps (h $ Just val)
   | otherwise                              = matchUrl q p c f xs ps (h Nothing) 
matchUrl _ _ _ _ []     (Empty           ) h = Just h 
matchUrl _ _ _ _ _      _                  _ = Nothing
