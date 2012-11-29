{-# LANGUAGE GADTs, KindSignatures, DataKinds, TypeOperators, OverloadedStrings, RankNTypes, PatternGuards, TupleSections #-}
module Geordi.UrlPattern.Internal where

import qualified Data.Text.Lazy as T
import Data.List
import qualified Data.Map as M
import Geordi.FileInfo
import Geordi.Param
import Geordi.UrlPattern.Types
import Data.Maybe


data Method = GET | POST | AnyMethod

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
   Star      :: (Param a) => UrlSegment m f (UrlParam [a])
   StarQ     :: (Param a) => UrlSegment m f (QueryParam (M.Map T.Text [a]))
   StarP     :: (Param a) => UrlSegment POST f (PostParam (M.Map T.Text [a]))
   StarC     :: (Param a) => UrlSegment m f (CookieParam (M.Map T.Text [a]))
   StarF     :: UrlSegment POST f (FileParam (M.Map T.Text [FileInfo f]))

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
allQuery       :: Param a => UrlPattern m f '[QueryParam (M.Map T.Text [a])]
allQuery       = (:/ Empty) $ StarQ
allCookie      :: Param a => UrlPattern m f '[CookieParam (M.Map T.Text [a])]
allCookie      = (:/ Empty) $ StarC
allPosted      :: Param a => UrlPattern POST f '[PostParam (M.Map T.Text [a])]
allPosted      = (:/ Empty) $ StarP
allFile        :: UrlPattern POST f '[FileParam (M.Map T.Text [FileInfo f])]
allFile        = (:/ Empty) $ StarF
splat          :: Param a => UrlPattern m f '[UrlParam [a]]
splat          = (:/ Empty) $ Star


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
                                      in (T.concat ls)
    link' acc qs (Str s      :/ p) = link' (s : acc) qs p
    link' acc qs (Param      :/ p) = \v -> link' (render v : acc) qs p
    link' acc qs (Query x    :/ p) = \v -> link' acc ((x, render v):qs) p
    link' acc qs (QueryOpt x :/ p) = maybe (link' acc qs p) 
                                           (\v -> link' acc ((x, render v):qs) p) 
    link' acc qs (StarQ :/ p) = \m -> let stuff = concatMap (uncurry $ \a b -> map ((a,) . render) b) $ M.toList m 
                                       in link' acc (reverse stuff ++ qs) p 
    link' acc qs (Star  :/ p) = \m -> link' (reverse (map render m) ++ acc) qs p                                   
    link' acc qs (Cookie _    :/ p) = link' acc qs  p
    link' acc qs (Posted _    :/ p) = link' acc qs  p
    link' acc qs (PostedOpt _ :/ p) = link' acc qs  p
    link' acc qs (CookieOpt _ :/ p) = link' acc qs  p
    link' acc qs (File _      :/ p) = link' acc qs p
    link' acc qs (FileOpt _   :/ p) = link' acc qs p
    link' acc qs (StarP       :/ p) = link' acc qs p
    link' acc qs (StarC       :/ p) = link' acc qs p
    link' acc qs (StarF       :/ p) = link' acc qs p


-- | Boolean matching predicate. Note that this only checks the actual path component of the URL pattern. 
--   Query string and other parameters are ignored.
matches :: UrlPattern m f ts -- ^ Pattern to match
        -> [T.Text]          -- ^ Url pieces
        -> Bool
matches (Str x :/ ps) (y:ys) = (x == y) && (ps `matches` ys)
matches (Param :/ ps) (_:ys) = ps `matches` ys
matches (Star  :/ ps) ys     = not . null $ dropWhile (not . matches ps) (tails ys)
matches (_     :/ ps) ys     = ps `matches` ys
matches  Empty        []     = True
matches  Empty        _      = False

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
matchUrl q p c f xs     (StarQ       :/ ps) h = matchUrl q p c f xs ps (h $ M.map (catMaybes . map parse) q)
matchUrl q p c f xs     (StarP       :/ ps) h = matchUrl q p c f xs ps (h $ M.map (catMaybes . map parse) p)
matchUrl q p c f xs     (StarC       :/ ps) h = matchUrl q p c f xs ps (h $ M.map (catMaybes . map parse) c)
matchUrl q p c f xs     (StarF       :/ ps) h = matchUrl q p c f xs ps (h $ f)
matchUrl q p c f xs     (Star        :/ ps) h = do (glob,rest) <- find (matches ps . snd) (inits xs `zip` tails xs)
                                                   matchUrl q p c f rest ps (h $ catMaybes $ map parse glob)
matchUrl _ _ _ _ []     (Empty           ) h = Just h 
matchUrl _ _ _ _ _      (Empty           ) _ = Nothing
matchUrl _ _ _ _ []     _                  _ = Nothing


