{-# LANGUAGE KindSignatures, DataKinds, TypeOperators, TypeFamilies #-}
module Geordi.UrlPattern.Types where

data SegmentType x = UrlParam x | QueryParam x | CookieParam x | StringSeg | PostParam x | FileParam x

type family   LinkSegments (x :: [SegmentType *]) :: [SegmentType *]
type instance LinkSegments '[]                    = '[]
type instance LinkSegments (UrlParam x    ': xs)  = UrlParam x ': LinkSegments xs
type instance LinkSegments (QueryParam x  ': xs)  = QueryParam x ': LinkSegments xs
type instance LinkSegments (StringSeg     ': xs)  = StringSeg ': LinkSegments xs
type instance LinkSegments (PostParam x   ': xs)  = LinkSegments xs
type instance LinkSegments (CookieParam x ': xs)  = LinkSegments xs
type instance LinkSegments (FileParam x   ': xs)  = LinkSegments xs

type family Types (x :: [SegmentType *]) :: [*]
type instance Types '[]                    = '[]
type instance Types (UrlParam x    ': xs)  = x ': Types xs
type instance Types (QueryParam x  ': xs)  = x ': Types xs
type instance Types (PostParam x   ': xs)  = x ': Types xs
type instance Types (CookieParam x ': xs)  = x ': Types xs
type instance Types (FileParam x   ': xs)  = x ': Types xs
type instance Types (StringSeg     ': xs)  = Types xs

type family   (:-->) (x :: [*]) (y :: *) :: *
type instance (:-->) '[]         y       = y
type instance (:-->) (x ': xs)   y       = x -> (xs :--> y)


type family (:++:) (a :: [SegmentType *])  (b :: [SegmentType *]) :: [SegmentType *]
type instance (:++:) '[] x = x
type instance (:++:) (x ': xs) y = x ': (xs :++: y)

