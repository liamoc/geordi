module Geordi.UrlPattern ( -- * The 'UrlPattern' monoid
                           UrlPattern
                         , Method (..)
                         , SegmentType (..)
                         , (//)
                         , nil
                           -- * URL component matchers
                         , str 
                         , param
                         , splat
                           -- * Query String Matchers
                         , query
                         , optionalQuery
                         , allQuery
                           -- * POST form parameter matchers
                         , posted
                         , optionalPosted
                         , allPosted
                           -- * Cookie matchers
                         , cookie
                         , optionalCookie
                         , allCookie
                           -- * File upload matchers
                         , upload
                         , optionalUpload
                         , allUpload
                           -- * Type indices and families
                         , module Geordi.UrlPattern.Types
                           -- * Matching and linking
                         , linkUrl
                         , matchUrl
                           -- * Some useful type-level proofs
                         , typesProof
                         , collectProof
                         , linkWitness
                         , typesWitness
                         ) where

import Geordi.UrlPattern.Internal

import Geordi.UrlPattern.Types

