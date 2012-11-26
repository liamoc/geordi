{-# LANGUAGE GADTs, StandaloneDeriving, ScopedTypeVariables, DataKinds, KindSignatures, PolyKinds, TypeOperators, FlexibleInstances #-}
module Tests.Geordi.UrlPattern where
import Test.Framework
import Test.QuickCheck
import Test.Framework.Providers.QuickCheck2 
import Tests.Util.TypeEquality
import Control.Applicative
import qualified Data.Text as T
import Geordi.FileInfo
import Geordi.Param
import Geordi.UrlPattern.Internal
import Geordi.UrlPattern.Types
deriving instance Show (UrlSegment m f l)
deriving instance Show (UrlPattern m f ls)
deriving instance Eq (UrlPattern m f ls)
deriving instance Eq (UrlSegment m f l)

instance Arbitrary T.Text where 
  arbitrary = T.pack <$> arbitrary 

data TestParam :: * -> * where
  T :: (Param a, Show a, Eq a) => String -> Gen a -> TestParam a
instance Show (TestParam a) where
  show (T s g) = "T " ++ show s ++ " arbitrary"

allParams = [ ExI $ (T "Int"    arbitrary :: TestParam Int   )
            , ExI $ (T "Bool"   arbitrary :: TestParam Bool  )
            , ExI $ (T "String" arbitrary :: TestParam String)
            ]

urlString = (:) <$> (elements str) <*> (listOf $ elements str)
  where str ="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-._~:@!$'*+,"

instance Arbitrary (Exists TestParam) where
  arbitrary = elements allParams

data SegType :: * -> SegmentType * -> * where
  StrW       :: SegType f (StringSeg)
  UrlW       :: TestParam x -> SegType f (UrlParam x)
  CookieW    :: TestParam x -> SegType f (CookieParam x)                                
  CookieOptW :: TestParam x -> SegType f (CookieParam (Maybe x))
  QueryW     :: TestParam x -> SegType f (QueryParam x)
  QueryOptW  :: TestParam x -> SegType f (QueryParam (Maybe x))
  PostOptW   :: TestParam x -> SegType f (PostParam (Maybe x))
  PostW      :: TestParam x -> SegType f (PostParam x)
  FileW      :: SegType f (FileParam (FileInfo f))
  FileOptW   :: SegType f (FileParam (Maybe (FileInfo f)))

deriving instance Show (SegType f a)

data SegList :: * -> [SegmentType *] -> * where
  Nil  :: SegList f '[]
  Cons :: SegType f x -> SegList f xs -> SegList f (x ': xs) 

deriving instance Show (SegList f l)
instance Show (Exists (SegList f)) where show (ExI l) = "ExI " ++ show l
instance Show (Exists2 SegList) where show (ExI2 l) = "ExI2 " ++ show l

instance Show (Exists (UrlPattern m f)) where show (ExI l) = "ExI " ++ show l

segtypes :: Gen (Exists (SegType f))
segtypes = do ExI p <- arbitrary 
              elements $ [ ExI $ UrlW p
                         , ExI $ CookieW p
                         , ExI $ QueryW p
                         , ExI StrW 
                         , ExI $ PostW p
                         , ExI $ QueryOptW p
                         , ExI $ PostOptW p
                         , ExI $ CookieOptW p
                         , ExI FileW  
                         , ExI FileOptW 
                         ]

seglists :: Gen (Exists (SegList f))
seglists = sized $ go
    where go 0 = return $ ExI Nil
          go n = do ExI x  <- segtypes
                    ExI xs <- go $ n - 1 
                    return $ ExI (Cons x xs)

urlsegs :: SegType f a -> Gen (UrlSegment POST f a)
urlsegs (UrlW (T {})      ) = return Param
urlsegs (QueryW (T {})    ) = Query     . T.pack <$> urlString
urlsegs (QueryOptW (T {}) ) = QueryOpt  . T.pack <$> urlString
urlsegs (PostOptW  (T {}) ) = PostedOpt . T.pack <$> urlString
urlsegs (StrW             ) = Str       . T.pack <$> urlString
urlsegs (PostW (T {})     ) = Posted    . T.pack <$> urlString
urlsegs (CookieW (T {})   ) = Cookie    . T.pack <$> urlString
urlsegs (CookieOptW (T {})) = CookieOpt . T.pack <$> urlString
urlsegs (FileOptW         ) = FileOpt   . T.pack <$> urlString       
urlsegs (FileW            ) = File      . T.pack <$> urlString       

urlpatterns :: SegList f a -> Gen (UrlPattern POST f a)
urlpatterns Nil = return Empty
urlpatterns (Cons r rs) = liftA2 (:/) (urlsegs r) (urlpatterns rs)

instance Arbitrary (Exists2 SegList) where
   arbitrary = seglists >>= \(ExI x) -> return $ ExI2 x

instance Arbitrary (Exists2 (UrlPattern POST)) where
  arbitrary = seglists >>= \(ExI x) -> ExI2 <$> urlpatterns x


type_concat_assoc :: SegList f a -> SegList f b -> SegList f c ->  ((a :++: b) :++: c) :==: (a :++: (b :++: c))
type_concat_assoc Nil         _ _ = Refl
type_concat_assoc (Cons r rs) b c = cong (type_concat_assoc rs b c)

type_concat_rident :: SegList f a -> (a :++: '[]) :==: a
type_concat_rident Nil = Refl
type_concat_rident (Cons r rs) = cong (type_concat_rident rs)

tests :: Test
tests = testGroup "Geordi.UrlPattern" [
          testGroup "(UrlPattern, (//), Empty) forms a monoid" [
            testProperty "associativity" $
              forAll (seglists ) $ \(ExI a') ->
              forAll (seglists ) $ \(ExI b') ->
              forAll (seglists ) $ \(ExI c') -> 
              forAll (urlpatterns a') $ \a -> 
              forAll (urlpatterns b') $ \b ->
              forAll (urlpatterns c') $ \c ->
                (a // (b // c)) == subst (type_concat_assoc a' b' c') ((a // b) // c)
          , testProperty "left identity" $ \(ExI2 a') -> forAll (urlpatterns a') $ \a ->
              Empty // a == a 
          , testProperty "right identity" $ \(ExI2 a') -> forAll (urlpatterns a') $ \a ->
              subst (type_concat_rident a') (a // Empty) == a
          ]
        , testGroup "Params are valid" $ map paramTest allParams
        ]
  where paramTest (ExI (T ty xs)) = testProperty ("(parse ∘ render) == Just :: " ++ ty ++ " -> Maybe " ++ ty) 
          $ forAll xs $ \x -> parse (render x) == Just x
