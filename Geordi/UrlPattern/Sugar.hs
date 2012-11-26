module Geordi.UrlPattern.Sugar (r, module Geordi.UrlPattern) where
import Control.Applicative
import Data.List
import Data.Maybe
import Language.Haskell.TH.Quote
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Geordi.UrlPattern


-- Quasiquoter parser
expQuoter :: String -> Q Exp
expQuoter x 
  =  do x1 <- quoter pieceQuoter . splitOn '/' . takeWhile (/= '?')          . filter notSpace $ x
        x2 <- quoter queryQuoter . splitOn '&' . drop 1 . dropWhile (/= '?') . filter notSpace $ x
        Just slashslash <- qLookupName False "//"
        return $ AppE (AppE (VarE slashslash) x1) x2
  where notSpace y = y /= ' ' && y /= '\n' && y /= '\r' && y /= '\t'
        splitOn c = filter (/= [c]) . groupBy (\a b -> (a == c) == (b == c) ) 
        quoter subquoter ls 
          = do Just end        <- qLookupName False "nil"  
               Just slashslash <- qLookupName False "//"
               ls' <- mapM subquoter ls
               return $ foldr (\a b -> AppE (AppE (ConE slashslash) a) b) (ConE end) ls'

        queryCon    = ConE . fromJust <$> qLookupName False "query"
        strCon      = ConE . fromJust <$> qLookupName False "str"
        queryOptCon = ConE . fromJust <$> qLookupName False "queryOpt" 
        paramCon    = ConE . fromJust <$> qLookupName False "param"

        pieceQuoter (':':_) = paramCon
        pieceQuoter n       = flip AppE <$> litE (stringL n) <*> strCon
        queryQuoter ('.':n) = flip AppE <$> litE (stringL n) <*> queryOptCon
        queryQuoter n       = flip AppE <$> litE (stringL n) <*> queryCon

-- | Quasiquoter for 'UrlPattern'. Better to write @[r| \/hello\/:p1\/:p2\/ ]@
--   than: @str \"Hello\" \/\/ param \/\/ param@, but only a little.
r :: QuasiQuoter
r = QuasiQuoter expQuoter undefined undefined undefined 
