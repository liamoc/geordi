module Main where

import Tests.Geordi.UrlPath as UrlPath
import Test.Framework


main = defaultMain [UrlPath.tests]
