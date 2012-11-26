module Main where

import Tests.Geordi.UrlPattern as UrlPattern
import Test.Framework


main = defaultMain [UrlPattern.tests]
