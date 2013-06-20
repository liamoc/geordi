Geordi
======

Geordi is a type-safe alternative to the simple and easy to use web microframework Scotty. 

It features a lot of cool features beyond Scotty, including, but not limited to:

* An easy to read URL handling DSL which does not pollute the global scope with data types, capable
  of handling query string, url segment, form post and file parameters in a type-safe way.
* An expressive handler system that allows for linear fallthrough and multiple handlers to
  execute, and explicit ways to redirect control flow for sophisticated applications. Once again,
  this feature promotes safer practices than Scotty.
* A modular sub-site system that allows a set of handlers to be nested underneath certain
  matchers.
* Type-safe generation of links based on handler definition, a much-touted feature of Yesod.
* The joy of using Haskell without Template Haskell garbage, unlike Yesod.
* The thrill of using an alpha-quality framework with several key features still missing.

A bunch of work still needs to be done, including:

* Convenience functions for json/html/xml/etc.
* Documentation
* Adding custom parser/renderer pairs to various types of parameters, making the Param type class
  simply a syntactic convenience.
* Performance optimisations (although performance is likely OK as is, it's based on wai)

Geordi makes extensive use of GHC Type extensions and will not work on any prior version to 7.6.1.


