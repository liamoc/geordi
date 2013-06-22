Geordi
======

Geordi is a type-safe alternative to the simple and easy to use web microframework Scotty. It is
designed to avoid code generation and pollution of the top-level scope, while still providing a
(shallower) DSL using advanced type system features like DataKinds to ensure safety. In particular
Yesod's generation of top-level data types and names for routes makes it very difficult to nest 
and structure routes beyond big monolithic blobs of quasiquoters. At the same time, Yesod's approach
allows for pervasive type-safety. This is an experimental framework that provides similar type-safe
idioms but without generating code or data types.

Reading the code is probably not for the faint of heart.

It features a lot of cool features beyond Scotty, including, but not limited to:

* An easy to read URL handling DSL which does not pollute the global scope with data types, capable
  of handling query string, url segment, form post and file parameters in a type-safe way.
* An expressive handler system that allows for linear fallthrough and multiple handlers to
  execute, and explicit ways to redirect control flow for sophisticated applications. Once again,
  this feature promotes safer practices than Scotty.
* A modular sub-site system that allows a set of handlers to be nested underneath certain
  matchers.
* A response monad that allows the overlay of additional monad transformers based on various
  extra parameters -- essentially allowing the DSL to be arbitrarily extended. This allows
  for extensions like the quick and dirty sessions example (see SessionDB.hs).
* Type-safe link, calls and redirects, based on handler definition -- a much-touted feature of Yesod.
* The joy of using Haskell without Template Haskell garbage, unlike Yesod.
* The thrill of using an alpha-quality framework with several key features still missing.

A bunch of work still needs to be done, including:

* Move html convenience function from test into library
* Move redirect convenience function into library
* Consider type-safe redirects as separate from links, as cookie parameters can be set.
* A more general way to filter type lists in the type system, to make redirects easier.
* Convenience functions for json/xml/etc.
* Documentation
* Adding custom parser/renderer pairs to various types of parameters, making the Param type class
  simply a syntactic convenience.
* Performance optimisations (although performance is likely OK as is, it's based on wai)

Geordi makes extensive use of GHC Type extensions and will not work on any prior version to 7.6.1.


