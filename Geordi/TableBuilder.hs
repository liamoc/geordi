module Geordi.TableBuilder ( -- * The 'TableBuilder' Type
                             TableBuilder
                             -- * Prefixes and Suffixes
                           , prefix
                           , suffix
                           , monadSuffix
                             -- * Adding handlers
                           , add
                           , handle
                           , get
                           , post
                           , request
                             -- * Running the builder
                           , buildTable
                           ) where

import Geordi.TableBuilder.Internal
