module Geordi.TableBuilder ( -- * The 'TableBuilder' Type
                             TableBuilder 
                             -- * Prefixes and Suffixes
                           , prefix
                           , suffix
                             -- * Adding handlers 
                           , add
                           , handle
                           , get
                           , post
                             -- * Running the builder
                           , buildTable
                           , buildApplication  ) where

import Geordi.TableBuilder.Internal                           
