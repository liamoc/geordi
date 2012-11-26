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
                           , request
                             -- * Running the builder
                           , buildTable
                           , buildApplication  ) where

import Geordi.TableBuilder.Internal                           
