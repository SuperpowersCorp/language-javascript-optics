module Language.JavaScript.Prelude
    ( module Exports
    ) where

import Control.Lens as Exports
import Data.String  as Exports        ( String )
import Protolude    as Exports hiding ( (&)
                                      , (<.>)
                                      , (<&>) -- sigh
                                      , Strict
                                      , from
                                      , to
                                      , uncons
                                      , unsnoc
                                      )

