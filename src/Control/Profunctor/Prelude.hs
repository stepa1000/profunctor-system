module Control.Profunctor.Prelude
  ( module X
--  , assoc, reassoc
  ) where

import Data.Profunctor as X
import Data.Profunctor.Monad as X
--import Data.Bifunctor.Functor as X

import Data.Profunctor.Strong as X
import Data.Profunctor.Closed as X
import Data.Profunctor.Choice as X
import Data.Profunctor.Traversing as X

import Data.Profunctor.Mapping as X
import Data.Profunctor.Yoneda as X
import Data.Bifunctor.Sum as X
import Data.Bifunctor.Product as X

import Data.Bifunctor.Tannen as X
import Data.Profunctor.Composition as X
import Data.Profunctor.Sieve as X

import Data.Functor.Compose as X

--assoc ((a,b),c) = (a,(b,c))
--reassoc (a, (b, c)) = ((a, b), c)
