{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Compose where

import           Course.Applicative
import           Course.Core
import           Course.Functor
import           Course.Monad

-- Exactly one of these exercises will not be possible to achieve. Determine which.

newtype Compose f g a =
  Compose (f (g a))

-- Implement a Functor instance for Compose
instance (Functor f, Functor g) =>
    Functor (Compose f g) where
  h <$> (Compose x) = Compose (((<$>).(<$>)) h x)

instance (Applicative f, Applicative g) =>
  Applicative (Compose f g) where
-- Implement the pure function for an Applicative instance for Compose
  pure = Compose . pure . pure
-- Implement the (<*>) function for an Applicative instance for Compose
  --(<*>) (Compose x) (Compose y) = Compose $ lift2 (<*>) x y
  (Compose abf) <*> (Compose x) = Compose $ pure (<*>) <*> abf <*> x

instance (Monad f, Monad g) =>
  Monad (Compose f g) where
-- Implement the (=<<) function for a Monad instance for Compose
  x =<< (Compose fga) = Compose $ fga >>= \ ga -> let gfgb = ga >>= (return . x) in undefined
