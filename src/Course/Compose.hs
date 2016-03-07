{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Course.Compose where

import Course.Core
import Course.Functor
import Course.Applicative
import Course.Monad

-- Exactly one of these exercises will not be possible to achieve. Determine which.

newtype Compose f g a =
  Compose (f (g a))
  deriving (Show)
           
-- $setup
-- >>> import Course.Core
-- >>> import Course.List
-- >>> import Course.Optional

-- | Test
--
-- >>> (+1) <$> (Compose (Full 1 :. Full 2 :. Empty :. Nil) ::Compose List Optional Int)
-- Compose [Full 2,Full 3,Empty]
-- >>> pure 3 :: Compose List Optional Int
-- Compose [Full 3]
-- >>> Compose (Full (+1):.Full(*2):.Nil) <*> Compose (Full 1 :. Full 2 :. Empty :. Nil)
-- Compose [Full 2,Full 3,Empty,Full 2,Full 4,Empty]

-- Implement a Functor instance for Compose
instance (Functor f, Functor g) =>
    Functor (Compose f g) where
  (<$>) :: (a -> b) -> Compose f g a -> Compose f g b
  f <$> Compose g =
    Compose $ (f <$>) <$> g

instance (Applicative f, Applicative g) =>
  Applicative (Compose f g) where
  pure x =
    Compose (pure (pure x))
  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  Compose f <*> Compose g =
    Compose (lift2 (<*>) f g)

instance (Monad f, Monad g) =>
  Monad (Compose f g) where
-- Implement the (=<<) function for a Monad instance for Compose
  (=<<) :: a -> Compose f g b -> Compose f g a -> Compose f g b
  f =<< Compose g =
    error "why this is impossible?" 
