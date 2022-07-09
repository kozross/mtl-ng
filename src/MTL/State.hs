{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module MTL.State
  ( -- * Type class
    MonadState (..),

    -- * Helper functions
    modify,

    -- * Derivation helper
    LiftingState (..),
  )
where

import Control.Category ((>>>))
import Data.Functor.Apply (Apply)
import Data.Functor.Bind.Class (Bind ((>>-)))
import Data.Kind (Type)
import MTL.Trans (MonadTrans (liftMonad))

-- | Laws
--
-- * /Get-get/: @'get' '*>' 'get'@ @=@ @'get'@
-- * /Get-put/: @'get' '>>=' 'put'@ @=@ @'pure' ()@
-- * /Put-put/: @'put' x '*>' 'put' y@ @=@ @'put' y@
--
-- @since 1.0.0
class (Monad m) => MonadState (s :: Type) (m :: Type -> Type) | m -> s where
  get :: m s
  put :: s -> m ()

-- | @'modify' f@ is equivalent to @'get' '>>=' (f '>>>' 'put')@.
--
-- @since 1.0.0
modify ::
  forall (m :: Type -> Type) (s :: Type).
  (MonadState s m) =>
  (s -> s) ->
  m ()
modify f = get >>= (f >>> put)

-- | A convenience newtype for writing \'lifted\' instances over transformers.
--
-- @since 1.0.0
newtype
  LiftingState
    (t :: (Type -> Type) -> Type -> Type)
    (m :: Type -> Type)
    (a :: Type) = LiftingState
  { -- | @since 1.0.0
    unLiftingState :: t m a
  }
  deriving
    ( -- | @since 1.0.0
      Functor,
      -- | @since 1.0.0
      Apply,
      -- | @since 1.0.0
      Applicative,
      -- | @since 1.0.0
      Monad
    )
    via (t m)

-- | @since 1.0.0
instance (Bind (t m)) => Bind (LiftingState t m) where
  {-# INLINEABLE (>>-) #-}
  LiftingState comp >>- f = LiftingState $ comp >>- (f >>> unLiftingState)

-- | @since 1.0.0
instance
  (MonadTrans t, MonadState s m) =>
  MonadState s (LiftingState t m)
  where
  {-# INLINEABLE get #-}
  get = LiftingState $ liftMonad get
  {-# INLINEABLE put #-}
  put x = LiftingState $ liftMonad (put x)
