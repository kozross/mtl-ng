{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module MTL.Ask
  ( -- * Type class
    ApplyAsk (..),

    -- * Helper functions
    asks,

    -- * Derivation helpers
    ComposeAskOuter (..),
    ComposeAskInner (..),
    LiftingAsk (..),
  )
where

import Data.Functor.Apply (Apply)
import Data.Functor.Bind.Class (Bind)
import Data.Functor.Compose (Compose (Compose))
import Data.Kind (Type)
import MTL.Trans (BindTrans (liftBind))

-- | = Laws
--
-- * /Ask-ask/: @'ask' 'Data.Functor.Apply..>' 'ask'@ @=@ @'ask'@
--
-- @since 1.0.0
class (Apply f) => ApplyAsk (r :: Type) (f :: Type -> Type) | f -> r where
  ask :: f r

-- | Shorthand for @f '<$>' 'ask'@.
--
-- @since 1.0.0
asks ::
  forall (f :: Type -> Type) (a :: Type) (r :: Type).
  (ApplyAsk r f) =>
  (r -> a) ->
  f a
asks f = f <$> ask

-- | Since the composition of two 'Apply's is still an 'Apply', you can derive
-- 'ApplyAsk' for 'Compose' in two ways: either \'borrowing\' from the \'outer\'
-- 'Apply' in the composition, or from the \'inner\'. This newtype derives
-- 'ApplyAsk' the first way; it is meant to be used with @DerivingVia@.
--
-- @since 1.0.0
newtype ComposeAskOuter (f :: Type -> Type) (g :: Type -> Type) (a :: Type)
  = ComposeAskOuter (f (g a))
  deriving
    ( -- | @since 1.0.0
      Functor,
      -- | @since 1.0.0
      Apply,
      -- | @since 1.0.0
      Applicative
    )
    via (Compose f g)

-- | @since 1.0.0
instance
  (Apply f, Apply g, Applicative g, ApplyAsk r f) =>
  ApplyAsk r (ComposeAskOuter f g)
  where
  {-# INLINEABLE ask #-}
  ask = ComposeAskOuter (asks pure)

-- | Similar to 'ComposeAskInner', except we derive 'ApplyAsk' from the
-- \'inner\' 'Apply' instead.
--
-- @since 1.0.0
newtype ComposeAskInner (f :: Type -> Type) (g :: Type -> Type) (a :: Type)
  = ComposeAskInner (f (g a))
  deriving
    ( -- | @since 1.0.0
      Functor,
      -- | @since 1.0.0
      Apply,
      -- | @since 1.0.0
      Applicative
    )
    via (Compose f g)

-- | @since 1.0.0
instance
  (Apply f, Applicative f, Apply g, ApplyAsk r g) =>
  ApplyAsk r (ComposeAskInner f g)
  where
  {-# INLINEABLE ask #-}
  ask = ComposeAskInner (pure ask)

-- | A convenience newtype for writing \'lifted\' instances over transformers.
--
-- @since 1.0.0
newtype
  LiftingAsk
    (t :: (Type -> Type) -> Type -> Type)
    (m :: Type -> Type)
    (a :: Type)
  = LiftingAsk (t m a)
  deriving
    ( -- | @since 1.0.0
      Functor,
      -- | @since 1.0.0
      Apply
    )
    via (t m)

-- | @since 1.0.0
instance (BindTrans t, ApplyAsk r m, Bind m) => ApplyAsk r (LiftingAsk t m) where
  {-# INLINEABLE ask #-}
  ask = LiftingAsk . liftBind $ ask
