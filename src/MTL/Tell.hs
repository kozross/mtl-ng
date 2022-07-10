{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module MTL.Tell
  ( -- * Type class
    ApplyTell (..),

    -- * Derivation helpers
    ComposeTellOuter (..),
    ComposeTellInner (..),
    LiftingTell (..),
  )
where

import Data.Functor (($>))
import Data.Functor.Apply (Apply)
import Data.Functor.Bind.Class (Bind)
import Data.Functor.Compose (Compose (Compose))
import Data.Kind (Type)
import MTL.Trans (BindTrans (liftBind))

-- | Laws
--
-- If you define only 'tell':
--
-- * /Tell-tell/: @'tell' x '.>' 'tell' y@ @=@ @'tell' (x '<>' y)@
--
-- If you define only 'write':
--
-- * /Write-write/: @'write' x1 y '.>' 'write' x2 z@ @=@ @'write' x2 (y <> z)@
--
-- If you define both, then both sets of laws above must hold, and additionally
-- the following, which serve as default definitions:
--
-- * @'tell'@ @=@ @'write' ()@
-- * @'write' x acc@ @=@ @'tell' acc '$>' x@
--
-- @since 1.0.0
class
  (Apply f, Semigroup w) =>
  ApplyTell (w :: Type) (f :: Type -> Type)
    | f -> w
  where
  {-# MINIMAL tell | write #-}
  {-# INLINEABLE tell #-}
  tell :: w -> f ()
  tell = write ()
  {-# INLINEABLE write #-}
  write :: forall (a :: Type). a -> w -> f a
  write x acc = tell acc $> x

-- | Since the composition of two 'Apply's is still an 'Apply', you can derive
-- 'ApplyTell' for 'Compose' in two ways: either \'borrowing\' from the \'outer\'
-- 'Apply' in the composition, or from the \'inner\'. This newtype derives
-- 'ApplyTell' the first way; it is meant to be used with @DerivingVia@.
--
-- @since 1.0.0
newtype ComposeTellOuter (f :: Type -> Type) (g :: Type -> Type) (a :: Type)
  = ComposeTellOuter (f (g a))
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
  (Apply g, ApplyTell w f, Semigroup w, Applicative g) =>
  ApplyTell w (ComposeTellOuter f g)
  where
  {-# INLINEABLE tell #-}
  tell x = ComposeTellOuter $ tell x $> pure ()
  {-# INLINEABLE write #-}
  write x acc = ComposeTellOuter $ pure <$> write x acc

-- | Similar to 'ComposeTellOuter', except we derive 'ApplyTell' from the
-- \'inner\' 'Apply' instead.
--
-- @since 1.0.0
newtype ComposeTellInner (f :: Type -> Type) (g :: Type -> Type) (a :: Type)
  = ComposeTellInner (f (g a))
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
  (Apply f, Applicative f, ApplyTell w g, Semigroup w) =>
  ApplyTell w (ComposeTellInner f g)
  where
  {-# INLINEABLE tell #-}
  tell x = ComposeTellInner $ pure . tell $ x
  {-# INLINEABLE write #-}
  write x acc = ComposeTellInner $ pure . write x $ acc

-- | A convenience newtype for writing \'lifted\' instances over transformers.
--
-- @since 1.0.0
newtype
  LiftingTell
    (t :: (Type -> Type) -> Type -> Type)
    (m :: Type -> Type)
    (a :: Type)
  = LiftingTell (t m a)
  deriving
    ( -- | @since 1.0.0
      Functor,
      -- | @since 1.0.0
      Apply
    )
    via (t m)

-- | @since 1.0.0
instance
  (ApplyTell w m, Semigroup w, BindTrans t, Bind m) =>
  ApplyTell w (LiftingTell t m)
  where
  {-# INLINEABLE tell #-}
  tell x = LiftingTell . liftBind $ tell x
  {-# INLINEABLE write #-}
  write x acc = LiftingTell . liftBind $ write x acc
