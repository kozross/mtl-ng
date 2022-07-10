{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module MTL.Local
  ( -- * Type class
    ApplyLocal (..),

    -- * Derivation helpers
    ComposeLocalOuter (..),
    ComposeLocalInner (..),
    LiftingLocal (..),
  )
where

import Data.Functor.Apply (Apply)
import Data.Functor.Bind.Class (Bind)
import Data.Functor.Compose (Compose (Compose))
import Data.Kind (Type)
import MTL.Ask
  ( ApplyAsk,
    ComposeAskInner (ComposeAskInner),
    ComposeAskOuter (ComposeAskOuter),
    LiftingAsk (LiftingAsk),
  )
import MTL.Trans (BindHoist (hoistBind))

-- | = Laws
--
-- * /Local identity/: @'local' 'id'@ @=@ @'id'@
-- * /Local composition/: @'local' f ('local' g comp)@ @=@ @'local' (f '>>>' g)
-- comp@
-- * /Ask agreement/: @'local' f 'ask'@ @=@ @'fmap' f 'ask'@
--
-- @since 1.0.0
class (ApplyAsk r f) => ApplyLocal (r :: Type) (f :: Type -> Type) | f -> r where
  local :: (r -> r) -> f a -> f a

-- | Since the composition of two 'Apply's is still an 'Apply', you can derive
-- 'ApplyLocal' for 'Compose' in two ways: either \'borrowing\' from the \'outer\'
-- 'Apply' in the composition, or from the \'inner\'. This newtype derives
-- 'ApplyLocal' the first way; it is meant to be used with @DerivingVia@.
--
-- @since 1.0.0
newtype ComposeLocalOuter (f :: Type -> Type) (g :: Type -> Type) (a :: Type)
  = ComposeLocalOuter (f (g a))
  deriving
    ( -- | @since 1.0.0
      Functor,
      -- | @since 1.0.0
      Apply,
      -- | @since 1.0.0
      Applicative
    )
    via (Compose f g)
  deriving
    ( -- | @since 1.0.0
      ApplyAsk r
    )
    via (ComposeAskOuter f g)

-- | @since 1.0.0
instance
  (Apply g, Applicative g, ApplyLocal r f) =>
  ApplyLocal r (ComposeLocalOuter f g)
  where
  {-# INLINEABLE local #-}
  local f (ComposeLocalOuter g) = ComposeLocalOuter . local f $ g

-- | Similar to 'ComposeLocalOuter', except we derive 'ApplyLocal' from the
-- \'inner\' 'Apply' instead.
--
-- @since 1.0.0
newtype ComposeLocalInner (f :: Type -> Type) (g :: Type -> Type) (a :: Type)
  = ComposeLocalInner (f (g a))
  deriving
    ( -- | @since 1.0.0
      Functor,
      -- | @since 1.0.0
      Apply,
      -- | @since 1.0.0
      Applicative
    )
    via (Compose f g)
  deriving
    ( -- | @since 1.0.0
      ApplyAsk r
    )
    via (ComposeAskInner f g)

-- | @since 1.0.0
instance
  (Apply f, Applicative f, ApplyLocal r g) =>
  ApplyLocal r (ComposeLocalInner f g)
  where
  {-# INLINEABLE local #-}
  local f (ComposeLocalInner g) = ComposeLocalInner . fmap (local f) $ g

-- | A convenience newtype for writing \'lifted\' instances over transformers.
--
-- @since 1.0.0
newtype
  LiftingLocal
    (t :: (Type -> Type) -> Type -> Type)
    (m :: Type -> Type)
    (a :: Type)
  = LiftingLocal (t m a)
  deriving
    ( -- | @since 1.0.0
      Functor,
      -- | @since 1.0.0
      Apply
    )
    via (t m)
  deriving
    ( -- | @since 1.0.0
      ApplyAsk r
    )
    via (LiftingAsk t m)

-- | @since 1.0.0
instance
  (BindHoist t, ApplyLocal r m, Bind m) =>
  ApplyLocal r (LiftingLocal t m)
  where
  {-# INLINEABLE local #-}
  local f (LiftingLocal comp) = LiftingLocal . hoistBind (local f) $ comp
