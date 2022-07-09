{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}

module MTL.Trans
  ( BindTrans (..),
    BindHoist (..),
    MonadTrans (..),
  )
where

import Data.Functor.Bind.Class (Bind)
import Data.Kind (Type)

-- | = Laws
--
-- * /Composition/: @'liftBind' (comp '>>-' f)@ @=@ @'liftBind' comp '>>-' (f
-- '>>>' 'liftBind)
--
-- @since 1.0.0
class
  (forall (m :: Type -> Type). (Bind m) => Bind (t m)) =>
  BindTrans (t :: (Type -> Type) -> Type -> Type)
  where
  liftBind ::
    forall (m :: Type -> Type) (a :: Type).
    (Bind m) =>
    m a ->
    t m a

-- | = Laws
--
-- * /Identity/: @'hoistBind' 'id'@ @=@ @'id'@
-- * /Composition/: @'hoistBind' (f '>>>' g)@ @=@ @'hoistBind' f '>>>'
-- 'hoistBind' g@
--
-- @since 1.0.0
class (BindTrans t) => BindHoist (t :: (Type -> Type) -> Type -> Type) where
  -- | The first argument to 'hoistBind' must be a bind morphism, but the type
  -- system cannot ensure this.
  hoistBind ::
    forall (m :: Type -> Type) (n :: Type -> Type) (b :: Type).
    (forall (a :: Type). m a -> n a) ->
    t m b ->
    t n b

-- | = Laws
--
-- * /Identity/: @'pure' '>>>' 'liftMonad'@ @=@ @'pure'@
--
-- @since 1.0.0
class
  ( forall (m :: Type -> Type). (Monad m) => Monad (t m),
    BindTrans t
  ) =>
  MonadTrans (t :: (Type -> Type) -> Type -> Type)
  where
  liftMonad ::
    forall (m :: Type -> Type) (a :: Type).
    (Monad m) =>
    m a ->
    t m a
