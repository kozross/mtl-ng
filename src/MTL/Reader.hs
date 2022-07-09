{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MTL.Reader
  ( -- * Types
    Reader (..),
    ReaderT (..),
  )
where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Comonad (Comonad)
import Control.Monad (MonadPlus)
import Control.Monad.Fix (MonadFix (mfix))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Zip (MonadZip (mzip))
import Data.Bitraversable (bitraverse)
import Data.Functor.Alt (Alt ((<!>)))
import Data.Functor.Apply (Apply)
import Data.Functor.Bind.Class (Bind ((>>-)))
import Data.Functor.Compose (Compose (Compose))
import Data.Functor.Contravariant (Contravariant)
import Data.Functor.Contravariant.Decide (Decide)
import Data.Functor.Contravariant.Divise (Divise)
import Data.Functor.Contravariant.Divisible (Decidable, Divisible)
import Data.Functor.Extend (Extend)
import Data.Kind (Type)
import Data.Monoid (Ap (Ap))
import MTL.Ask (ApplyAsk (ask), ComposeAskOuter (ComposeAskOuter))
import MTL.Local
  ( ApplyLocal (local),
    ComposeLocalOuter (ComposeLocalOuter),
  )
import MTL.Trans
  ( BindHoist (hoistBind),
    BindTrans (liftBind),
    MonadTrans (liftMonad),
  )

-- | @since 1.0.0
newtype Reader (r :: Type) (a :: Type) = Reader
  { -- | @since 1.0.0
    runReader :: r -> a
  }
  deriving
    ( -- | @since 1.0.0
      Functor,
      -- | @since 1.0.0
      Apply,
      -- | @since 1.0.0
      Applicative,
      -- | @since 1.0.0
      Bind,
      -- | @since 1.0.0
      Monad,
      -- | @since 1.0.0
      MonadFix,
      -- | @since 1.0.0
      Extend,
      -- | @since 1.0.0
      Comonad
    )
    via ((->) r)
  deriving
    ( -- | @since 1.0.0
      Semigroup,
      -- | @since 1.0.0
      Monoid
    )
    via (Ap ((->) r) a)

-- | @since 1.0.0
instance MonadZip (Reader r) where
  mzip (Reader f) (Reader g) = Reader $ \x -> (f x, g x)

-- | Satisfies the following additional laws:
--
-- * Right distribution
-- * Left catch
--
-- @since 1.0.0
instance Alt (Reader r) where
  {-# INLINEABLE (<!>) #-}
  x <!> _ = x

-- | @since 1.0.0
instance ApplyAsk r (Reader r) where
  {-# INLINEABLE ask #-}
  ask = Reader id

-- | @since 1.0.0
instance ApplyLocal r (Reader r) where
  {-# INLINEABLE local #-}
  local f (Reader g) = Reader $ \x -> g . f $ x

-- | @since 1.0.0
newtype ReaderT (r :: Type) (m :: Type -> Type) (a :: Type) = ReaderT
  { -- | @since 1.0.0
    runReaderT :: r -> m a
  }
  deriving
    ( -- | @since 1.0.0
      Semigroup,
      -- | @since 1.0.0
      Monoid
    )
    via (Ap (Compose ((->) r) m) a)
  deriving
    ( -- | @since 1.0.0
      Functor,
      -- | @since 1.0.0
      Contravariant,
      -- | @since 1.0.0
      Apply,
      -- | @since 1.0.0
      Divise,
      -- | @since 1.0.0
      Applicative,
      -- | @since 1.0.0
      Divisible,
      -- | @since 1.0.0
      Decide,
      -- | @since 1.0.0
      Decidable
    )
    via (Compose ((->) r) m)
  deriving
    ( -- | @since 1.0.0
      ApplyAsk r
    )
    via (ComposeAskOuter (Reader r) m)
  deriving
    ( -- | @since 1.0.0
      ApplyLocal r
    )
    via (ComposeLocalOuter (Reader r) m)

-- | If @m@ satisfies additional laws, then so does @'ReaderT' r m@ for any
-- choice of @r@.
--
-- @since 1.0.0
instance (Alt m) => Alt (ReaderT r m) where
  {-# INLINEABLE (<!>) #-}
  ReaderT f <!> ReaderT g = ReaderT $ \x -> f x <!> g x

-- | If @m@ satisfies additional laws, then so does @'ReaderT' r m@ for any
-- choice of @r@.
--
-- @since 1.0.0
instance (Alternative m) => Alternative (ReaderT r m) where
  {-# INLINEABLE empty #-}
  empty = ReaderT $ const empty
  {-# INLINEABLE (<|>) #-}
  ReaderT f <|> ReaderT g = ReaderT $ \x -> f x <|> g x

-- | @since 1.0.0
instance (Bind m) => Bind (ReaderT r m) where
  {-# INLINEABLE (>>-) #-}
  ReaderT f >>- g = ReaderT $ \x ->
    f x >>- (\y -> (runReaderT . g) y x)

-- | @since 1.0.0
instance (Monad m) => Monad (ReaderT r m) where
  {-# INLINEABLE (>>=) #-}
  ReaderT f >>= g = ReaderT $ \x ->
    f x >>= (\y -> (runReaderT . g) y x)

-- | @since 1.0.0
instance (MonadPlus m) => MonadPlus (ReaderT r m)

-- | @since 1.0.0
instance (MonadIO m) => MonadIO (ReaderT r m) where
  {-# INLINEABLE liftIO #-}
  liftIO comp = ReaderT $ \_ -> liftIO comp

-- | @since 1.0.0
instance (MonadFix m) => MonadFix (ReaderT r m) where
  {-# INLINEABLE mfix #-}
  mfix f = ReaderT $ \x -> mfix $ \y -> runReaderT (f y) x

-- | @since 1.0.0
instance (MonadZip m) => MonadZip (ReaderT r m) where
  {-# INLINEABLE mzip #-}
  mzip (ReaderT f) (ReaderT g) = ReaderT $ \x -> bitraverse f g (x, x)

-- | @since 1.0.0
instance BindTrans (ReaderT r) where
  {-# INLINEABLE liftBind #-}
  liftBind comp = ReaderT $ const comp

-- | @since 1.0.0
instance BindHoist (ReaderT r) where
  {-# INLINEABLE hoistBind #-}
  hoistBind f (ReaderT g) = ReaderT $ \x -> f (g x)

-- | @since 1.0.0
instance MonadTrans (ReaderT r) where
  {-# INLINEABLE liftMonad #-}
  liftMonad comp = ReaderT $ const comp
