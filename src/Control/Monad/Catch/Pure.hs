{-
Copyright 2012 Google Inc. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif

#ifndef MIN_VERSION_transformers
#define MIN_VERSION_transformers(x,y,z) 1
#endif

#ifndef MIN_VERSION_mtl
#define MIN_VERSION_mtl(x,y,z) 1
#endif

--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2013, (c) Google Inc. 2012
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- This module supports monads that can throw extensible exceptions. The
-- exceptions are the very same from "Control.Exception", and the operations
-- offered very similar, but here they are not limited to 'IO'.
--
-- This code is in the style of both transformers and mtl, and is compatible
-- with them, though doesn't mimic the module structure or offer the complete
-- range of features in those packages.
--
-- This is very similar to 'ErrorT' and 'MonadError', but based on features of
-- "Control.Exception". In particular, it handles the complex case of
-- asynchronous exceptions by including 'mask' in the typeclass. Note that the
-- extensible extensions feature relies the RankNTypes language extension.
--------------------------------------------------------------------

module Control.Monad.Catch.Pure (
    -- * Transformer
    -- $transformer
    CatchT(..), Catch
  , runCatch
  , mapCatchT
  ) where

#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 706)
import Prelude hiding (foldr)
#else
import Prelude hiding (catch, foldr)
#endif

import Control.Applicative
import Control.Monad.Catch
import Control.Monad.Reader as Reader
import Control.Monad.RWS
import Data.Foldable
import Data.Functor.Identity
import Data.Traversable as Traversable

------------------------------------------------------------------------------
-- $transformer
-- The @transformers@-style monad transfomer
------------------------------------------------------------------------------

-- | Add 'Exception' handling abilities to a 'Monad'.
newtype CatchT m a = CatchT { runCatchT :: m (Either SomeException a) }

type Catch = CatchT Identity

runCatch :: Catch a -> Either SomeException a
runCatch = runIdentity . runCatchT

instance Monad m => Functor (CatchT m) where
  fmap f (CatchT m) = CatchT (liftM (fmap f) m)

instance Monad m => Applicative (CatchT m) where
  pure a = CatchT (return (Right a))
  (<*>) = ap

instance Monad m => Monad (CatchT m) where
  return a = CatchT (return (Right a))
  CatchT m >>= k = CatchT $ m >>= \ea -> case ea of
    Left e -> return (Left e)
    Right a -> runCatchT (k a)
  fail = CatchT . return . Left . toException . userError

instance MonadFix m => MonadFix (CatchT m) where
  mfix f = CatchT $ mfix $ \a -> runCatchT $ f $ case a of
    Right r -> r
    _       -> error "empty mfix argument"

instance Foldable m => Foldable (CatchT m) where
  foldMap f (CatchT m) = foldMap (foldMapEither f) m where
    foldMapEither g (Right a) = g a
    foldMapEither _ (Left _) = mempty

instance (Monad m, Traversable m) => Traversable (CatchT m) where
  traverse f (CatchT m) = CatchT <$> Traversable.traverse (traverseEither f) m where
    traverseEither g (Right a) = Right <$> g a
    traverseEither _ (Left e) = pure (Left e)

instance Monad m => Alternative (CatchT m) where
  empty = mzero
  (<|>) = mplus

instance Monad m => MonadPlus (CatchT m) where
  mzero = CatchT $ return $ Left $ toException $ userError ""
  mplus (CatchT m) (CatchT n) = CatchT $ m >>= \ea -> case ea of
    Left _ -> n
    Right a -> return (Right a)

instance MonadTrans CatchT where
  lift m = CatchT $ do
    a <- m
    return $ Right a

instance MonadIO m => MonadIO (CatchT m) where
  liftIO m = CatchT $ do
    a <- liftIO m
    return $ Right a

instance Monad m => MonadCatch (CatchT m) where
  throwM = CatchT . return . Left . toException
  catch (CatchT m) c = CatchT $ m >>= \ea -> case ea of
    Left e -> case fromException e of
      Just e' -> runCatchT (c e')
      Nothing -> return (Left e)
    Right a -> return (Right a)
  mask a = a id
  uninterruptibleMask a = a id

instance MonadState s m => MonadState s (CatchT m) where
  get = lift get
  put = lift . put
#if MIN_VERSION_mtl(2,1,0)
  state = lift . state
#endif

instance MonadReader e m => MonadReader e (CatchT m) where
  ask = lift ask
  local f (CatchT m) = CatchT (local f m)

instance MonadWriter w m => MonadWriter w (CatchT m) where
  tell = lift . tell
  listen = mapCatchT $ \ m -> do
    (a, w) <- listen m
    return $! fmap (\ r -> (r, w)) a
  pass = mapCatchT $ \ m -> pass $ do
    a <- m
    return $! case a of
        Left  l      -> (Left  l, id)
        Right (r, f) -> (Right r, f)
#if MIN_VERSION_mtl(2,1,0)
  writer aw = CatchT (Right `liftM` writer aw)
#endif

instance MonadRWS r w s m => MonadRWS r w s (CatchT m)

-- | Map the unwrapped computation using the given function.
--
-- * @'runErrorT' ('mapErrorT' f m) = f ('runErrorT' m@)
mapCatchT :: (m (Either SomeException a) -> n (Either SomeException b))
          -> CatchT m a
          -> CatchT n b
mapCatchT f m = CatchT $ f (runCatchT m)
