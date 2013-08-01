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

module Control.Monad.Catch (
    -- * Typeclass
    -- $mtl
    MonadCatch(..)

    -- * Utilities
    -- $utilities
  , catchAll
  , catchIOError
  , catchJust
  , catchIf
  , Handler(..), catches
  , handle
  , handleJust
  , try
  , tryJust
  , onException
  , bracket
  , bracket_
  , finally
  , bracketOnError
    -- * Re-exports from Control.Exception
  , Exception(..)
  , SomeException(..)
  ) where

#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 706)
import Prelude hiding (foldr)
#else
import Prelude hiding (catch, foldr)
#endif

import Control.Exception (Exception(..), SomeException(..))
import qualified Control.Exception as ControlException
import qualified Control.Monad.Trans.RWS.Lazy as LazyRWS
import qualified Control.Monad.Trans.RWS.Strict as StrictRWS
import qualified Control.Monad.Trans.State.Lazy as LazyS
import qualified Control.Monad.Trans.State.Strict as StrictS
import qualified Control.Monad.Trans.Writer.Lazy as LazyW
import qualified Control.Monad.Trans.Writer.Strict as StrictW
import Control.Monad.Trans.Identity
import Control.Monad.Reader as Reader
import Control.Monad.RWS
import Data.Foldable

------------------------------------------------------------------------------
-- $mtl
-- The mtl style typeclass
------------------------------------------------------------------------------

class Monad m => MonadCatch m where
  -- | Throw an exception. Note that this throws when this action is run in
  -- the monad @m@, not when it is applied. It is a generalization of
  -- "Control.Exception"'s 'ControlException.throwIO'.
  throwM :: Exception e => e -> m a

  -- | Provide a handler for exceptions thrown during execution of the first
  -- action. Note that type of the type of the argument to the handler will
  -- constrain which exceptions are caught. See "Control.Exception"'s
  -- 'ControlException.catch'.
  catch :: Exception e => m a -> (e -> m a) -> m a

  -- | Runs an action with asynchronous exceptions disabled. The action is
  -- provided a method for restoring the async. environment to what it was
  -- at the 'mask' call. See "Control.Exception"'s 'ControlException.mask'.
  mask :: ((forall a. m a -> m a) -> m b) -> m b

  -- | Like 'mask', but the masked computation is not interruptible (see
  -- "Control.Exception"'s 'ControlException.uninterruptibleMask'. WARNING:
  -- Only use if you need to mask exceptions around an interruptible operation
  -- AND you can guarantee the interruptible operation will only block for a
  -- short period of time. Otherwise you render the program/thread unresponsive
  -- and/or unkillable.
  uninterruptibleMask :: ((forall a. m a -> m a) -> m b) -> m b

instance MonadCatch IO where
  throwM = ControlException.throwIO
  catch = ControlException.catch
  mask = ControlException.mask
  uninterruptibleMask = ControlException.uninterruptibleMask

instance MonadCatch m => MonadCatch (IdentityT m) where
  throwM e = lift $ throwM e
  catch (IdentityT m) f = IdentityT (catch m (runIdentityT . f))
  mask a = IdentityT $ mask $ \u -> runIdentityT (a $ q u)
    where q u = IdentityT . u . runIdentityT
  uninterruptibleMask a =
    IdentityT $ uninterruptibleMask $ \u -> runIdentityT (a $ q u)
      where q u = IdentityT . u . runIdentityT

instance MonadCatch m => MonadCatch (LazyS.StateT s m) where
  throwM e = lift $ throwM e
  catch = LazyS.liftCatch catch
  mask a = LazyS.StateT $ \s -> mask $ \u -> LazyS.runStateT (a $ q u) s
    where q u (LazyS.StateT b) = LazyS.StateT (u . b)
  uninterruptibleMask a =
    LazyS.StateT $ \s -> uninterruptibleMask $ \u -> LazyS.runStateT (a $ q u) s
      where q u (LazyS.StateT b) = LazyS.StateT (u . b)

instance MonadCatch m => MonadCatch (StrictS.StateT s m) where
  throwM e = lift $ throwM e
  catch = StrictS.liftCatch catch
  mask a = StrictS.StateT $ \s -> mask $ \u -> StrictS.runStateT (a $ q u) s
    where q u (StrictS.StateT b) = StrictS.StateT (u . b)
  uninterruptibleMask a =
    StrictS.StateT $ \s -> uninterruptibleMask $ \u -> StrictS.runStateT (a $ q u) s
      where q u (StrictS.StateT b) = StrictS.StateT (u . b)

instance MonadCatch m => MonadCatch (ReaderT r m) where
  throwM e = lift $ throwM e
  catch (ReaderT m) c = ReaderT $ \r -> m r `catch` \e -> runReaderT (c e) r
  mask a = ReaderT $ \e -> mask $ \u -> Reader.runReaderT (a $ q u) e
    where q u (ReaderT b) = ReaderT (u . b)
  uninterruptibleMask a =
    ReaderT $ \e -> uninterruptibleMask $ \u -> Reader.runReaderT (a $ q u) e
      where q u (ReaderT b) = ReaderT (u . b)

instance (MonadCatch m, Monoid w) => MonadCatch (StrictW.WriterT w m) where
  throwM e = lift $ throwM e
  catch (StrictW.WriterT m) h = StrictW.WriterT $ m `catch ` \e -> StrictW.runWriterT (h e)
  mask a = StrictW.WriterT $ mask $ \u -> StrictW.runWriterT (a $ q u)
    where q u b = StrictW.WriterT $ u (StrictW.runWriterT b)
  uninterruptibleMask a =
    StrictW.WriterT $ uninterruptibleMask $ \u -> StrictW.runWriterT (a $ q u)
      where q u b = StrictW.WriterT $ u (StrictW.runWriterT b)

instance (MonadCatch m, Monoid w) => MonadCatch (LazyW.WriterT w m) where
  throwM e = lift $ throwM e
  catch (LazyW.WriterT m) h = LazyW.WriterT $ m `catch ` \e -> LazyW.runWriterT (h e)
  mask a = LazyW.WriterT $ mask $ \u -> LazyW.runWriterT (a $ q u)
    where q u b = LazyW.WriterT $ u (LazyW.runWriterT b)
  uninterruptibleMask a =
    LazyW.WriterT $ uninterruptibleMask $ \u -> LazyW.runWriterT (a $ q u)
      where q u b = LazyW.WriterT $ u (LazyW.runWriterT b)

instance (MonadCatch m, Monoid w) => MonadCatch (LazyRWS.RWST r w s m) where
  throwM e = lift $ throwM e
  catch (LazyRWS.RWST m) h = LazyRWS.RWST $ \r s -> m r s `catch` \e -> LazyRWS.runRWST (h e) r s
  mask a = LazyRWS.RWST $ \r s -> mask $ \u -> LazyRWS.runRWST (a $ q u) r s
    where q u (LazyRWS.RWST b) = LazyRWS.RWST $ \ r s -> u (b r s)
  uninterruptibleMask a =
    LazyRWS.RWST $ \r s -> uninterruptibleMask $ \u -> LazyRWS.runRWST (a $ q u) r s
      where q u (LazyRWS.RWST b) = LazyRWS.RWST $ \ r s -> u (b r s)

instance (MonadCatch m, Monoid w) => MonadCatch (StrictRWS.RWST r w s m) where
  throwM e = lift $ throwM e
  catch (StrictRWS.RWST m) h = StrictRWS.RWST $ \r s -> m r s `catch` \e -> StrictRWS.runRWST (h e) r s
  mask a = StrictRWS.RWST $ \r s -> mask $ \u -> StrictRWS.runRWST (a $ q u) r s
    where q u (StrictRWS.RWST b) = StrictRWS.RWST $ \ r s -> u (b r s)
  uninterruptibleMask a =
    StrictRWS.RWST $ \r s -> uninterruptibleMask $ \u -> StrictRWS.runRWST (a $ q u) r s
      where q u (StrictRWS.RWST b) = StrictRWS.RWST $ \ r s -> u (b r s)

------------------------------------------------------------------------------
-- $utilities
-- These functions follow those from "Control.Exception", except that they are
-- based on methods from the 'MonadCatch' typeclass. See
-- "Control.Exception" for API usage.
------------------------------------------------------------------------------

-- | Catches all exceptions, and somewhat defeats the purpose of the extensible
-- exception system. Use sparingly.
catchAll :: MonadCatch m => m a -> (SomeException -> m a) -> m a
catchAll = catch

-- | Catch all 'IOError' (eqv. 'IOException') exceptions. Still somewhat too
-- general, but better than using 'catchAll'. See 'catchIf' for an easy way
-- of catching specific 'IOError's based on the predicates in "System.IO.Error".
catchIOError :: MonadCatch m => m a -> (IOError -> m a) -> m a
catchIOError = catch

-- | Catch exceptions only if they pass some predicate. Often useful with the
-- predicates for testing 'IOError' values in "System.IO.Error".
catchIf :: (MonadCatch m, Exception e) =>
    (e -> Bool) -> m a -> (e -> m a) -> m a
catchIf f a b = a `catch` \e -> if f e then b e else throwM e

-- | A more generalized way of determining which exceptions to catch at
-- run time.
catchJust :: (MonadCatch m, Exception e) =>
    (e -> Maybe b) -> m a -> (b -> m a) -> m a
catchJust f a b = a `catch` \e -> maybe (throwM e) b $ f e

-- | Flipped 'catch'. See "Control.Exception"'s 'ControlException.handle'.
handle :: (MonadCatch m, Exception e) => (e -> m a) -> m a -> m a
handle = flip catch
{-# INLINE handle #-}

-- | Flipped 'catchJust'. See "Control.Exception"'s 'ControlException.handleJust'.
handleJust :: (MonadCatch m, Exception e) => (e -> Maybe b) -> (b -> m a) -> m a -> m a
handleJust f = flip (catchJust f)
{-# INLINE handleJust #-}

-- | Similar to 'catch', but returns an 'Either' result. See "Control.Exception"'s
-- 'Control.Exception.try'.
try :: (MonadCatch m, Exception e) => m a -> m (Either e a)
try a = catch (Right `liftM` a) (return . Left)

-- | A variant of 'try' that takes an exception predicate to select
-- which exceptions are caught. See "Control.Exception"'s 'ControlException.tryJust'
tryJust :: (MonadCatch m, Exception e) =>
    (e -> Maybe b) -> m a -> m (Either b a)
tryJust f a = catch (Right `liftM` a) (\e -> maybe (throwM e) (return . Left) (f e))

-- | Generalized version of 'ControlException.Handler'
data Handler m a = forall e . ControlException.Exception e => Handler (e -> m a)

instance Monad m => Functor (Handler m) where
  fmap f (Handler h) = Handler (liftM f . h)

-- | Catches different sorts of exceptions. See "Control.Exception"'s 'ControlException.catches'
catches :: (Foldable f, MonadCatch m) => m a -> f (Handler m a) -> m a
catches a hs = a `catch` handler
  where
    handler e = foldr probe (throwM e) hs
      where
        probe (Handler h) xs = maybe xs h (ControlException.fromException e)

-- | Run an action only if an exception is thrown in the main action. The
-- exception is not caught, simply rethrown.
onException :: MonadCatch m => m a -> m b -> m a
onException action handler = action `catchAll` \e -> handler >> throwM e

-- | Generalized abstracted pattern of safe resource acquisition and release
-- in the face of exceptions. The first action \"acquires\" some value, which
-- is \"released\" by the second action at the end. The third action \"uses\"
-- the value and its result is the result of the 'bracket'.
--
-- If an exception occurs during the use, the release still happens before the
-- exception is rethrown.
bracket :: MonadCatch m => m a -> (a -> m b) -> (a -> m c) -> m c
bracket acquire release use = mask $ \unmasked -> do
  resource <- acquire
  result <- unmasked (use resource) `onException` release resource
  _ <- release resource
  return result

-- | Version of 'bracket' without any value being passed to the second and
-- third actions.
bracket_ :: MonadCatch m => m a -> m b -> m c -> m c
bracket_ before after action = bracket before (const after) (const action)

-- | Perform an action with a finalizer action that is run, even if an
-- exception occurs.
finally :: MonadCatch m => m a -> m b -> m a
finally action finalizer = bracket_ (return ()) finalizer action

-- | Like 'bracket', but only performs the final action if there was an
-- exception raised by the in-between computation.
bracketOnError :: MonadCatch m => m a -> (a -> m b) -> (a -> m c) -> m c
bracketOnError acquire release use = mask $ \unmasked -> do
  resource <- acquire
  unmasked (use resource) `onException` release resource
