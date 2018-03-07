{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif

#ifndef MIN_VERSION_transformers
#define MIN_VERSION_transformers(x,y,z) 1
#endif

#ifndef MIN_VERSION_mtl
#define MIN_VERSION_mtl(x,y,z) 1
#endif

{-# OPTIONS_GHC -fno-warn-deprecations #-}
--------------------------------------------------------------------
-- |
-- Copyright   :  (C) Edward Kmett 2013-2015, (c) Google Inc. 2012
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
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
-- extensible exceptions feature relies on the RankNTypes language extension.
--------------------------------------------------------------------

module Control.Monad.Catch (
    -- * Typeclass
    -- $mtl
    MonadThrow(..)
  , MonadCatch(..)
  , MonadMask(..)

    -- * Utilities
    -- $utilities
  , mask_
  , uninterruptibleMask_
  , catchAll
  , catchIOError
  , catchJust
  , catchIf
  , Handler(..), catches
  , handle
  , handleAll
  , handleIOError
  , handleJust
  , handleIf
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

import Control.Exception (Exception(..), SomeException(..))
import qualified Control.Exception as ControlException
import qualified Control.Monad.STM as STM
import qualified Control.Monad.Trans.RWS.Lazy as LazyRWS
import qualified Control.Monad.Trans.RWS.Strict as StrictRWS
import qualified Control.Monad.Trans.State.Lazy as LazyS
import qualified Control.Monad.Trans.State.Strict as StrictS
import qualified Control.Monad.Trans.Writer.Lazy as LazyW
import qualified Control.Monad.Trans.Writer.Strict as StrictW
import Control.Monad.STM (STM)
import Control.Monad.Trans.List (ListT(..), runListT)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad.Trans.Error (ErrorT(..), Error, runErrorT)
import Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import Control.Monad.Trans.Cont (ContT)
import Control.Monad.Trans.Identity
import Control.Monad.Reader as Reader

import Language.Haskell.TH.Syntax (Q)

#if __GLASGOW_HASKELL__ < 706
import Prelude hiding (catch, foldr)
import Data.Foldable
import Data.Monoid
#elif __GLASGOW_HASKELL__ < 710
import Prelude hiding (foldr)
import Data.Foldable
import Data.Monoid
#endif

------------------------------------------------------------------------------
-- $mtl
-- The mtl style typeclass
------------------------------------------------------------------------------

-- | A class for monads in which exceptions may be thrown.
--
-- Instances should obey the following law:
--
-- > throwM e >> x = throwM e
--
-- In other words, throwing an exception short-circuits the rest of the monadic
-- computation.
class Monad m => MonadThrow m where
  -- | Throw an exception. Note that this throws when this action is run in
  -- the monad @m@, not when it is applied. It is a generalization of
  -- "Control.Exception"'s 'ControlException.throwIO'.
  --
  -- Should satisfy the law:
  --
  -- > throwM e >> f = throwM e
  throwM :: Exception e => e -> m a

-- | A class for monads which allow exceptions to be caught, in particular
-- exceptions which were thrown by 'throwM'.
--
-- Instances should obey the following law:
--
-- > catch (throwM e) f = f e
--
-- Note that the ability to catch an exception does /not/ guarantee that we can
-- deal with all possible exit points from a computation. Some monads, such as
-- continuation-based stacks, allow for more than just a success/failure
-- strategy, and therefore @catch@ /cannot/ be used by those monads to properly
-- implement a function such as @finally@. For more information, see
-- 'MonadMask'.
class MonadThrow m => MonadCatch m where
  -- | Provide a handler for exceptions thrown during execution of the first
  -- action. Note that type of the type of the argument to the handler will
  -- constrain which exceptions are caught. See "Control.Exception"'s
  -- 'ControlException.catch'.
  catch :: Exception e => m a -> (e -> m a) -> m a

-- | A class for monads which provide for the ability to account for
-- all possible exit points from a computation, and to mask
-- asynchronous exceptions. Continuation-based monads are invalid
-- instances of this class.
--
-- Instances should ensure that, in the following code:
--
-- > f `finally` g
--
-- The action @g@ is called regardless of what occurs within @f@, including
-- async exceptions.
class MonadCatch m => MonadMask m where
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

  -- | A generalized version of the standard bracket function which allows
  -- distinguishing different exit cases. Instead of providing it a single
  -- release action, this function takes two different actions: one for the
  -- case of a successful run of the inner function, and one in the case of an
  -- exception. The former function is provided the acquired value, while
  -- the exception release function is provided both the acquired value and
  -- the exception that was thrown. The result values of both of these
  -- functions are ignored.
  --
  -- /NOTE/ This method was added in version 0.9.0 of this
  -- library. Previously, implementation of functions like 'bracket'
  -- and 'finally' in this module were based on the 'mask' and
  -- 'uninterruptibleMask' functions only, disallowing some classes of
  -- tranformers from having @MonadMask@ instances (notably
  -- multi-exit-point transformers like 'ExceptT'). If you are a
  -- library author, you'll now need to provide an implementation for
  -- this method. As two examples, here is a @ReaderT@ implementation:
  --
  -- @
  -- generalBracket acquire release cleanup use = ReaderT $ \r ->
  --   generalBracket
  --     (runReaderT acquire r)
  --     (\resource -> runReaderT (release resource) r)
  --     (\resource e -> runReaderT (cleanup resource e) r)
  --     (\resource -> runReaderT (use resource) r)
  -- @
  --
  -- This implementation reuses the base monad's @generalBracket@, and
  -- simply uses the @ReaderT@ environment to run the relevant
  -- @acquire@, @release@, @cleanup@ (for exceptions), and @use@
  -- actions. A more complicated example is the implementation for
  -- @ExceptT@, which must implement @ExceptT@'s short-circuit logic
  -- itself:
  --
  -- @
  -- generalBracket acquire release cleanup use = ExceptT $
  --   generalBracket
  --     (runExceptT acquire)
  --     (\eresource ->
  --       case eresource of
  --         Left _ -> return ()
  --         Right resource -> runExceptT (release resource) >> return ())
  --     (\eresource e ->
  --        case eresource of
  --          Left _ -> return ()
  --          Right resource -> runExceptT (cleanup resource e) >> return ())
  --     (either (return . Left) (runExceptT . use))
  -- @
  --
  -- In this implementation, we need to deal with the potential that
  -- the @acquire@ action returned a @Left@ (as opposed to succeeding
  -- with a @Right@ or throwing an exception via @throwM@), and
  -- therefore have to handle the @Left@ case explicitly when provide
  -- @release@, @cleanup@, and @use@ actions to the base monad's
  -- implementation of @generalBracket@.
  --
  -- You should ensure that in all cases of the @acquire@ action
  -- completing successfully, either the @release@ or @cleanup@
  -- actions are called, regardless of what occurs in @use@.
  --
  -- @since 0.9.0
  generalBracket
    :: m a
    -- ^ acquire some resource
    -> (a -> m ignored1)
    -- ^ release, no exception thrown
    -> (a -> SomeException -> m ignored2)
    -- ^ release, some exception thrown; the exception will be rethrown
    -> (a -> m b)
    -- ^ inner action to perform with the resource
    -> m b

instance MonadThrow [] where
  throwM _ = []
instance MonadThrow Maybe where
  throwM _ = Nothing
instance MonadThrow Q where
  throwM = fail . show

instance MonadThrow IO where
  throwM = ControlException.throwIO
instance MonadCatch IO where
  catch = ControlException.catch
instance MonadMask IO where
  mask = ControlException.mask
  uninterruptibleMask = ControlException.uninterruptibleMask
  generalBracket acquire release cleanup use = mask $ \unmasked -> do
    resource <- acquire
    result <- unmasked (use resource) `catch` \e -> do
      _ <- cleanup resource e
      throwM e
    _ <- release resource
    return result

instance MonadThrow STM where
  throwM = STM.throwSTM
instance MonadCatch STM where
  catch = STM.catchSTM

instance e ~ SomeException => MonadThrow (Either e) where
  throwM = Left . toException
-- | @since 0.8.3
instance e ~ SomeException => MonadCatch (Either e) where
  catch (Left e) f =
    case fromException e of
      Nothing -> Left e
      Just e' -> f e'
  catch x@(Right _) _ = x
-- | @since 0.8.3
instance e ~ SomeException => MonadMask (Either e) where
  mask f = f id
  uninterruptibleMask f = f id

  generalBracket acquire release cleanup use =
    case acquire of
      Left e -> Left e
      Right resource ->
        case use resource of
          Left e -> cleanup resource e >> Left e
          Right result -> do
            _ <- release resource
            return result

instance MonadThrow m => MonadThrow (IdentityT m) where
  throwM e = lift $ throwM e
instance MonadCatch m => MonadCatch (IdentityT m) where
  catch (IdentityT m) f = IdentityT (catch m (runIdentityT . f))
instance MonadMask m => MonadMask (IdentityT m) where
  mask a = IdentityT $ mask $ \u -> runIdentityT (a $ q u)
    where q :: (m a -> m a) -> IdentityT m a -> IdentityT m a
          q u = IdentityT . u . runIdentityT
  uninterruptibleMask a =
    IdentityT $ uninterruptibleMask $ \u -> runIdentityT (a $ q u)
      where q :: (m a -> m a) -> IdentityT m a -> IdentityT m a
            q u = IdentityT . u . runIdentityT

  generalBracket acquire release cleanup use = IdentityT $
    generalBracket
      (runIdentityT acquire)
      (runIdentityT . release)
      (\resource e -> runIdentityT (cleanup resource e))
      (\resource -> runIdentityT (use resource))

instance MonadThrow m => MonadThrow (LazyS.StateT s m) where
  throwM e = lift $ throwM e
instance MonadCatch m => MonadCatch (LazyS.StateT s m) where
  catch = LazyS.liftCatch catch
instance MonadMask m => MonadMask (LazyS.StateT s m) where
  mask a = LazyS.StateT $ \s -> mask $ \u -> LazyS.runStateT (a $ q u) s
    where q :: (m (a, s) -> m (a, s)) -> LazyS.StateT s m a -> LazyS.StateT s m a
          q u (LazyS.StateT b) = LazyS.StateT (u . b)
  uninterruptibleMask a =
    LazyS.StateT $ \s -> uninterruptibleMask $ \u -> LazyS.runStateT (a $ q u) s
      where q :: (m (a, s) -> m (a, s)) -> LazyS.StateT s m a -> LazyS.StateT s m a
            q u (LazyS.StateT b) = LazyS.StateT (u . b)

  generalBracket acquire release cleanup use = LazyS.StateT $ \s0 ->
    generalBracket
      (LazyS.runStateT acquire s0)

      -- Note that we're reverting to s1 here, the state after the
      -- acquire step, and _not_ getting the state from the successful
      -- run of the inner action. This is because we may be on top of
      -- something like ExceptT, where no updated state is available.
      (\(resource, s1) -> LazyS.runStateT (release resource) s1)
      (\(resource, s1) e -> LazyS.runStateT (cleanup resource e) s1)
      (\(resource, s1) -> LazyS.runStateT (use resource) s1)

instance MonadThrow m => MonadThrow (StrictS.StateT s m) where
  throwM e = lift $ throwM e
instance MonadCatch m => MonadCatch (StrictS.StateT s m) where
  catch = StrictS.liftCatch catch
instance MonadMask m => MonadMask (StrictS.StateT s m) where
  mask a = StrictS.StateT $ \s -> mask $ \u -> StrictS.runStateT (a $ q u) s
    where q :: (m (a, s) -> m (a, s)) -> StrictS.StateT s m a -> StrictS.StateT s m a
          q u (StrictS.StateT b) = StrictS.StateT (u . b)
  uninterruptibleMask a =
    StrictS.StateT $ \s -> uninterruptibleMask $ \u -> StrictS.runStateT (a $ q u) s
      where q :: (m (a, s) -> m (a, s)) -> StrictS.StateT s m a -> StrictS.StateT s m a
            q u (StrictS.StateT b) = StrictS.StateT (u . b)

  generalBracket acquire release cleanup use = StrictS.StateT $ \s0 ->
    generalBracket
      (StrictS.runStateT acquire s0)
      (\(resource, s1) -> StrictS.runStateT (release resource) s1)
      (\(resource, s1) e -> StrictS.runStateT (cleanup resource e) s1)
      (\(resource, s1) -> StrictS.runStateT (use resource) s1)

instance MonadThrow m => MonadThrow (ReaderT r m) where
  throwM e = lift $ throwM e
instance MonadCatch m => MonadCatch (ReaderT r m) where
  catch (ReaderT m) c = ReaderT $ \r -> m r `catch` \e -> runReaderT (c e) r
instance MonadMask m => MonadMask (ReaderT r m) where
  mask a = ReaderT $ \e -> mask $ \u -> runReaderT (a $ q u) e
    where q :: (m a -> m a) -> ReaderT e m a -> ReaderT e m a
          q u (ReaderT b) = ReaderT (u . b)
  uninterruptibleMask a =
    ReaderT $ \e -> uninterruptibleMask $ \u -> runReaderT (a $ q u) e
      where q :: (m a -> m a) -> ReaderT e m a -> ReaderT e m a
            q u (ReaderT b) = ReaderT (u . b)

  generalBracket acquire release cleanup use = ReaderT $ \r ->
    generalBracket
      (runReaderT acquire r)
      (\resource -> runReaderT (release resource) r)
      (\resource e -> runReaderT (cleanup resource e) r)
      (\resource -> runReaderT (use resource) r)

instance (MonadThrow m, Monoid w) => MonadThrow (StrictW.WriterT w m) where
  throwM e = lift $ throwM e
instance (MonadCatch m, Monoid w) => MonadCatch (StrictW.WriterT w m) where
  catch (StrictW.WriterT m) h = StrictW.WriterT $ m `catch ` \e -> StrictW.runWriterT (h e)
instance (MonadMask m, Monoid w) => MonadMask (StrictW.WriterT w m) where
  mask a = StrictW.WriterT $ mask $ \u -> StrictW.runWriterT (a $ q u)
    where q :: (m (a, w) -> m (a, w)) -> StrictW.WriterT w m a -> StrictW.WriterT w m a
          q u b = StrictW.WriterT $ u (StrictW.runWriterT b)
  uninterruptibleMask a =
    StrictW.WriterT $ uninterruptibleMask $ \u -> StrictW.runWriterT (a $ q u)
      where q :: (m (a, w) -> m (a, w)) -> StrictW.WriterT w m a -> StrictW.WriterT w m a
            q u b = StrictW.WriterT $ u (StrictW.runWriterT b)

  generalBracket acquire release cleanup use = StrictW.WriterT $
    generalBracket
      (StrictW.runWriterT acquire)
      -- NOTE: The updated writer values here are actually going to be
      -- lost, as the return value of this cleanup is discarded
      (StrictW.runWriterT . release . fst)
      (\(resource, w1) e -> do
        (a, w2) <- StrictW.runWriterT (cleanup resource e)
        return (a, mappend w1 w2))
      (\(resource, w1) -> do
        (a, w2) <- StrictW.runWriterT (use resource)
        return (a, mappend w1 w2))

instance (MonadThrow m, Monoid w) => MonadThrow (LazyW.WriterT w m) where
  throwM e = lift $ throwM e
instance (MonadCatch m, Monoid w) => MonadCatch (LazyW.WriterT w m) where
  catch (LazyW.WriterT m) h = LazyW.WriterT $ m `catch ` \e -> LazyW.runWriterT (h e)
instance (MonadMask m, Monoid w) => MonadMask (LazyW.WriterT w m) where
  mask a = LazyW.WriterT $ mask $ \u -> LazyW.runWriterT (a $ q u)
    where q :: (m (a, w) -> m (a, w)) -> LazyW.WriterT w m a -> LazyW.WriterT w m a
          q u b = LazyW.WriterT $ u (LazyW.runWriterT b)
  uninterruptibleMask a =
    LazyW.WriterT $ uninterruptibleMask $ \u -> LazyW.runWriterT (a $ q u)
      where q :: (m (a, w) -> m (a, w)) -> LazyW.WriterT w m a -> LazyW.WriterT w m a
            q u b = LazyW.WriterT $ u (LazyW.runWriterT b)

  generalBracket acquire release cleanup use = LazyW.WriterT $
    generalBracket
      (LazyW.runWriterT acquire)
      (LazyW.runWriterT . release . fst)
      (\(resource, w1) e -> do
        (a, w2) <- LazyW.runWriterT (cleanup resource e)
        return (a, mappend w1 w2))
      (\(resource, w1) -> do
        (a, w2) <- LazyW.runWriterT (use resource)
        return (a, mappend w1 w2))

instance (MonadThrow m, Monoid w) => MonadThrow (LazyRWS.RWST r w s m) where
  throwM e = lift $ throwM e
instance (MonadCatch m, Monoid w) => MonadCatch (LazyRWS.RWST r w s m) where
  catch (LazyRWS.RWST m) h = LazyRWS.RWST $ \r s -> m r s `catch` \e -> LazyRWS.runRWST (h e) r s
instance (MonadMask m, Monoid w) => MonadMask (LazyRWS.RWST r w s m) where
  mask a = LazyRWS.RWST $ \r s -> mask $ \u -> LazyRWS.runRWST (a $ q u) r s
    where q :: (m (a, s, w) -> m (a, s, w)) -> LazyRWS.RWST r w s m a -> LazyRWS.RWST r w s m a
          q u (LazyRWS.RWST b) = LazyRWS.RWST $ \ r s -> u (b r s)
  uninterruptibleMask a =
    LazyRWS.RWST $ \r s -> uninterruptibleMask $ \u -> LazyRWS.runRWST (a $ q u) r s
      where q :: (m (a, s, w) -> m (a, s, w)) -> LazyRWS.RWST r w s m a -> LazyRWS.RWST r w s m a
            q u (LazyRWS.RWST b) = LazyRWS.RWST $ \ r s -> u (b r s)

  generalBracket acquire release cleanup use = LazyRWS.RWST $ \r s0 ->
    generalBracket
      (LazyRWS.runRWST acquire r s0)
      -- All comments from StateT and WriterT apply here too
      (\(resource, s1, _) -> LazyRWS.runRWST (release resource) r s1)
      (\(resource, s1, w1) e -> do
        (a, s2, w2) <- LazyRWS.runRWST (cleanup resource e) r s1
        return (a, s2, mappend w1 w2))
      (\(resource, s1, w1) -> do
        (a, s2, w2) <- LazyRWS.runRWST (use resource) r s1
        return (a, s2, mappend w1 w2))

instance (MonadThrow m, Monoid w) => MonadThrow (StrictRWS.RWST r w s m) where
  throwM e = lift $ throwM e
instance (MonadCatch m, Monoid w) => MonadCatch (StrictRWS.RWST r w s m) where
  catch (StrictRWS.RWST m) h = StrictRWS.RWST $ \r s -> m r s `catch` \e -> StrictRWS.runRWST (h e) r s
instance (MonadMask m, Monoid w) => MonadMask (StrictRWS.RWST r w s m) where
  mask a = StrictRWS.RWST $ \r s -> mask $ \u -> StrictRWS.runRWST (a $ q u) r s
    where q :: (m (a, s, w) -> m (a, s, w)) -> StrictRWS.RWST r w s m a -> StrictRWS.RWST r w s m a
          q u (StrictRWS.RWST b) = StrictRWS.RWST $ \ r s -> u (b r s)
  uninterruptibleMask a =
    StrictRWS.RWST $ \r s -> uninterruptibleMask $ \u -> StrictRWS.runRWST (a $ q u) r s
      where q :: (m (a, s, w) -> m (a, s, w)) -> StrictRWS.RWST r w s m a -> StrictRWS.RWST r w s m a
            q u (StrictRWS.RWST b) = StrictRWS.RWST $ \ r s -> u (b r s)

  generalBracket acquire release cleanup use = StrictRWS.RWST $ \r s0 ->
    generalBracket
      (StrictRWS.runRWST acquire r s0)
      (\(resource, s1, _) -> StrictRWS.runRWST (release resource) r s1)
      (\(resource, s1, w1) e -> do
        (a, s2, w2) <- StrictRWS.runRWST (cleanup resource e) r s1
        return (a, s2, mappend w1 w2))
      (\(resource, s1, w1) -> do
        (a, s2, w2) <- StrictRWS.runRWST (use resource) r s1
        return (a, s2, mappend w1 w2))

-- Transformers which are only instances of MonadThrow and MonadCatch, not MonadMask
instance MonadThrow m => MonadThrow (ListT m) where
  throwM = lift . throwM
instance MonadCatch m => MonadCatch (ListT m) where
  catch (ListT m) f = ListT $ catch m (runListT . f)

-- | Throws exceptions into the base monad.
instance MonadThrow m => MonadThrow (MaybeT m) where
  throwM = lift . throwM
-- | Catches exceptions from the base monad.
instance MonadCatch m => MonadCatch (MaybeT m) where
  catch (MaybeT m) f = MaybeT $ catch m (runMaybeT . f)

-- | Throws exceptions into the base monad.
instance (Error e, MonadThrow m) => MonadThrow (ErrorT e m) where
  throwM = lift . throwM
-- | Catches exceptions from the base monad.
instance (Error e, MonadCatch m) => MonadCatch (ErrorT e m) where
  catch (ErrorT m) f = ErrorT $ catch m (runErrorT . f)
instance (Error e, MonadMask m) => MonadMask (ErrorT e m) where
  mask f = ErrorT $ mask $ \u -> runErrorT $ f (q u)
    where
      q :: (m (Either e a) -> m (Either e a))
        -> ErrorT e m a -> ErrorT e m a
      q u (ErrorT b) = ErrorT (u b)
  uninterruptibleMask f = ErrorT $ uninterruptibleMask $ \u -> runErrorT $ f (q u)
    where
      q :: (m (Either e a) -> m (Either e a))
        -> ErrorT e m a -> ErrorT e m a
      q u (ErrorT b) = ErrorT (u b)

  generalBracket acquire release cleanup use = ErrorT $
    generalBracket
      (runErrorT acquire)
      (\eresource ->
        case eresource of
          Left _ -> return () -- nothing to release, it didn't succeed
          Right resource -> runErrorT (release resource) >> return ())
      (\eresource e ->
         case eresource of
           Left _ -> return ()
           Right resource -> runErrorT (cleanup resource e) >> return ())
      (either (return . Left) (runErrorT . use))

-- | Throws exceptions into the base monad.
instance MonadThrow m => MonadThrow (ExceptT e m) where
  throwM = lift . throwM
-- | Catches exceptions from the base monad.
instance MonadCatch m => MonadCatch (ExceptT e m) where
  catch (ExceptT m) f = ExceptT $ catch m (runExceptT . f)
instance MonadMask m => MonadMask (ExceptT e m) where
  mask f = ExceptT $ mask $ \u -> runExceptT $ f (q u)
    where
      q :: (m (Either e a) -> m (Either e a))
        -> ExceptT e m a -> ExceptT e m a
      q u (ExceptT b) = ExceptT (u b)
  uninterruptibleMask f = ExceptT $ uninterruptibleMask $ \u -> runExceptT $ f (q u)
    where
      q :: (m (Either e a) -> m (Either e a))
        -> ExceptT e m a -> ExceptT e m a
      q u (ExceptT b) = ExceptT (u b)

  generalBracket acquire release cleanup use = ExceptT $
    generalBracket
      (runExceptT acquire)
      (\eresource ->
        case eresource of
          Left _ -> return ()
          Right resource -> runExceptT (release resource) >> return ())
      (\eresource e ->
         case eresource of
           Left _ -> return ()
           Right resource -> runExceptT (cleanup resource e) >> return ())
      (either (return . Left) (runExceptT . use))

instance MonadThrow m => MonadThrow (ContT r m) where
  throwM = lift . throwM
-- I don't believe any valid of MonadCatch exists for ContT.
-- instance MonadCatch m => MonadCatch (ContT r m) where

------------------------------------------------------------------------------
-- $utilities
-- These functions follow those from "Control.Exception", except that they are
-- based on methods from the 'MonadCatch' typeclass. See
-- "Control.Exception" for API usage.
------------------------------------------------------------------------------

-- | Like 'mask', but does not pass a @restore@ action to the argument.
mask_ :: MonadMask m => m a -> m a
mask_ io = mask $ \_ -> io

-- | Like 'uninterruptibleMask', but does not pass a @restore@ action to the
-- argument.
uninterruptibleMask_ :: MonadMask m => m a -> m a
uninterruptibleMask_ io = uninterruptibleMask $ \_ -> io

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

-- | Flipped 'catchIOError'
handleIOError :: MonadCatch m => (IOError -> m a) -> m a -> m a
handleIOError = handle

-- | Flipped 'catchAll'
handleAll :: MonadCatch m => (SomeException -> m a) -> m a -> m a
handleAll = handle

-- | Flipped 'catchIf'
handleIf :: (MonadCatch m, Exception e) => (e -> Bool) -> (e -> m a) -> m a -> m a
handleIf f = flip (catchIf f)

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
--
-- Note that this is essentially a type-specialized version of
-- 'generalBracket'. This function has a more common signature (matching the
-- signature from "Control.Exception"), and is often more convenient to use. By
-- contrast, 'generalBracket' is more expressive, allowing us to implement
-- other functions like 'bracketOnError'.
bracket :: MonadMask m => m a -> (a -> m b) -> (a -> m c) -> m c
bracket acquire release use = generalBracket
  acquire
  release
  (\a _e -> release a)
  use

-- | Version of 'bracket' without any value being passed to the second and
-- third actions.
bracket_ :: MonadMask m => m a -> m b -> m c -> m c
bracket_ before after action = bracket before (const after) (const action)

-- | Perform an action with a finalizer action that is run, even if an
-- exception occurs.
finally :: MonadMask m => m a -> m b -> m a
finally action finalizer = bracket_ (return ()) finalizer action

-- | Like 'bracket', but only performs the final action if there was an
-- exception raised by the in-between computation.
bracketOnError :: MonadMask m => m a -> (a -> m b) -> (a -> m c) -> m c
bracketOnError acquire release use = generalBracket
  acquire
  (\_ -> return ())
  (\a _e -> release a)
  use
