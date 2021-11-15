{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE CPP #-}

#if !(MIN_VERSION_transformers(0,6,0))
{-# OPTIONS_GHC -fno-warn-deprecations #-}
#endif

module Control.Monad.Catch.Tests (tests) where

#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ < 706)
import Prelude hiding (catch)
#endif

#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative ((<*>))
#endif
import Control.Monad (unless)
import Data.Data (Data, Typeable)
import Data.IORef (newIORef, writeIORef, readIORef)

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Identity (IdentityT(..))
import Control.Monad.Reader (ReaderT(..))
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import Control.Monad.STM (STM, atomically)
--import Control.Monad.Cont (ContT(..))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Property, ioProperty, once)
import Test.QuickCheck.Monadic (monadic, run, assert)
import qualified Control.Monad.State.Lazy as LazyState
import qualified Control.Monad.State.Strict as StrictState
import qualified Control.Monad.Writer.Lazy as LazyWriter
import qualified Control.Monad.Writer.Strict as StrictWriter
import qualified Control.Monad.RWS.Lazy as LazyRWS
import qualified Control.Monad.RWS.Strict as StrictRWS
#if !(MIN_VERSION_transformers(0,6,0))
import Control.Monad.Error (ErrorT(..))
import Control.Monad.List (ListT(..))
#endif

import Control.Monad.Catch
import Control.Monad.Catch.Pure

data TestException = TestException String
    deriving (Show, Eq, Data, Typeable)

instance Exception TestException

data MSpec m = MSpec
    { mspecName :: String
    , mspecRunner :: (m Property -> Property)
    }

-- the @m Bool@ determines whether the @m ()@ has executed
data DetectableEffect m = DetectableEffect
    { detectableEffectMSpec :: MSpec m
    , detectableEffectEffectDetector :: m (m (), m Bool)
    }

data SomeMSpec = forall m. (MonadCatch m) => SomeMSpec (MSpec m)

data SomeDetectableEffect = forall m. (MonadMask m) => SomeDetectableEffect (DetectableEffect m)

testMonadCatch :: SomeMSpec -> Property
testMonadCatch (SomeMSpec MSpec { mspecRunner }) = monadic mspecRunner $
    run $ catch failure handler
  where
    failure = throwM (TestException "foo") >> error "testMonadCatch"
    handler (_ :: TestException) = return ()

testCatchJust :: SomeMSpec -> Property
testCatchJust (SomeMSpec MSpec { mspecRunner }) = monadic mspecRunner $ do
    nice <- run $ catchJust testException posFailure posHandler
    assert $ nice == ("pos", True)
    bad <- run $ catch (catchJust testException negFailure posHandler) negHandler
    assert $ bad == ("neg", True)
  where
    testException (TestException s) = if s == "pos" then Just True else Nothing
    posHandler x = return ("pos", x)
    negHandler (_ :: TestException) = return ("neg", True)
    posFailure = throwM (TestException "pos") >> error "testCatchJust pos"
    negFailure = throwM (TestException "neg") >> error "testCatchJust neg"

testDetectableEffect :: SomeDetectableEffect -> Property
testDetectableEffect (SomeDetectableEffect (DetectableEffect mspec effectDetector)) = do
    monadic (mspecRunner mspec) $ do
        effectWasPerformed <- run $ do
          (effect, detector) <- effectDetector
          _ <- runExceptT $ ExceptT (return $ Left ()) `finally` lift effect
          detector
        assert effectWasPerformed

tests :: Test
tests = testGroup "Control.Monad.Catch.Tests" $
   ([ mkMonadCatch
    , mkCatchJust
    ] <*> mspecs) ++
   ([ mkDetectableEffect
    ] <*> detectableEffects) ++
    [ testCase "ExceptT+Left" exceptTLeft
    , testCase "release error wins" releaseErrorWins
    ]
  where
    mspecs =
        [ SomeMSpec mspecIO
        , SomeMSpec mspecIdentityTIO
        , SomeMSpec mspecLazyStateTIO
        , SomeMSpec mspecStrictStateTIO
        , SomeMSpec mspecReaderTIO
        , SomeMSpec mspecLazyWriterTIO
        , SomeMSpec mspecStrictWriterTIO
        , SomeMSpec mspecLazyRWSTIO
        , SomeMSpec mspecStrictRWSTIO

        , SomeMSpec mspecMaybeTIO
#if !(MIN_VERSION_transformers(0,6,0))
        , SomeMSpec mspecErrorTIO
        , SomeMSpec mspecListTIO
#endif
        , SomeMSpec mspecSTM
        --, SomeMSpec mspecContTIO

        , SomeMSpec mspecCatchTIdentity
        , SomeMSpec mspecEitherSomeException
        ]

    mspecIO :: MSpec IO
    mspecIO = MSpec "IO" io

    mspecIdentityTIO :: MSpec (IdentityT IO)
    mspecIdentityTIO = MSpec "IdentityT IO" $ io . runIdentityT

    mspecLazyStateTIO :: MSpec (LazyState.StateT Bool IO)
    mspecLazyStateTIO = MSpec "LazyState.StateT IO" $ io . flip LazyState.evalStateT False

    mspecStrictStateTIO :: MSpec (StrictState.StateT Bool IO)
    mspecStrictStateTIO = MSpec "StrictState.StateT IO" $ io . flip StrictState.evalStateT False

    mspecReaderTIO :: MSpec (ReaderT () IO)
    mspecReaderTIO = MSpec "ReaderT IO" $ io . flip runReaderT ()

    mspecLazyWriterTIO :: MSpec (LazyWriter.WriterT () IO)
    mspecLazyWriterTIO = MSpec "LazyWriter.WriterT IO" $ io . fmap tfst . LazyWriter.runWriterT

    mspecStrictWriterTIO :: MSpec (StrictWriter.WriterT () IO)
    mspecStrictWriterTIO = MSpec "StrictWriter.WriterT IO" $ io . fmap tfst . StrictWriter.runWriterT

    mspecLazyRWSTIO :: MSpec (LazyRWS.RWST () () Bool IO)
    mspecLazyRWSTIO = MSpec "LazyRWS.RWST IO" $ \m -> io $ fmap tfst $ LazyRWS.evalRWST m () False

    mspecStrictRWSTIO :: MSpec (StrictRWS.RWST () () Bool IO)
    mspecStrictRWSTIO = MSpec "StrictRWS.RWST IO" $ \m -> io $ fmap tfst $ StrictRWS.evalRWST m () False

    mspecMaybeTIO :: MSpec (MaybeT IO)
    mspecMaybeTIO = MSpec "MaybeT IO" $ \m -> io $ fmap (maybe undefined id) (runMaybeT m)

#if !(MIN_VERSION_transformers(0,6,0))
    mspecErrorTIO :: MSpec (ErrorT String IO)
    mspecErrorTIO = MSpec "ErrorT IO" $ \m -> io $ fmap (either error id) (runErrorT m)

    mspecListTIO :: MSpec (ListT IO)
    mspecListTIO = MSpec "ListT IO" $ \m -> io $ fmap (foldr const undefined) (runListT m)
#endif

    mspecSTM :: MSpec STM
    mspecSTM = MSpec "STM" $ io . atomically

    --mspecContTIO :: MSpec (ContT () IO)
    --mspecContTIO = MSpec "ContT IO" $ \m -> io $ runContT m return

    mspecCatchTIdentity :: MSpec Catch
    mspecCatchTIdentity = MSpec "Catch" $ fromRight . runCatch

    mspecEitherSomeException :: MSpec (Either SomeException)
    mspecEitherSomeException = MSpec "Either SomeException" fromRight

    tfst :: (Property, ()) -> Property = fst
    fromRight (Left _) = error "fromRight"
    fromRight (Right a) = a
    io = ioProperty

    detectableEffects =
        [ SomeDetectableEffect $ detectableEffectIO
        , SomeDetectableEffect detectableEffectLazyStateTIO
        , SomeDetectableEffect detectableEffectStrictStateTIO
        , SomeDetectableEffect detectableEffectLazyRWSTIO
        , SomeDetectableEffect detectableEffectStrictRWSTIO
        ]

    detectableEffectIO :: DetectableEffect IO
    detectableEffectIO = DetectableEffect
      { detectableEffectMSpec = mspecIO
      , detectableEffectEffectDetector = do
          ref <- newIORef False
          return (writeIORef ref True, readIORef ref)
      }

    detectableEffectLazyStateTIO :: DetectableEffect (LazyState.StateT Bool IO)
    detectableEffectLazyStateTIO = DetectableEffect
      { detectableEffectMSpec = mspecLazyStateTIO
      , detectableEffectEffectDetector = do
          LazyState.put False
          return (LazyState.put True, LazyState.get)
      }

    detectableEffectStrictStateTIO :: DetectableEffect (StrictState.StateT Bool IO)
    detectableEffectStrictStateTIO = DetectableEffect
      { detectableEffectMSpec = mspecStrictStateTIO
      , detectableEffectEffectDetector = do
          StrictState.put False
          return (StrictState.put True, StrictState.get)
      }

    detectableEffectLazyRWSTIO :: DetectableEffect (LazyRWS.RWST () () Bool IO)
    detectableEffectLazyRWSTIO = DetectableEffect
      { detectableEffectMSpec = mspecLazyRWSTIO
      , detectableEffectEffectDetector = do
          LazyRWS.put False
          return (LazyRWS.put True, LazyRWS.get)
      }

    detectableEffectStrictRWSTIO :: DetectableEffect (StrictRWS.RWST () () Bool IO)
    detectableEffectStrictRWSTIO = DetectableEffect
      { detectableEffectMSpec = mspecStrictRWSTIO
      , detectableEffectEffectDetector = do
          StrictRWS.put False
          return (StrictRWS.put True, StrictRWS.get)
      }

    mkMonadCatch = mkMSpecTest "MonadCatch" testMonadCatch
    mkCatchJust = mkMSpecTest "catchJust" testCatchJust
    mkDetectableEffect = mkDetectableEffectTest "effect during release" testDetectableEffect

    mkMSpecTest :: String -> (SomeMSpec -> Property) -> SomeMSpec -> Test
    mkMSpecTest name test = \someMSpec@(SomeMSpec spec) ->
        testProperty (name ++ " " ++ mspecName spec) $ once $ test someMSpec

    mkDetectableEffectTest :: String -> (SomeDetectableEffect -> Property) -> SomeDetectableEffect -> Test
    mkDetectableEffectTest name test = \someDetectableEffect@(SomeDetectableEffect detectableEffect) ->
        let testName = name ++ " " ++ mspecName (detectableEffectMSpec detectableEffect)
        in testProperty testName $ once $ test someDetectableEffect

    exceptTLeft = do
      ref <- newIORef False
      Left () <- runExceptT $ ExceptT (return $ Left ()) `finally` lift (writeIORef ref True)
      val <- readIORef ref
      unless val $ error "Looks like cleanup didn't happen"

    -- if both 'use' and 'release' abort, the 'release' error should win
    releaseErrorWins = do
      Left val <- runExceptT $ ExceptT (return $ Left False) `finally` ExceptT (return $ Left True)
      unless val $ error "Looks like the 'use' error won"
