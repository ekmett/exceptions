{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Monad.Catch.Tests (tests) where

import Prelude hiding (catch)

import Control.Monad (void)
import Data.Data (Data, Typeable)

import Control.Monad.Trans.Identity (IdentityT(..))
import Control.Monad.Reader (ReaderT(..))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Property, once)
import Test.QuickCheck.Monadic (monadic, run)
import Test.QuickCheck.Property (morallyDubiousIOProperty)
import qualified Control.Monad.State.Lazy as LazyState
import qualified Control.Monad.State.Strict as StrictState
import qualified Control.Monad.Writer.Lazy as LazyWriter
import qualified Control.Monad.Writer.Strict as StrictWriter
import qualified Control.Monad.RWS.Lazy as LazyRWS
import qualified Control.Monad.RWS.Strict as StrictRWS

import Control.Monad.Catch (Exception, MonadCatch(..), runCatch)

data TestException = TestException
    deriving (Show, Data, Typeable)

instance Exception TestException

testMonadCatch :: (Functor m, MonadCatch m) => (m Property -> Property) -> Property
testMonadCatch f = monadic f $ do
    run $ catch failure handler
  where
    failure = do
        void $ throwM TestException
        error "testMonadCatch"
    handler (_ :: TestException) = return ()

tests :: Test
tests = testGroup "Control.Monad.Catch.Tests"
    [ mkMonadCatch "IO" morallyDubiousIOProperty
    , mkMonadCatch "IdentityT IO" $ io . runIdentityT
    , mkMonadCatch "LazyState.StateT IO" $ io . flip LazyState.evalStateT ()
    , mkMonadCatch "StrictState.StateT IO" $ io . flip StrictState.evalStateT ()
    , mkMonadCatch "ReaderT IO" $ io . flip runReaderT ()
    , mkMonadCatch "LazyWriter.WriterT IO" $ io . fmap tfst . LazyWriter.runWriterT
    , mkMonadCatch "StrictWriter.WriterT IO" $ io . fmap tfst . StrictWriter.runWriterT
    , mkMonadCatch "LazyRWS.RWST IO" $ \m -> io $ fmap tfst $ LazyRWS.evalRWST m () ()
    , mkMonadCatch "StrictRWS.RWST IO" $ \m -> io $ fmap tfst $ StrictRWS.evalRWST m () ()

    , mkMonadCatch "CatchT Indentity" $ fromRight . runCatch
    ]
  where
    tfst :: (Property, ()) -> Property
    tfst = fst
    fromRight (Left _) = error "fromRight"
    fromRight (Right a) = a
    io = morallyDubiousIOProperty
    mkMonadCatch name f = testProperty ("MonadCatch " ++ name) $ once $ testMonadCatch f
