{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

module System.Taffybar.Information.X11DesktopInfoSpec
  ( spec
  , runX11
  , TestEvent
  , TestEvent'(..)
  , TestAtom(..)
  ) where

import Control.Monad (forM, (<=<))
import Control.Monad.Trans.Reader (ReaderT(..))
import Data.Word (Word64, Word32)
import GHC.Generics (Generic)
import Graphics.X11.Types (Atom)
import Graphics.X11.Xlib.Extras (Event(..))
import UnliftIO.Exception (evaluate)
import UnliftIO.STM (atomically, newTChan, writeTChan, readTChan)

import Test.Hspec hiding (context)
import Test.QuickCheck
import Test.QuickCheck.Monadic

import System.Taffybar.Test.UtilSpec hiding (spec)
import System.Taffybar.Test.XvfbSpec (withXdummy, xpropSet, XPropName(..), XPropValue(..))
import System.Taffybar.Information.X11DesktopInfo

spec :: Spec
spec = around withXdummy $ do
  describe "withX11Context" $ do
    it "trivial" $ \dn -> example $
      runX11 dn (pure ()) `shouldReturn` ()

    it "getPrimaryOutputNumber" $ \dn -> example $
      runX11 dn getPrimaryOutputNumber `shouldReturn` Just 0

    it "read property of root window" $ \dn -> do
      xpropSet dn (XPropName "_XMONAD_VISIBLE_WORKSPACES") (XPropValue "hello")
      ws <- runX11 dn (readAsListOfString Nothing "_XMONAD_VISIBLE_WORKSPACES")
      ws `shouldBe` ["hello"]

    it "send something" $ \dn -> do
      runX11 dn $ do
        atom <- getAtom "iamanatom"
        sendCommandEvent atom 42

  describe "eventLoop" $ around_ (laxTimeout' 1_000_000) $ do
    -- fixme: it crashes after about 3 tests
    it "receives command event" $ property . noShrinking . withMaxSuccess 2 . prop_eventLoop_sendCommandEvent
    it "hammer trivial " $ property . withMaxSuccess 50 . prop_eventLoop_trivial

------------------------------------------------------------------------

prop_eventLoop_trivial :: DisplayName -> Property
prop_eventLoop_trivial dn = monadicIO $ run $
  runX11 dn $ withEventLoop (const $ pure ()) (pure ())

type TestEvent = TestEvent' TestAtom

data TestEvent' a = TestEvent
  { testEventArg1 :: !a
  , testEventArg2 :: !Word64
  } deriving (Show, Read, Eq, Generic)

newtype TestAtom = TestAtom { getTestAtom :: String }
  deriving (Show, Read, Eq, Ord, Generic)

instance Arbitrary TestEvent where
  arbitrary = TestEvent <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary TestAtom where
  arbitrary = TestAtom <$> listOf1 (chooseEnum ('a', 'z'))
  shrink = map TestAtom . filter (not . null) . shrinkList (\c -> ['a' | c /= 'a']) . getTestAtom

prop_eventLoop_sendCommandEvent :: DisplayName -> [TestEvent] -> Property
prop_eventLoop_sendCommandEvent dn msgs = monadicIO $ fmap conjoin $
  run $ runX11 dn $ do
   chan <- atomically newTChan
   withX11EventLoop dn
     (atomically . writeTChan chan <=< evaluate . getClientMessageEvent) $
     forM msgs $ \(TestEvent (TestAtom s) param) -> do
       atom <- getAtom s
       sendCommandEvent atom param
       let sent = TestEvent atom param
       recv <- atomically $ readTChan chan
       pure $ recv === Right sent

getClientMessageEvent :: Event -> Either Word32 (TestEvent' Atom)
getClientMessageEvent ClientMessageEvent{..} = case ev_data of
  arg1:_ -> Right (TestEvent ev_message_type (fromIntegral arg1))
  [] -> Left ev_event_type
getClientMessageEvent e = Left (ev_event_type e)

------------------------------------------------------------------------

runX11 :: DisplayName -> ReaderT X11Context IO a -> IO a
runX11 dn = withX11Context dn . runReaderT
