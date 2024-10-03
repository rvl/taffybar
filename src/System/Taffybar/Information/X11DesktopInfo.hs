{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.Information.X11DesktopInfo
-- Copyright   : (c) José A. Romero L.
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan Malison <IvanMalison@gmail.com>
-- Stability   : unstable
-- Portability : unportable
--
-- Low-level functions to access data provided by the X11 desktop via window
-- properties. One of them ('getVisibleTags') depends on the
-- 'XMonad.Hooks.TaffybarPagerHints.pagerHints' hook
-- being installed in your @~\/.xmonad\/xmonad.hs@ configuration:
--
-- > import XMonad.Hooks.TaffybarPagerHints (pagerHints)
-- >
-- > main = xmonad $ ewmh $ pagerHints $ ...
--
-----------------------------------------------------------------------------

module System.Taffybar.Information.X11DesktopInfo
  ( -- * Context
    X11Context
  , DisplayName(..)
  , withX11Context

  -- * Properties
  , X11Property
  , X11PropertyT

  -- ** Event loop
  , withX11EventLoop
  , withEventLoop
  , eventLoop

  -- ** Context getters
  , getDisplay
  , getAtom

  -- ** Basic properties of windows
  , X11Window
  , PropertyFetcher
  , fetch
  , readAsInt
  , readAsListOfInt
  , readAsListOfString
  , readAsListOfWindow
  , readAsString

  -- ** Getters
  , isWindowUrgent
  , getPrimaryOutputNumber
  , getVisibleTags

  -- ** Operations
  , doLowerWindow
  , postX11RequestSyncProp
  , sendCommandEvent
  , sendWindowEvent
  ) where

import qualified Codec.Binary.UTF8.String as UTF8
import qualified Control.Exception as E
import Control.Monad
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.IO.Unlift (MonadUnliftIO(..))
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Bits (testBit, (.|.))
import Data.Default (Default(..))
import Data.List (elemIndex)
import Data.List.Split (endBy)
import Data.Maybe (fromMaybe, listToMaybe)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Graphics.X11.Xrandr (XRRScreenResources(..), XRROutputInfo(..), xrrGetOutputInfo, xrrGetScreenResources, xrrGetOutputPrimary)
import System.Log.Logger (Priority (..), logM)
import System.Taffybar.Information.SafeX11 hiding (displayName)
import System.Taffybar.Util (labelMyThread)
import UnliftIO.Async (withAsync)
import UnliftIO.Concurrent (forkFinally, threadDelay, killThread)
import UnliftIO.Exception (bracket, throwIO, SomeException, throwString, SomeAsyncException)
import UnliftIO.MVar (MVar, newMVar, newEmptyMVar, readMVar, modifyMVar, putMVar, takeMVar)
import UnliftIO.Timeout (timeout)

logX :: MonadIO m => Priority -> String -> m ()
logX p = liftIO . logM "System.Taffybar.Information.X11DesktopInfo" p

-- | Represents a connection to an X11 display.
-- Use 'withX11Context' to construct one of these.
data X11Context = X11Context
  { ctxDisplayName :: DisplayName
  , ctxDisplay :: Display
  , ctxRoot :: Window
  , ctxAtomCache :: MVar [(String, Atom)]
  }

-- | Specifies an X11 display to connect to.
data DisplayName = DefaultDisplay
                   -- ^ Use the @DISPLAY@ environment variable.
                 | DisplayName String
                   -- ^ Of the form @hostname:number.screen_number@
                 deriving (Show, Read, Eq, Ord, Generic)

instance Default DisplayName where
  def = DefaultDisplay

-- | Translate 'DisplayName' for use with 'openDisplay'.
fromDisplayName :: DisplayName -> String
fromDisplayName DefaultDisplay = ""
fromDisplayName (DisplayName displayName) = displayName

-- | A 'ReaderT' with 'X11Context'.
type X11PropertyT m a = ReaderT X11Context m a
-- | 'IO' actions with access to an 'X11Context'.
type X11Property a = X11PropertyT IO a
type X11Window = Window
type PropertyFetcher a = Display -> Atom -> X11Window -> IO (Maybe [a])

-- | An 'X11PropertyT' that returns the 'Display' object stored in the
-- 'X11Context'.
getDisplay :: Monad m => X11PropertyT m Display
getDisplay = ctxDisplay <$> ask

doRead :: Integral a => b -> ([a] -> b)
       -> PropertyFetcher a
       -> Maybe X11Window
       -> String
       -> X11Property b
doRead b transform windowPropFn window name =
  maybe b transform <$> fetch windowPropFn window name

-- | Retrieve the property of the given window (or the root window, if Nothing)
-- with the given name as a value of type Int. If that property hasn't been set,
-- then return -1.
readAsInt :: Maybe X11Window -- ^ window to read from. Nothing means the root window.
          -> String -- ^ name of the property to retrieve
          -> X11Property Int
readAsInt = doRead (-1) (maybe (-1) fromIntegral . listToMaybe) getWindowProperty32

-- | Retrieve the property of the given window (or the root window, if Nothing)
-- with the given name as a list of Ints. If that property hasn't been set, then
-- return an empty list.
readAsListOfInt :: Maybe X11Window -- ^ window to read from. Nothing means the root window.
                -> String          -- ^ name of the property to retrieve
                -> X11Property [Int]
readAsListOfInt = doRead [] (map fromIntegral) getWindowProperty32

-- | Retrieve the property of the given window (or the root window, if Nothing)
-- with the given name as a String. If the property hasn't been set, then return
-- an empty string.
readAsString :: Maybe X11Window -- ^ window to read from. Nothing means the root window.
             -> String          -- ^ name of the property to retrieve
             -> X11Property String
readAsString = doRead "" (UTF8.decode . map fromIntegral) getWindowProperty8

-- | Retrieve the property of the given window (or the root window, if Nothing)
-- with the given name as a list of Strings. If the property hasn't been set,
-- then return an empty list.
readAsListOfString :: Maybe X11Window -- ^ window to read from. Nothing means the root window.
                   -> String          -- ^ name of the property to retrieve
                   -> X11Property [String]
readAsListOfString = doRead [] parse getWindowProperty8
  where parse = endBy "\0" . UTF8.decode . map fromIntegral

-- | Retrieve the property of the given window (or the root window, if Nothing)
-- with the given name as a list of X11 Window IDs. If the property hasn't been
-- set, then return an empty list.
readAsListOfWindow :: Maybe X11Window -- ^ window to read from. Nothing means the root window.
                   -> String          -- ^ name of the property to retrieve
                   -> X11Property [X11Window]
readAsListOfWindow = doRead [] (map fromIntegral) getWindowProperty32

-- | Determine whether the \"urgent\" flag is set in the WM_HINTS of the given
-- window.
isWindowUrgent :: X11Window -> X11Property Bool
isWindowUrgent window = do
  hints <- fetchWindowHints window
  return $ testBit (wmh_flags hints) urgencyHintBit

-- | Retrieve the value of the special @_XMONAD_VISIBLE_WORKSPACES@
-- hint set by the 'XMonad.Hooks.TaffybarPagerHints.pagerHints' hook
-- provided by [xmonad-contrib]("XMonad.Hooks.TaffybarPagerHints")
-- (see module documentation for instructions on how to do this), or
-- an empty list of strings if the @pagerHints@ hook is not available.
getVisibleTags :: X11Property [String]
getVisibleTags = readAsListOfString Nothing "_XMONAD_VISIBLE_WORKSPACES"

-- | Return the 'Atom' with the given name.
getAtom :: MonadUnliftIO m => String -> X11PropertyT m Atom
getAtom s = do
  d <- asks ctxDisplay
  cacheVar <- asks ctxAtomCache
  a <- lookup s <$> readMVar cacheVar
  let updateCacheAction = modifyMVar cacheVar $ \cache -> do
          atom <- liftIO $ internAtom d s False
          return ((s, atom):cache, atom)
  maybe updateCacheAction return a

-- | Starts an XLib event loop which listens to incoming events of:
--
--   * @PropertyChangeMask@ (i.e. 'Graphics.X11.Types.propertyNotify' events)
--   * @SubstructureNotifyMask@ (includes 'Graphics.X11.Types.mapNotify')
--   * @StructureNotifyMask@ (includes 'Graphics.X11.Types.clientMessage')
--
-- The given handler is invoked for each 'Graphics.X11.Xlib.Extras.Event' received.
--
-- When an event of type 'Graphics.X11.Xlib.Extras.MapNotifyEvent' is
-- emitted by a newly created window, 'eventLoop' will also listen for
-- 'Graphics.X11.Types.propertyNotify' events on that window.
eventLoop
  :: (HasCallStack, MonadUnliftIO m)
  => m () -- ^ Ready notification.
  -> (Maybe SomeException -> m ()) -- ^ Cancel notification.
  -> (Event -> X11PropertyT m ()) -- ^ Event handler.
  -> X11PropertyT m a -- ^ Never returns.
eventLoop ready cancelled dispatch = do
  X11Context{..} <- ask
  withRunInIO $ \run -> do
    labelMyThread "X11EventLoop"
    selectInput ctxDisplay ctxRoot $
      propertyChangeMask .|. substructureNotifyMask .|. structureNotifyMask
    sync ctxDisplay False
    logX DEBUG "eventLoop: ready"
    run $ lift ready
    allocaXEvent $ \xe -> forever $ do
      logX DEBUG "eventLoop: waiting for XNextEvent"
      nextEventInterruptible
        (nextEventUnblocker ctxDisplayName)
        (run . lift . cancelled)
        ctxDisplay xe
      event <- getEvent xe
      logX DEBUG $ "eventLoop: got an event: " ++ show event
      case event of
        MapNotifyEvent { ev_window = window } ->
          selectInput ctxDisplay window propertyChangeMask
        _ -> return ()
      run (dispatch event)

-- | Send a dummy 'ClientMessageEvent' event to root window in order
-- to unblock 'nextEventIntr'.
--
-- Open a new 'Display' connection to do this because we shouldn't use
-- the same display connection concurrently.
nextEventUnblocker :: HasCallStack => DisplayName -> SomeAsyncException -> IO ()
nextEventUnblocker dn e = do
  logX DEBUG $ "Unblocking nextEvent because of " ++ show e
  withX11Context dn $ runReaderT $ sendCommandEvent 0 0

-- | Block until an 'Event' arrives on the given X11 'Display'.
-- Special hacks added are added so that asynchronous
-- exceptions cause it to promptly unblock.
--
-- It's a bit of a mess because 'nextEventIntr' is not really
-- interruptible. This is because 'XNextEvent' seems to be resistant
-- to the [@SIGPIPE@ method](https://downloads.haskell.org/ghc/9.6.6/docs/users_guide/exts/ffi.html#interruptible-foreign-calls)
-- of interrupting FFI calls.
--
-- So we fork a thread to run 'nextEventIntr' and wait for an MVar to
-- receive its result. Under normal operation, the thread will finish
-- after an event arrives. If an asynchronous exception is thrown, this
-- thread will linger vexatiously.
--
-- To encourage 'nextEventIntr' to return, we send a dummy X event
-- known to match the current event mask. This unblocker function is
-- the first 'IO ()' parameter.
--
-- The second 'IO ()' parameter is a notification function which will
-- be called when the thread actually finishes (successfully or
-- otherwise). This is mostly useful for unit tests, where
-- 'nextEventIntr' threads can pile up until the X server connection
-- limit is reached.
nextEventInterruptible
  :: HasCallStack
  => (SomeAsyncException -> IO ()) -- ^ Unblocker
  -> (Maybe SomeException -> IO ()) -- ^ Notification of completion.
  -> Display -- ^ Wait for event on this display connection.
  -> XEventPtr
  -> IO ()
nextEventInterruptible unblocker notifier d xe = do
  result <- newEmptyMVar

  -- Run nextEventIntr in a thread. This is a FFI call which is marked
  -- as interruptible, but is resistant to SIGPIPE.
  -- A new XEvent is allocated every time because each nextEvent call
  -- is running in a new thread.
  let eventThread = do
        labelMyThread "XNextEvent"
        nextEventIntr d xe
  let finalize r = do
        notifier (either Just (const Nothing) r)
        putMVar result r
  t <- forkFinally eventThread finalize

  let unblock e = unblocker e >> killThread t >> throwIO e

  -- Block waiting for eventThread to return.
  -- Waiting for an MVar is interruptible of course.
  -- If an async exception has been thrown, apply plunger.
  -- Avoid UnliftIO.catch because of uninterruptibleMask.
  r <- takeMVar result `E.catch` unblock

  either throwIO pure r

-- | The combination of 'withEventLoop' and 'withX11Context'.
--
-- Opens a new connection to the given X11 display and runs an event
-- handling loop in a thread whilst the given action is running.
--
-- The event handler is an 'X11PropertyT' action.
withX11EventLoop
  :: MonadUnliftIO m
  => DisplayName -- ^ Open connection to this X11 display
  -> (Event -> X11PropertyT m ()) -- ^ Event handler
  -> m a  -- ^ Main action
  -> m a
withX11EventLoop dn h = withX11Context dn . runReaderT . withEventLoop h

-- | Fork an 'eventLoop' thread to handle X11 events in the background
-- while running the given action.
--
-- Events will be dispatched synchronously in the 'eventLoop' thread
-- by the given handler function.
--
-- NB: The event loop needs its own 'X11Context' to separately handle
-- communications from the X server. The 'X11Context' should not be
-- shared with other threads.
withEventLoop
  :: (HasCallStack, MonadUnliftIO m)
  => (Event -> X11PropertyT m ())
  -> m a
  -> X11PropertyT m a
withEventLoop dispatch action = do
  ready <- newEmptyMVar
  cancelled <- newEmptyMVar
  let eventLoop' = eventLoop (putMVar ready ()) (putMVar cancelled) dispatch
      action' = do
        takeMVar ready
        r <- lift $ slowDown 10_000 action
        getDisplay >>= liftIO . flip sync False
        pure r
      waitALittle = timeout 200_000 (takeMVar cancelled) >>= \case
        Just _ -> pure ()
        Nothing -> throwString "withEventLoop: did not cancel within 200msec"

  -- NB: Not UnliftIO.finally because of uninterruptibleMask
  withRunInIO $ \run ->
    run (withAsync eventLoop' (const action')) `E.finally` waitALittle

-- | Ensures than an action blocks for at least the given number of
-- microseconds.
slowDown :: MonadUnliftIO m => Int -> m a -> m a
slowDown minTimeUsec = bracket' before after . const
  where
    before = getMonotonicTimeNSec
    after startTime = do
      endTime <- getMonotonicTimeNSec
      let margin = minTimeUsec - fromIntegral ((endTime - startTime) `div` 1000)
      when (margin > 0) $ threadDelay margin
    -- NB: Not UnliftIO.bracket because of uninterruptibleMask
    bracket' a b c = withRunInIO $ \run -> E.bracket a b (run . c)

-- | Emit a \"command\" event with one argument for the X server. This is used
-- to send events that can be received by event hooks in the XMonad process and
-- acted upon in that context.
sendCommandEvent :: MonadIO m => Atom -> Atom -> X11PropertyT m ()
sendCommandEvent cmd arg = sendCustomEvent cmd arg Nothing

-- | Similar to 'sendCommandEvent', but with an argument of type 'X11Window'.
sendWindowEvent :: Atom -> X11Window -> X11Property ()
sendWindowEvent cmd win = sendCustomEvent cmd cmd (Just win)

-- | Builds a new 'X11Context' containing a connection to the given
-- X11 display and its root window.
--
-- If the X11 connection could not be opened, it will throw
-- @'userError' "openDisplay"@. This can occur if the
-- @X -maxclients@ limit has been exceeded.
withX11Context
  :: (HasCallStack, MonadUnliftIO m)
  => DisplayName -- ^ Display name.
  -> (X11Context -> m a) -- ^ Action to run.
  -> m a
withX11Context ctxDisplayName = bracket (liftIO open) (liftIO . close)
  where
    open = do
      d <- openDisplay (fromDisplayName ctxDisplayName)
      ctxRoot <- rootWindow d (defaultScreen d)
      ctxAtomCache <- newMVar []
      return $ X11Context{ctxDisplay=d,..}
    close = closeDisplay . ctxDisplay

-- | Apply the given function to the given window in order to obtain the X11
-- property with the given name, or Nothing if no such property can be read.
fetch :: (Integral a)
      => PropertyFetcher a -- ^ Function to use to retrieve the property.
      -> Maybe X11Window   -- ^ Window to read from. Nothing means the root Window.
      -> String            -- ^ Name of the property to retrieve.
      -> X11Property (Maybe [a])
fetch fetcher window name = do
  X11Context{..} <- ask
  atom <- getAtom name
  liftIO $ fetcher ctxDisplay atom (fromMaybe ctxRoot window)

-- | Retrieve the @WM_HINTS@ mask assigned by the X server to the given window.
fetchWindowHints :: X11Window -> X11Property WMHints
fetchWindowHints window = do
  d <- getDisplay
  liftIO $ getWMHints d window

-- | Emit an event of type @ClientMessage@ that can be listened to and consumed
-- by XMonad event hooks.
sendCustomEvent :: MonadIO m
                => Atom -- ^ Command
                -> Atom -- ^ Argument
                -> Maybe X11Window -- ^ 'Just' a window, or 'Nothing' for the root window
                -> X11PropertyT m ()
sendCustomEvent cmd arg win = do
  X11Context{..} <- ask
  let win' = fromMaybe ctxRoot win
  liftIO $ allocaXEvent $ \e -> do
    setEventType e clientMessage
    setClientMessageEvent e win' cmd 32 arg currentTime
    sendEvent ctxDisplay ctxRoot False structureNotifyMask e
    sync ctxDisplay False

-- | Post the provided X11Property to taffybar's dedicated X11 thread, and wait
-- for the result. The provided default value will be returned in the case of an
-- error.
postX11RequestSyncProp :: MonadUnliftIO m => X11PropertyT m a -> a -> X11PropertyT m a
postX11RequestSyncProp prop a = withRunInIO $ \run ->
  postX11RequestSyncDef a (run prop)

-- | 'X11Property' which reflects whether or not the provided 'RROutput' is active.
isActiveOutput :: XRRScreenResources -> RROutput -> X11Property Bool
isActiveOutput sres output = do
  display <- getDisplay
  maybeOutputInfo <- liftIO $ xrrGetOutputInfo display sres output
  return $ maybe 0 xrr_oi_crtc maybeOutputInfo /= 0

-- | Return all the active RANDR outputs.
getActiveOutputs :: X11Property [RROutput]
getActiveOutputs = do
  X11Context{..} <- ask
  liftIO (xrrGetScreenResources ctxDisplay ctxRoot) >>= \case
    Just sres -> filterM (isActiveOutput sres) (xrr_sr_outputs sres)
    Nothing -> return []

-- | Get the index of the primary monitor as set and ordered by Xrandr.
getPrimaryOutputNumber :: X11Property (Maybe Int)
getPrimaryOutputNumber = do
  X11Context{..} <- ask
  primary <- liftIO $ xrrGetOutputPrimary ctxDisplay ctxRoot
  outputs <- getActiveOutputs
  return $ primary `elemIndex` outputs

-- | Move the given 'X11Window' to the bottom of the X11 window stack.
doLowerWindow :: MonadIO m => X11Window -> X11PropertyT m ()
doLowerWindow window =
  getDisplay >>= liftIO . flip lowerWindow window
