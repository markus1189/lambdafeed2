#! /usr/bin/env nix-shell
#! nix-shell -i "runhaskell --ghc-arg=-threaded --ghc-arg=-Wall +RTS -N -RTS"
#! nix-shell -p 'ghc.withPackages (p: with p; [ feed wreq stm monad-par ])'
#! nix-shell -p exiftool jq imagemagick rawtherapee which
#! nix-shell --pure
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Lens.TH (makeClassy)
import qualified Brick
import qualified Brick.BChan
import qualified Brick.Types
import qualified Brick.Util
import qualified Brick.Widgets.Core
import qualified Brick.Widgets.List
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (Async, async, wait)
import           Control.Concurrent.QSem
import           Control.Concurrent.STM (STM, atomically, retry)
import           Control.Concurrent.STM.TBQueue (TBQueue, newTBQueue, writeTBQueue, readTBQueue, isEmptyTBQueue, flushTBQueue)
import           Control.Exception.Base (bracket_, finally, try, SomeException)
import           Control.Lens
import           Control.Monad (when, forever, replicateM_)
import           Control.Monad.IO.Class (liftIO, MonadIO(..))
import qualified Control.Monad.Par.Class as Par
import           Control.Monad.Par.IO (runParIO)
import qualified Data.Default
import           Data.Foldable (for_, traverse_)
import           Data.List (foldl')
import           Data.Monoid ((<>))
import qualified Data.Text.IO as TIO
import           Data.Time.Clock (UTCTime)
import           Data.Traversable (for, traverse)
import qualified Data.Vector
import qualified Graphics.Vty
import qualified Network.Wreq as Wreq
import qualified Text.Feed.Import as FeedImport
import qualified Text.Feed.Query as FeedQuery
import           Text.Feed.Types (Feed, Item)

import           Events (Event(..), Command(..), CustomEvent(..), feedFetched, fetchFeeds)
import qualified Events as Events
import           LambdaFeed

data Names = ItemListName | ItemListViewport deriving (Show, Eq, Ord)

data ModelState = ModelState

data InterfaceState = InterfaceState { _interfaceEvents :: Brick.BChan.BChan CustomEvent
                                     , _interfaceItemsList :: Brick.Widgets.List.List Names Item
                                     }
makeClassy ''InterfaceState

data AppState = AppState ModelState InterfaceState
makeClassy ''AppState

instance HasInterfaceState AppState where
  interfaceState f (AppState m i) = AppState m <$> f i

myApp :: Brick.App AppState CustomEvent Names
myApp = Brick.App { Brick.appDraw = \s -> [drawListWidget (s ^. interfaceItemsList)]
                  , Brick.appChooseCursor = Brick.showFirstCursor
                  , Brick.appHandleEvent = eventHandler
                  , Brick.appStartEvent = startApp
                  , Brick.appAttrMap = const myAttrMap
                  }

drawListWidget :: Brick.Widgets.List.List Names Item -> Brick.Types.Widget Names
drawListWidget = Brick.Widgets.Core.viewport ItemListViewport Brick.Types.Vertical
                 . Brick.Widgets.Core.vLimit 20
                 . Brick.Widgets.List.renderList renderListItem True

renderListItem :: p -> Item -> Brick.Types.Widget n
renderListItem _ item = Brick.Widgets.Core.str (show (FeedQuery.getItemTitle item))

startApp :: MonadIO m => AppState -> m AppState
startApp s@(AppState _ _) = do
  liftIO $ Brick.BChan.writeBChan (s ^. interfaceEvents) Events.fetchFeeds
  return s

myAttrMap :: Brick.AttrMap
myAttrMap = Brick.attrMap Graphics.Vty.defAttr []

eventHandler :: AppState -> Brick.Types.BrickEvent n CustomEvent -> Brick.Types.EventM Names (Brick.Types.Next AppState)
eventHandler s (Brick.AppEvent (UserCommand userCmd)) = handleUserCommand s userCmd
eventHandler s (Brick.AppEvent (UserEvent userEvt)) = handleUserEvent s userEvt
eventHandler s (Brick.Types.VtyEvent vtyEvent) = handleVtyEvent s vtyEvent

handleVtyEvent :: AppState -> Graphics.Vty.Event -> Brick.Types.EventM Names (Brick.Types.Next AppState)
handleVtyEvent s e = case e of
  Graphics.Vty.EvKey Graphics.Vty.KEsc [] -> Brick.halt s
  Graphics.Vty.EvKey (Graphics.Vty.KChar 'q') [] -> Brick.halt s
  _ -> do
    newList <- Brick.Widgets.List.handleListEvent e (s ^. interfaceItemsList)
    Brick.continue (s & interfaceItemsList .~ newList)

handleUserCommand :: AppState -> Command -> Brick.Types.EventM Names (Brick.Types.Next AppState)
handleUserCommand s FetchFeeds = do
  liftIO (async $ handleFetchFeeds (s ^. interfaceEvents))
  Brick.continue s

handleFetchFeeds :: Brick.BChan.BChan CustomEvent -> IO ()
handleFetchFeeds chan = do
  urls <- readUrls
  _ <- parFor 10 urls $ \url -> do
    eitherFeed <- try @SomeException $ downloadFeedFromUrl url
    Brick.BChan.writeBChan chan $ case eitherFeed of
      Left e -> feedFetched (Left (show e))
      Right (Left e) -> feedFetched (Left e)
      Right (Right feed) -> feedFetched (Right feed)
  return ()

handleUserEvent :: AppState -> Event -> Brick.Types.EventM Names (Brick.Types.Next AppState)
handleUserEvent s (FeedFetched (Left _)) = Brick.continue s -- TODO
handleUserEvent s (FeedFetched (Right feed)) = do
  let items = FeedQuery.getFeedItems feed
      newList = foldl' (flip $ Brick.Widgets.List.listInsert 0) (s ^. interfaceItemsList) items
  Brick.continue (s & interfaceItemsList .~ newList)

main :: IO ()
main = do
  eventChan <- Brick.BChan.newBChan 10
  let initialState = AppState ModelState (InterfaceState eventChan (Brick.Widgets.List.list ItemListName [] 1))
  _ <- Brick.customMain
                  (Graphics.Vty.mkVty Graphics.Vty.defaultConfig)
                  (Just eventChan)
                  myApp
                  initialState
  return ()

readUrls :: IO [String]
readUrls = lines <$> readFile "urls"
