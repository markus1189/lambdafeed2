#! /usr/bin/env nix-shell
#! nix-shell -i "runhaskell --ghc-arg=-threaded --ghc-arg=-Wall +RTS -N -RTS"
#! nix-shell -p 'ghc.withPackages (p: with p; [ feed wreq stm monad-par ])'
#! nix-shell -p exiftool jq imagemagick rawtherapee which
#! nix-shell --pure
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLists #-}

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

import           Events (Event(..), Command(..), CustomEvent(..))
import qualified Events as Events
import           LambdaFeed

data Names = ItemListName | ItemListViewport deriving (Show, Eq, Ord)

data State = State (Brick.BChan.BChan CustomEvent) (Brick.Widgets.List.List Names Item)

myApp :: Brick.App State CustomEvent Names
myApp = Brick.App { Brick.appDraw = \(State _ items) -> [drawListWidget items]
                  , Brick.appChooseCursor = Brick.showFirstCursor
                  , Brick.appHandleEvent = eventHandler
                  , Brick.appStartEvent = startApp
                  , Brick.appAttrMap = const $ myAttrMap
                  }

drawListWidget :: Brick.Widgets.List.List Names Item -> Brick.Types.Widget Names
drawListWidget = Brick.Widgets.Core.viewport ItemListViewport Brick.Types.Vertical
                 . Brick.Widgets.Core.vLimit 20
                 . Brick.Widgets.List.renderList renderListItem True

renderListItem :: p -> Item -> Brick.Types.Widget n
renderListItem _ item = Brick.Widgets.Core.str (show (FeedQuery.getItemTitle item))

startApp :: MonadIO m => State -> m State
startApp s@(State chan _) = do
  liftIO $ Brick.BChan.writeBChan chan Events.fetchFeeds
  return s

myAttrMap :: Brick.AttrMap
myAttrMap = Brick.attrMap Graphics.Vty.defAttr []

eventHandler :: State -> Brick.Types.BrickEvent n CustomEvent -> Brick.Types.EventM Names (Brick.Types.Next State)
eventHandler s (Brick.AppEvent (UserCommand userCmd)) = handleUserCommand s userCmd
eventHandler s (Brick.AppEvent (UserEvent userEvt)) = handleUserEvent s userEvt
eventHandler s (Brick.Types.VtyEvent vtyEvent) = handleVtyEvent s vtyEvent

handleVtyEvent :: State -> Graphics.Vty.Event -> Brick.Types.EventM Names (Brick.Types.Next State)
handleVtyEvent s@(State chan list) e = case e of
  Graphics.Vty.EvKey Graphics.Vty.KEsc [] -> Brick.halt s
  _ -> do
    newList <- Brick.Widgets.List.handleListEvent e list
    Brick.continue (State chan newList)

handleUserCommand :: State -> Command -> Brick.Types.EventM Names (Brick.Types.Next State)
handleUserCommand (State chan _) FetchFeeds = handleFetchFeeds chan

handleFetchFeeds :: Brick.BChan.BChan CustomEvent -> Brick.Types.EventM n (Brick.Types.Next State)
handleFetchFeeds chan = do
  asyncFeeds <- liftIO downloadFeeds
  feedItems <- mkList . concat <$> liftIO (for asyncFeeds wait)
  Brick.continue (State chan feedItems)
  where
    mkList is = Brick.Widgets.List.list ItemListName (Data.Vector.fromList is) 1

handleUserEvent :: State -> Event -> Brick.Types.EventM Names (Brick.Types.Next State)
handleUserEvent = undefined

downloadFeeds :: IO [Async [Item]]
downloadFeeds = do
  urls <- readUrls
  parFor 10 urls $ \url -> do
    eitherFeed <- try @SomeException $ downloadFeedFromUrl url
    return $ case eitherFeed of
      Left _ -> []
      Right (Left _) -> []
      Right (Right feed) -> FeedQuery.getFeedItems feed

main :: IO ()
main = do
  eventChan <- Brick.BChan.newBChan 10
  let initialState = State eventChan (Brick.Widgets.List.list ItemListName [] 1)
  _ <- Brick.customMain
                  (Graphics.Vty.mkVty Graphics.Vty.defaultConfig)
                  (Just eventChan)
                  myApp
                  initialState
  return ()

readUrls :: IO [String]
readUrls = lines <$> readFile "urls"
