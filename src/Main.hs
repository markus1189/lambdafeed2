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
import           Control.Monad.IO.Class (liftIO)
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

import           LambdaFeed

data Names = ItemListName | ItemListViewport deriving (Show, Eq, Ord)

data State = State (Brick.BChan.BChan CustomEvents) (Brick.Widgets.List.List Names Item)

data CustomEvents = FetchFeeds

myApp :: Brick.App State CustomEvents Names
myApp = Brick.App { Brick.appDraw = \(State _ items) -> [drawListWidget items]
                  , Brick.appChooseCursor = Brick.showFirstCursor
                  , Brick.appHandleEvent = eventHandler
                  , Brick.appStartEvent = startApp
                  , Brick.appAttrMap = const $ myAttrMap
                  }

drawListWidget = Brick.Widgets.Core.viewport ItemListViewport Brick.Types.Vertical
                 . Brick.Widgets.Core.vLimit 10
                 . Brick.Widgets.List.renderList renderListItem True

renderListItem focused item = Brick.Widgets.Core.str (show (FeedQuery.getItemTitle item))

startApp s@(State chan _) = do
  liftIO $ Brick.BChan.writeBChan chan FetchFeeds
  return s

myAttrMap = Brick.attrMap Graphics.Vty.defAttr []

eventHandler (State chan _) (Brick.AppEvent FetchFeeds) = do
  asyncFeeds <- liftIO downloadFeeds
  feedItems <- mkList . concat <$> (liftIO $ for asyncFeeds wait)
  Brick.continue (State chan feedItems)
  where mkList is = Brick.Widgets.List.list ItemListName (Data.Vector.fromList is) 1

eventHandler s@(State chan list) ev@(Brick.Types.VtyEvent e) = case e of
  Graphics.Vty.EvKey Graphics.Vty.KEsc [] -> Brick.halt s
  _ -> do
    newList <- Brick.Widgets.List.handleListEvent e list
    Brick.continue (State chan newList)

downloadFeeds = parFor 10 urls $ \url -> do
    eitherFeed <- try @SomeException $ downloadFeedFromUrl url
    return $ case eitherFeed of
      Left e -> []
      Right (Left e) -> []
      Right (Right feed) -> FeedQuery.getFeedItems feed

main :: IO ()
main = do
  eventChan <- Brick.BChan.newBChan 10
  let initialState = State eventChan (Brick.Widgets.List.list ItemListName [] 1)
  finalState <- Brick.customMain
                  (Graphics.Vty.mkVty Graphics.Vty.defaultConfig)
                  (Just eventChan)
                  myApp
                  initialState
  return ()

urls :: [String]
urls = ["https://www.reddit.com/r/haskell/.rss"
       ,"https://www.reddit.com/r/scala/.rss"
       ,"https://www.reddit.com/r/notebooks/.rss"
       ,"https://www.reddit.com/r/fantasy/.rss"
       ,"https://mathwithbaddrawings.com/feed"
       ,"http://blog.kubernetes.io/feedItems/posts/default"
       ,"https://photographylife.com/feed"
       ,"http://calnewport.com/blog/feed"
       ,"http://www.beyondthelines.net/feed/"
       ,"http://waitbutwhy.com/feed"
       ,"https://failex.blogspot.com/feedItems/posts/default"
       ,"https://thegeocachingjunkie.com/feed/"
       ,"https://www.geocaching.com/blog/feed/"
       ,"http://www.locusmap.eu/feed/"
       ,"http://nixos.org/blogs.xml"
       ,"https://www.reddit.com/r/NixOS/.rss"
       ,"http://www.drmaciver.com/blog/feed/"
       ,"https://feedItems.feedburner.com/datastax"
       ,"http://www.lihaoyi.com/feed.xml"
       ,"http://feedItems.feedburner.com/incodeblog"
       ,"http://www.martinseeler.com/feed.xml"
       ,"http://www.gridsagegames.com/blog/feed/"
       ,"http://news.ycombinator.com/rss"
       ,"http://www.reddit.com/message/inbox/.rss?feed=ad7158bdbfcb759e26c91793741895526fcd6781&user=markus1189"
       ,"http://www.reddit.com/r/haskell/.rss"
       ,"https://typesandkinds.wordpress.com/feed/"
       ,"http://www.reddit.com/r/emacs/.rss"
       ,"http://www.reddit.com/r/commandline/.rss"
       ,"http://www.reddit.com/r/geocaching/.rss"
       ,"http://www.reddit.com/r/compsci/.rss"
       ,"http://www.reddit.com/r/geb/.rss"
       ,"http://www.reddit.com/r/notebooks/.rss"
       ,"https://www.reddit.com/r/journalingisart/.rss"
       ,"http://www.jetpens.com/blog/feed"
       ,"http://feedItems.feedburner.com/BlackCover"
       ]
