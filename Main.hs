#! /usr/bin/env nix-shell
#! nix-shell -i "runhaskell --ghc-arg=-threaded --ghc-arg=-Wall"
#! nix-shell -p 'ghc.withPackages (p: with p; [ feed wreq stm ])'
#! nix-shell -p exiftool jq imagemagick rawtherapee which
#! nix-shell --pure

import Control.Monad (when, forever, replicateM_)
import Control.Lens
import qualified Network.Wreq as Wreq
import qualified Text.Feed.Query as FeedQuery
import qualified Text.Feed.Import as FeedImport
import Text.Feed.Types (Feed)
import Data.Foldable (for_)
import qualified Data.Text.IO as TIO
import Control.Concurrent.STM (STM, atomically, retry)
import Control.Concurrent.STM.TBQueue (TBQueue, newTBQueue, writeTBQueue, readTBQueue, isEmptyTBQueue, flushTBQueue)
import Control.Concurrent.Async (Async, async, wait)

data Work a = Done | Work a deriving (Show, Eq)

newtype Url = Url String deriving (Show, Eq)

data ResultLog = ResultLog (TBQueue (Work Feed))

newResultLog :: Int -> IO ResultLog
newResultLog n = atomically (ResultLog <$> newTBQueue n)

data WorkLog = WorkLog (TBQueue (Work Url))

newWorkLog :: Int -> IO WorkLog
newWorkLog n = atomically (WorkLog <$> newTBQueue n)

readWork :: WorkLog -> STM (Work Url)
readWork (WorkLog q) = readTBQueue q

isEmptyWorkLog :: WorkLog -> STM Bool
isEmptyWorkLog (WorkLog q) = isEmptyTBQueue q

putWork :: WorkLog -> Url -> STM ()
putWork (WorkLog q) u = writeTBQueue q (Work u)

spawnWorker :: TBQueue Feed -> WorkLog -> IO (Async ())
spawnWorker out wl = async loop
  where loop = do
          w <- atomically (readWork wl)
          case w of
            Done ->
              writeTBQueue out Done
              return ()
            Work (Url url) -> do
              rbody <- Wreq.get url <&> view Wreq.responseBody
              let Just feed = FeedImport.parseFeedSource rbody
              atomically (writeTBQueue out feed)
              loop

main :: IO ()
main = do
  wl <- newWorkLog 2
  output <- atomically (newTBQueue 5)
  workers <- replicateM_ 10 (spawnWorker output wl)
  l <- async loop
  for_ urls (atomically . putWork wl)
  wait l

  where loop = do
          feeds <- atomically $ do
            work <$> readTBQueue output
            -- empty <- isEmptyTBQueue output
            -- when empty retry
            -- flushTBQueue output
            case work of
              
            putStrLn "####################"
            TIO.putStrLn (FeedQuery.getFeedTitle feed)
            print (length $ FeedQuery.getFeedItems feed)
            putStrLn "####################"

urls = map Url ["https://www.reddit.com/r/haskell/.rss"
               ,"https://www.reddit.com/r/scala/.rss"
               ,"https://www.reddit.com/r/notebooks/.rss"
               ,"https://www.reddit.com/r/fantasy/.rss"
               ,"https://mathwithbaddrawings.com/feed"
               ,"http://blog.kubernetes.io/feeds/posts/default"
               ,"https://photographylife.com/feed"
               ,"http://calnewport.com/blog/feed"
               ,"http://www.beyondthelines.net/feed/"
               ,"http://waitbutwhy.com/feed"
               ,"https://failex.blogspot.com/feeds/posts/default"
               ,"https://thegeocachingjunkie.com/feed/"
               ,"https://www.geocaching.com/blog/feed/"
               ,"http://www.locusmap.eu/feed/"
               ,"http://nixos.org/blogs.xml"
               ,"https://www.reddit.com/r/NixOS/.rss"
               ,"http://www.drmaciver.com/blog/feed/"
               ,"https://feeds.feedburner.com/datastax"
               ,"http://www.lihaoyi.com/feed.xml"
               ,"http://feeds.feedburner.com/incodeblog"
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
               ,"http://feeds.feedburner.com/BlackCover"
               ]
