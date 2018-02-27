#! /usr/bin/env nix-shell
#! nix-shell -i "runhaskell --ghc-arg=-threaded --ghc-arg=-Wall +RTS -N -RTS"
#! nix-shell -p 'ghc.withPackages (p: with p; [ feed wreq stm monad-par ])'
#! nix-shell -p exiftool jq imagemagick rawtherapee which
#! nix-shell --pure
{-# LANGUAGE TypeApplications #-}

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (Async, async, wait)
import           Control.Concurrent.QSem
import           Control.Concurrent.STM (STM, atomically, retry)
import           Control.Concurrent.STM.TBQueue (TBQueue, newTBQueue, writeTBQueue, readTBQueue, isEmptyTBQueue, flushTBQueue)
import           Control.Exception.Base (bracket_, finally)
import           Control.Lens
import           Control.Monad (when, forever, replicateM_)
import           Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Par.Class as Par
import           Control.Monad.Par.IO (runParIO)
import           Data.Foldable (for_, traverse_)
import           Data.Monoid ((<>))
import qualified Data.Text.IO as TIO
import           Data.Time.Clock (UTCTime)
import           Data.Traversable (for, traverse)
import qualified Network.Wreq as Wreq
import qualified Text.Feed.Import as FeedImport
import qualified Text.Feed.Query as FeedQuery
import           Text.Feed.Types (Feed)

import LambdaFeed

main :: IO ()
main = do
  asyncs <- parFor 5 urls $ \url -> do
    feedResult <- download url
    case feedResult of
      Left e -> putStrLn e
      Right feed -> do
        TIO.putStrLn (FeedQuery.getFeedTitle feed)
        print (FeedQuery.getFeedHome feed)
        print (length $ FeedQuery.getFeedItems feed)
        -- print (map (FeedQuery.getItemPublishDate @UTCTime) (FeedQuery.getFeedItems feed))
  for_ asyncs wait

urls = ["https://www.reddit.com/r/haskell/.rss"
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
