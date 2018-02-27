module LambdaFeed (parFor
                  ,parTraverse
                  ,downloadFeedFromUrl
                  ) where

import           Control.Concurrent.Async (Async, async)
import           Control.Concurrent.QSem
import           Control.Exception.Base (finally)
import           Control.Lens
import           Data.Monoid ((<>))
import           Data.Traversable (for)
import qualified Network.Wreq as Wreq
import qualified Text.Feed.Import as FeedImport
import           Text.Feed.Types (Feed)

downloadFeedFromUrl :: String -> IO (Either String Feed)
downloadFeedFromUrl url = do
  rbody <- Wreq.get url <&> view Wreq.responseBody
  case FeedImport.parseFeedSource rbody of
    Nothing -> return $ Left ("Failed to parse response of url: " <> url)
    Just feed -> return (Right feed)

parTraverse :: Traversable t => Int -> (a -> IO b) -> t a -> IO (t (Async b))
parTraverse n f xs = do
  qsem <- newQSem n
  for xs $ \x -> do
    waitQSem qsem
    async (f x `finally` signalQSem qsem)

parFor :: Traversable t => Int -> t a -> (a -> IO b) -> IO (t (Async b))
parFor n = flip (parTraverse n)

