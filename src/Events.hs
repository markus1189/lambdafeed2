module Events (Event(..)
              ,Command(..)
              ,CustomEvent(..)

              ,fetchFeeds
              ,feedFetched
              ) where

import Text.Feed.Types (Feed)

data Command = FetchFeeds

data Event = FeedFetched (Either String Feed)

data CustomEvent = UserCommand Command | UserEvent Event

fetchFeeds :: CustomEvent
fetchFeeds = UserCommand FetchFeeds

feedFetched :: Either String Feed -> CustomEvent
feedFetched r = UserEvent (FeedFetched r)
