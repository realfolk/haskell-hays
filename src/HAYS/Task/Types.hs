module HAYS.Task.Types
    ( ToIO
    ) where

-- ** ToIO

type ToIO taskConfig m a = taskConfig -> m a -> IO a
