module HAYS.Task.Types
    ( OnError
    , ToIO
    ) where

-- ** ToIO

type ToIO taskConfig m a = taskConfig -> m a -> IO a

-- ** OnError

type OnError error' a = error' -> a
