-- | Events for Caramia.
--
-- At the moment, this is just a thin layer of abstraction over the SDL2
-- library.
--

module Caramia.Events
    (
    -- * Getting events.
      getEvents
    , SDL.Event(..) )
    where

import Caramia.Context
import Caramia.Internal.Error
import Graphics.UI.SDL
import qualified Graphics.UI.SDL as SDL
import Foreign.Marshal.Array
import Data.Monoid
import Control.Applicative
import Control.Monad

-- | Gets all the events that have happened since the last time it was called
-- or after the initialization of SDL2.
getEvents :: IO [SDL.Event]
getEvents = do
    _ <- currentContextID
    pumpEvents
    (events, num_events) <- allocaArray 200 $ \event_ptr -> do
        result <- fromIntegral <$>
                  peepEvents event_ptr 200
                             SDL.eventActionGetEvent
                             SDL.eventTypeFirstEvent
                             SDL.eventTypeLastEvent
        when (result == -1) throwSDLError
        (,) <$> peekArray result event_ptr <*> return result
    if num_events == 200
      then do more_events <- getEvents
              return $ events <> more_events
      else return events

