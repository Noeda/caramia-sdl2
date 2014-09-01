-- | Events for Caramia.
--
-- At the moment, this is just a thin layer of abstraction over the SDL2
-- library.
--

{-# LANGUAGE LambdaCase #-}

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
import Graphics.Rendering.OpenGL.Raw.Core32
import Foreign.Marshal.Array
import Data.Monoid
import Data.Foldable
import Control.Applicative
import Control.Monad

-- | Gets all the events that have happened since the last time it was called
-- or after the initialization of SDL2.
getEvents :: IO [SDL.Event]
getEvents = do
    _ <- currentContextID
    pumpEvents
    (events', num_events) <- allocaArray 200 $ \event_ptr -> do
        result <- fromIntegral <$>
                  peepEvents event_ptr 200
                             SDL.eventActionGetEvent
                             SDL.eventTypeFirstEvent
                             SDL.eventTypeLastEvent
        when (result == -1) throwSDLError
        (,) <$> peekArray result event_ptr <*> return result
    events <- handleOurEvents events'
    if num_events == 200
      then do more_events <- getEvents
              return $ events <> more_events
      else return events

-- | Handle some events ourselves that we don't expect the user to handle.
--
-- Right now the result is always the same as input though; we only want to
-- know if some special events happen.
handleOurEvents :: [SDL.Event] -> IO [SDL.Event]
handleOurEvents events = return events <* (for_ events $ \case
    WindowEvent { windowEventEvent = wevent
                , windowEventData1 = w
                , windowEventData2 = h } |
        wevent == windowEventResized ->
            glViewport 0 0 (fromIntegral w) (fromIntegral h)
    _ -> return ())


