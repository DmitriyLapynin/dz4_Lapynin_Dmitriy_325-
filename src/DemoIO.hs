module DemoIO where

import Graphics.Gloss.Interface.Pure.Game
import System.Random
import System.IO
import System.Environment
import Data.List
import Data.Char

--------------
-- Data types.
--------------

-- Config for colors.
data ColorConfig = ColorConfig
  { color1 :: Color
  , color2 :: Color
  }

-- General application state.
data AppState = AppState
  { number :: Int -- Random number generator.
  , randomGen :: StdGen -- Current number.
  , colors :: ColorConfig -- Colors config.
  }

-------------
-- Constants.
-------------

-- Random numbers range.
numbersRange :: (Int, Int)
numbersRange = (-10, 10)

-- Path to config file.
configPath :: FilePath
configPath = "config.txt"

-- Game display mode.
display :: Display
display = FullScreen

-- Background color.
bgColor :: Color
bgColor = black

-- Simulation steps per second.
fps :: Int
fps = 60

-- Text shift on screen.
textShift :: Float
textShift = 250

------------------
-- Pure functions.
------------------

-- Parse config from string.
-- Config format: 2 lines, one color per line.
parseConfig :: String -> Maybe ColorConfig
parseConfig str = case map findColor (lines str) of
  [Just c1, Just c2] -> Just $ ColorConfig c1 c2
  _ -> Nothing
  where
    findColor :: String -> Maybe Color
    findColor s = lookup s colorMap
    colorMap = zip names colors
    colors = [red, green, blue, white, yellow]
    names = ["red", "green", "blue", "white", "yellow"]

-- Draw a picture: two numbers of different colors defined in config.
drawApp :: AppState -> Picture
drawApp (AppState n _ (ColorConfig c1 c2)) = Pictures [pic1, pic2]
  where
    pic1 = Color c1 $ Translate (-textShift) 0 txt
    pic2 = Color c2 $ Translate textShift 0 txt
    txt = Text (show n)

-- Handle events.
handleEvent :: Event -> AppState -> AppState
-- Increase number when UP is pressed.
handleEvent (EventKey (SpecialKey KeyUp) Down _ _) state =
  state { number = (number state) + 1 }
-- Decrease number when DOWN is pressed.
handleEvent (EventKey (SpecialKey KeyDown) Down _ _) state =
  state { number = (number state) - 1 }
-- Generate new random number when Space is pressed.
handleEvent (EventKey (SpecialKey KeySpace) Down _ _) (AppState _ r c) =
  -- Get new random number and generator.
  let (newn, newr) = randomR numbersRange r
  -- Update BOTH number AND generator.
  in AppState newn newr c
-- Ignore all other events.
handleEvent (EventKey (MouseButton _) _ _ _) state = 
  state {number = (number state) - (number state)}  
handleEvent _ state = state

-- Simulation step (updates nothing).
updateApp :: Float -> AppState -> AppState
updateApp _ x = x

------------------------------
-- Main function for this app.
------------------------------




-- Run game. This is the ONLY unpure function.
run :: IO ()
run = do
  args <- getArgs
  if length(args) /= 2 then
    putStrLn "Wrong number of args. Expect 2 args"
  else do
    if (not (isDigit(head(head(tail(args))))) ) then 
      putStrLn "The second arg is not number"
    else do
      str <- readFile (head(args))
        -- Try to parse config.
      case parseConfig str of
        Nothing -> putStrLn "Wrong config"
        Just cfg -> do
        -- Get new random number generator (unpure action).
          rndGen <- newStdGen
        -- Run application.
          play display bgColor fps (AppState ((ord(head(head(tail(args))))) - ord('0')) rndGen cfg) drawApp handleEvent updateApp
      
       









 