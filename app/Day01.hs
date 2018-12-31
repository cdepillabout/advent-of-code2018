module Main where

-- common imports that are often used
import Control.Lens
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Text (Text)
import qualified Data.Text as Text

-- you can import from modules defined in src/
import Lib (someFunc)


main :: IO ()
main = interact mySolution

mySolution :: String -> String
mySolution = id
