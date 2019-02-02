module Main where

import Control.Applicative ()
import Control.Lens
import Control.Monad (void)
import Control.Monad.ST
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Char (ord)
import Data.Foldable (traverse_)
import qualified Data.List as List
import Data.Map (Map, insertWith, toList)
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, pack)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Vector
import qualified Data.Vector as Vector
import Data.Vector.Mutable
import qualified Data.Vector.Mutable as MVector
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Debug.Trace

main :: IO ()
main = pure ()
