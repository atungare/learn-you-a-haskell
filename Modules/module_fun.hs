import Data.List
import Data.Char

caesar :: Int -> String -> String
caesar shift input = map (chr . (+ shift) . ord) input

uncaesar :: Int -> String -> String
uncaesar shift = caesar $ negate shift
