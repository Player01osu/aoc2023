import Data.Char (isDigit)
import Data.List (isPrefixOf)
import Debug.Trace (traceShowId)

takeHeadLast (x:[]) = [x, x]
takeHeadLast (x:xs) = [x, last xs]

toString [] = []
toString s@(c:cs)
  | isDigit c            = c:(toString cs)
  | isPrefixOf "one" s   = '1':(toString (drop 3 s))
  | isPrefixOf "two" s   = '2':(toString (drop 3 s))
  | isPrefixOf "three" s = '3':(toString (drop 5 s))
  | isPrefixOf "four" s  = '4':(toString (drop 4 s))
  | isPrefixOf "five" s  = '5':(toString (drop 4 s))
  | isPrefixOf "six" s   = '6':(toString (drop 3 s))
  | isPrefixOf "seven" s = '7':(toString (drop 5 s))
  | isPrefixOf "eight" s = '8':(toString (drop 5 s))
  | isPrefixOf "nine" s  = '9':(toString (drop 4 s))
  | isPrefixOf "zero" s  = '0':(toString (drop 4 s))
  | otherwise            = toString cs

takeDigits s = foldr (\x acc -> if isDigit x then x:acc else acc) [] s

answerOne s = sum $ map (read . takeHeadLast . takeDigits) $ lines $ s

answerTwo s = sum $ map (read . takeHeadLast . toString) $ lines $ s

main = interact $ show . answerOne
