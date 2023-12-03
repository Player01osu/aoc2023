import Data.Char (isDigit)
import Data.List (isPrefixOf)

takeHeadTail (x:[]) = [x, x]
takeHeadTail (x:xs) = [x, last xs]

toString [] = []
toString s@(c:cs)
  | isDigit c            = c:(toString cs)
  | isPrefixOf "one" s   = '1':(toString cs)
  | isPrefixOf "two" s   = '2':(toString cs)
  | isPrefixOf "three" s = '3':(toString cs)
  | isPrefixOf "four" s  = '4':(toString cs)
  | isPrefixOf "five" s  = '5':(toString cs)
  | isPrefixOf "six" s   = '6':(toString cs)
  | isPrefixOf "seven" s = '7':(toString cs)
  | isPrefixOf "eight" s = '8':(toString cs)
  | isPrefixOf "nine" s  = '9':(toString cs)
  | isPrefixOf "zero" s  = '0':(toString cs)
  | otherwise            = toString cs

takeDigits s = foldr (\x acc -> if isDigit x then x:acc else acc) [] s

answerOne s = sum $ map (read . takeHeadTail . takeDigits) $ lines $ s

answerTwo s = sum $ map (read . takeHeadTail . toString) $ lines $ s

main = interact $ show . answerOne
