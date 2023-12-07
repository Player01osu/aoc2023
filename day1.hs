import Data.Char (isDigit, intToDigit)
import Data.List (isPrefixOf)
import Data.Maybe (maybeToList)

takeHeadLast (x:[]) = [x, x]
takeHeadLast (x:xs) = [x, last xs]

toString [] = []
toString s@(c:cs)
  | isDigit c = c:(toString cs)
  | otherwise = (map intToDigit (maybeToList $ find digits s 0)) <> (toString cs)
  where
    digits = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
    find [] _ _ = Nothing
    find (s:xs) a acc
      | isPrefixOf s a = Just acc
      | otherwise      = find xs a (acc + 1)

takeDigits s = foldr (\x acc -> if isDigit x then x:acc else acc) [] s

answerOne s = sum $ map (read . takeHeadLast . takeDigits) $ lines $ s

answerTwo s = sum $ map (read . takeHeadLast . toString) $ lines $ s

main = interact $ show . answerTwo
