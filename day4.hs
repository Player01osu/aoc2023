import Data.List (intersect, sort)

find _ [] = error "Could not find element"
find a (x:xs)
  | a == x = 0
  | otherwise = 1 + find a xs

splitInput :: String -> ([Int], [Int])
splitInput s =
  let (_, (_:card)) = splitAt (find ':' s) s
      (left, (_:right)) = splitAt (find '|' card) card
      hand = map read $ words left
      winning = map read $ words right
  in (hand, winning)

score 0 = 0
score x = 2 ^ (x - 1)

rle list = go list
  where
    go [] = []
    go ((n, x):xs) = (n, x):(go $ take x $ drop (n + 1) list) <> (go xs)

answerOne s = show $ sum $ map (score . length . (uncurry $ intersect) . splitInput) $ lines s

answerTwo s = show $ length $ rle $ zip [0..] $ map (length . (uncurry $ intersect) . splitInput) $ lines s

main = interact $ answerTwo
