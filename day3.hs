import Data.Char (isDigit)
import Debug.Trace (traceShowId)
import qualified Data.Map.Strict as M

type Row = Int
type Col = Int
type Coord = (Row, Col)
type Gear = [Int]
type Gears = M.Map (Row, Col) Gear

checkLine len idx s = any isSymbol $ take len $ drop (idx - 1) s

isSurround l len idx = any (checkLine len idx) l

find :: Eq a => (a -> Bool) -> [a] -> [a]
find _ [] = []
find f (x:xs)
  | f x = x:find f xs
  | otherwise = find f xs

trimLine len col l = take len $ drop (col - 1) l

surroundGear :: [(Row, String)] -> Int -> Col -> [Coord]
surroundGear l len col = concat $ map allGears l
  where
    allGears :: (Row, String) -> [(Row, Col)]
    allGears (row, s) = map (\(x, _) -> (row, x)) $ find (isSymbol . snd) $ trimLine len col $ zip [0..] s

isSymbol c = not (isDigit c || c == '.')

isGear c = c == '*'

checkLineGear len idx s = any isGear $ take len $ drop (idx - 1) s

insertGear :: Coord -> Int -> Gears -> Gears
insertGear coord gear gears = M.insertWith (<>) coord [gear] gears

filterGears :: Gears -> [Gear]
filterGears gears = filter ((==) 2 . length) $ M.elems $ gears

foldGears :: [Gear] -> Int
foldGears gearList = sum $ map (foldl1 (*)) gearList

-- .123. == 5
padLength s len col
  = (length $ takeWhile (not . isDigit) $ drop (col - 1) s)
  + len
  + (length $ take 1 $ drop (col + len) s)

collectLine l@(currentLine:_) = go (zip [0..] currentLine) []
  where
    go :: [(Int, Char)] -> [Int] -> [Int]
    go [] acc = acc
    go ((col, c):cursor) acc
      | isDigit c =
          let numString = c:(takeWhile isDigit (map snd cursor))
              rest = dropWhile (\(_, x) -> isDigit x) cursor
              len = padLength currentLine (length numString) col
              n = read numString
          in if isSurround l len col
            then go rest (n:acc)
            else go rest acc
      | otherwise = go cursor acc

collectLineGear :: [(Row, String)] -> Gears -> Gears
collectLineGear l@((currentRow, currentLine):_) gears = go (zip [0..] currentLine) gears
  where
    go :: [(Col, Char)] -> Gears -> Gears
    go [] acc = acc
    go ((col, c):cursor) acc
      | isDigit c =
          let numString = c:(takeWhile isDigit (map snd cursor))
              rest = dropWhile (\(_, x) -> isDigit x) cursor
              len = padLength currentLine (length numString) col
              n = read numString
              gears = surroundGear l len col
          in go rest (foldr (\b a -> insertGear b n a) acc gears)
      | otherwise = go cursor acc

collect :: [String] -> [Int]
collect (a:list@(b:c:_)) = go list ((collectLine [b, a, c]) <> (collectLine [a, b]))
  where
    go [] acc = acc
    go [curr] acc = (acc <> (collectLine [curr]))
    go [prev,curr] acc = (acc <> (collectLine [curr, prev]))
    go (prev:xs@(curr:next:_)) acc = go xs (acc <> (collectLine [curr, prev, next]))

collectGears :: [String] -> [Gear]
collectGears (a:list@(b:c:_)) = go (zip [1..] list) (collectLineGear [(1, b), (0, a), (2, c)] $ collectLineGear [(0, a), (1, b)] M.empty)
  where
    go :: [(Row, String)] -> Gears -> [Gear]
    go [] acc = filterGears acc
    go [curr] acc = filterGears $ collectLineGear [curr] acc
    go [prev,curr] acc = filterGears $ collectLineGear [curr, prev] acc
    go (prev:xs@(curr:next:_)) acc = go xs (collectLineGear [curr, prev, next] acc)

answerOne s = show $ sum $ collect $ lines s

answerTwo s = show $ foldGears $ collectGears $ lines s

main = interact $ answerTwo
