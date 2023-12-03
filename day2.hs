import Data.List.Split (splitOn)

type Id = Int
type Reds = Int
type Greens = Int
type Blues = Int

type Game = (Id, [Bag])

type Bag = (Reds, Greens, Blues)

validBag :: Bag -> Bool
validBag (reds, greens, blues) = reds <= 12 && greens <= 13 && blues <= 14

valid :: Game -> Bool
valid (_, bags) = all validBag bags

parseBag :: String -> Bag
parseBag str = go (0, 0, 0) (words $ filter (/= ',') str)
  where
    go acc [] = acc
    go (red, green, _) (n:"blue":xs) = go (red, green, read n) xs
    go (red, _, blue) (n:"green":xs) = go (red, read n, blue) xs
    go (_, green, blue) (n:"red":xs) = go (read n, green, blue) xs

gameId (id, _) = id

parse :: String -> Game
parse s =
  let id = read $ last $ words $ takeWhile (/= ':') s
      bags = map parseBag $ splitOn ";" $ drop 2 $ dropWhile (/= ':') s
  in (id, bags)

findOpt :: Game -> Bag
findOpt (_, bags) =
  (maxColor getRed, maxColor getGreen, maxColor getBlue)
  where
    maxColor f = maximum $ map f $ bags
    getRed (x, _, _) = x
    getGreen (_, x, _) = x
    getBlue (_, _, x) = x

power (reds, greens, blues) = reds * greens * blues

answerOne :: [String] -> String
answerOne games = show $ sum $ map gameId $ filter valid $ map parse games

answerTwo :: [String] -> String
answerTwo games = show $ sum $ map (power . findOpt) $ map parse games

main = interact $ answerTwo . lines
