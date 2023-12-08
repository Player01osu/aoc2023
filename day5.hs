import Debug.Trace (traceShowId)
import Data.List (lookup)

input = unlines $ ["seeds: 79 14 55 13",
        "",
        "seed-to-soil map:",
        "50 98 2",
        "52 50 48",
        "",
        "soil-to-fertilizer map:",
        "0 15 37",
        "37 52 2",
        "39 0 15",
        "",
        "fertilizer-to-water map:",
        "49 53 8",
        "0 11 42",
        "42 0 7",
        "57 7 4",
        "",
        "water-to-light map:",
        "88 18 7",
        "18 25 70",
        "",
        "light-to-temperature map:",
        "45 77 23",
        "81 45 19",
        "68 64 13",
        "",
        "temperature-to-humidity map:",
        "0 69 1",
        "1 0 69",
        "",
        "humidity-to-location map:",
        "60 56 37",
        "56 93 4"]

findPred f [] = 0
findPred f (x:xs)
  | f x = 0
  | otherwise = 1 + (findPred f xs)

find a list = findPred ((==) a) list

splitAtElem a list = splitAt (find a list) list

tupleMap (x:y:z:_) = (x,y,z)

splitMaps :: [String] -> [[(Int, Int, Int)]]
splitMaps [] = []
splitMaps s =
  let (rawMap, rest) = splitAtElem "" $ drop 2 s
      finalMap = map (tupleMap . map read . words) $ rawMap
  in finalMap:(splitMaps rest)

splitSeed :: String -> ([Int], String)
splitSeed s =
  let seeds = map read $ words $ drop 1 $ takeWhile (/= '\n') $ dropWhile (/= ':') s
      (_:rest) = dropWhile (/= '\n') s
  in (seeds, rest)

splitSeedRange :: String -> ([Int], String)
splitSeedRange s =
  let seeds = rangeCreate $ map read $ words $ drop 1 $ takeWhile (/= '\n') $ dropWhile (/= ':') s
      (_:rest) = dropWhile (/= '\n') s
  in (seeds, rest)
  where
    rangeCreate :: [Int] -> [Int]
    rangeCreate [] = []
    rangeCreate (a:b:xs) = [a..a + b - 1] <> rangeCreate xs

transformSeed :: [[(Int, Int, Int)]] -> Int -> Int
transformSeed maps seed = go maps seed
  where
    go :: [[(Int, Int, Int)]] -> Int -> Int
    go [] seed = seed
    go (sMap:xs) seed = go xs (fromMap seed sMap)

    fromMap :: Int -> [(Int, Int, Int)] -> Int
    fromMap seed [] = seed
    fromMap seed ((dest, source, range):xs)
      | seed >= source && seed < source + range = seed + (dest - source)
      | otherwise = fromMap seed xs

answerOne :: String -> Int
answerOne s =
  let (seeds, rest) = splitSeed s
      maps = splitMaps $ lines rest
  in minimum $ map (transformSeed maps) seeds

answerTwo :: String -> Int
answerTwo s =
  let (seeds, rest) = splitSeedRange s
      maps = splitMaps $ lines rest
  in minimum $ map (transformSeed maps) seeds

main = interact $ show . answerTwo
