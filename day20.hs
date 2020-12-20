import Data.List
import Data.List as List
import Data.List.Split as Split
import Data.Map as Map
import Data.Set as Set

tileId :: [String] -> Integer
tileId = read . head . List.drop 1 . Split.splitOneOf ": " . head

flipped :: (String, String, String, String) -> (String, String, String, String)
flipped (left, right, top, bot) = (right, left, reverse top, reverse bot)

rotations ::
     (String, String, String, String) -> [(String, String, String, String)]
rotations tile = rotations' tile ++ rotations' (flipped tile)

rotations' ::
     (String, String, String, String) -> [(String, String, String, String)]
rotations' tile@(left, right, top, bot) =
  [ tile -- normal
  , (bot, top, reverse left, reverse right) -- 90 deg
  , (reverse right, reverse left, reverse top, reverse bot) -- 180 deg
  , (reverse top, reverse bot, right, left) -- -90 deg
  ]

edges :: [String] -> (String, String, String, String)
edges tile = (left . List.drop 1 $ tile, right . List.drop 1 $ tile, top, bot)
  where
    left s = List.map (head) s
    right s = List.map (List.last) s
    top = head . List.drop 1 $ tile
    bot = List.last tile

tilesMatch :: [String] -> [String] -> Bool
tilesMatch a b =
  any
    (== True)
    [ True
    | x@(al, ar, at, ab) <- rotations (edges a)
    , y@(bl, br, bt, bb) <- rotations (edges b)
    , ar == bl
    ]

corner :: [String] -> [[String]] -> Bool
corner tile allTiles =
  (== 3) .
  List.length .
  List.map snd .
  List.filter ((== True) . fst) . List.map (\b -> (tilesMatch tile b, tileId b)) $
  allTiles

zipWith2dIndex :: [[a]] -> [((Int, Int), a)]
zipWith2dIndex xss = [((i, j), x) | (j, xs) <- zip [0 ..] xss, (i, x) <- zip [0 ..] xs]

main :: IO ()
main = do
  str <- readFile "input.txt"
  let input = str
  let foo = List.map lines . Split.splitOn "\n\n" $ input
  print $ product . List.map fst . List.filter snd . List.map (\x -> (tileId x, corner x foo)) $ foo
  
  let seaFields = length . List.filter (== '#') . List.concatMap (\t -> [v | ((x,y),v) <- zipWith2dIndex (tail t), x /= 0 && y /= 0 && y /= 9 && x /= 9]) . List.map lines . Split.splitOn "\n\n" $ input
  print $ [seaFields - i * 15 | i <- [32..64]]