import Data.Char(isDigit)
import Data.Maybe(fromJust, isJust)
toNum c = if isDigit c then c else if c == '-' then '-' else ' '
fling xMin xMax yMin yMax x' y' x y
  | x > xMax = Nothing
  | y < yMin = Nothing
  | x >= xMin, y <= yMax = Just [(x,y)]
  | Just rest <- fling xMin xMax yMin yMax ((x' - 1) `max` 0) (y' - 1) (x + x') (y + y') = Just ((x,y) : rest)
  | otherwise = Nothing
main = do
  ln <- readFile "in/17.txt"
  let [xMin, xMax, yMin, yMax] = (read <$> (words $ toNum <$> ln)) :: [Int]
      hits = [ fromJust p | x' <- [1 .. xMax], y' <- [yMin .. (-10)*yMin]
                          , let p = fling xMin xMax yMin yMax x' y' 0 0, isJust p ]
  print $ maximum $ snd <$> concat hits -- part 1
  print $ length hits -- part 2
