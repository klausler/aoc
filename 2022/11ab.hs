import Data.List(sort)
import qualified Data.Map as M
data Monkey = Monkey { items :: [Int], op :: Int->Int, divisor :: Int, yes, no :: Int }
parse [] = []
parse (["Monkey",_]:items:op:div:tm:fm:rest) =
    Monkey worries (fn $ drop 4 op) (read $ last div) (read $ last tm) (read $ last fm) : parse rest
  where worries = read $ '[' : (concat $ drop 2 items) ++ "]"
        arg "old" old = old
        arg n _ = read n
        fn ["*",a] old = old * arg a old
        fn ["+",a] old = old + arg a old
doItem modify monkey state item = M.adjust (\(ct,xs)->(ct,xs++[item'])) dest state
  where item' = modify $ (op monkey) item
        dest = if item' `mod` (divisor monkey) == 0 then yes monkey else no monkey
doMonkey modify monkeys state id = foldl (doItem modify $ monkeys M.! id) state' items
  where (ct,items) = state M.! id
        state' = M.insert id (ct + length items, []) state
doRound modify monkeys state = foldl (doMonkey modify monkeys) state (M.keys monkeys)
business final = product $ take 2 $ reverse $ sort $ fst <$> M.elems final
main = do
  parsed <- (parse . (words <$>) . filter (not.null) . lines) <$> readFile "in/11.txt"
  let monkeys = M.fromList $ zip [0..] parsed
      modulus = product $ divisor <$> monkeys
      start = M.fromList $ zip [0..] $ (0,) <$> (items <$> parsed)
  print $ business $ (iterate (doRound (`div` 3) monkeys) start) !! 20 -- part A
  print $ business $ (iterate (doRound (`mod` modulus) monkeys) start) !! 10000 -- part B
