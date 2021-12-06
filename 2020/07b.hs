import Data.List(isPrefixOf)
import Data.Traversable(traverse)
import qualified Data.Map as M
parseRec ln = ((adj ++ " " ++ color), f items)
  where
    (adj:color:"bags":"contain":items) = words ln
    f [] = []
    f ["no", "other", "bags."] = []
    f (ct:adj:color:b:rest)
      | "bag" `isPrefixOf` b = (read ct, adj ++ " " ++ color):f rest
assess done [] = done
assess done rest = uncurry assess $ f rest
  where
    f [] = (done, [])
    f ((k,xs):rest)
      | null xs = (M.insert k 1 yes, no)
      | Just ns <- traverse g xs = (M.insert k (1 + sum ns) yes, no)
      | otherwise = (yes, (k,xs):no)
      where
        (yes,no) = f rest
    g (ct, x) = fmap (ct *) $ M.lookup x done
main = do
  txt <- readFile "in/07.txt"
  print $ fmap (subtract 1) $ M.lookup "shiny gold" $ assess M.empty $ map parseRec $ lines txt
