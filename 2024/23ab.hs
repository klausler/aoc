import qualified Data.Map as M
import qualified Data.Set as S
expand :: M.Map String (S.Set String) -> S.Set String -> S.Set (S.Set String)
expand nmap clique
  | S.null clique = S.empty
  | otherwise = S.fromList [ S.union clique $ S.singleton f | f <- S.toList frontier ]
  where frontier = foldr1 S.intersection [ nmap M.! x | x <- S.toList clique ]
grow nmap cliques = S.unions $ expand nmap <$> S.toList cliques
largest :: M.Map String (S.Set String) -> S.Set (S.Set String) -> S.Set String
largest nmap cliques
  | S.size cliques == 1 = head $ S.toList cliques
  | otherwise = largest nmap $ grow nmap cliques
main = do
  lns <- lines <$> readFile "in/23.txt"
  let pairs = [ ((a1:[a2]),(b1:[b2])) | (a1:a2:'-':b1:b2:"") <- lns ]
      nmap = M.fromListWith S.union $ concat
        [ [ (a,S.singleton b), (b,S.singleton a)] | (a,b) <- pairs ]
      twoCliques = S.fromList [ S.fromList [a,b] | (a,bs) <- M.assocs nmap, b <- S.toList bs ]
      threeCliques = grow nmap twoCliques
  print $ sum [ 1 | c <- S.toList threeCliques
                  , let xs = S.toList c
                  , any (('t'==).head) $ S.toList c ] -- part 1
  putStrLn $ tail $ concat $ fmap (',':) $ S.toList $ largest nmap threeCliques -- part 2
