main = do
  txt <- readFile "in/03.txt"
  let
    lns = lines txt
    rows = length lns
    cols = length $ head lns
    isTree r c = (lns !! r) !! (c `mod` cols) == '#'
  print $ length [ () | r <- [0..rows - 1], isTree r (3*r) ]
