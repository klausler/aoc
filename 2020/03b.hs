doit txt rs cs = hits
  where
    lns = lines txt
    rows = length lns
    cols = length $ head lns
    isTree r c = (lns !! r) !! (c `mod` cols) == '#'
    hits = length [ () | j <- [0..rows-1], j*rs < rows, isTree (j*rs) (j*cs) ]

main = do
  txt <- readFile "input03.txt"
  let hits = [ doit txt rs cs | (rs, cs) <- [ (1,1), (1,3), (1,5), (1,7), (2,1) ]]
  print hits
  print $ foldr1 (*) hits
