import qualified Data.Set as S
decode 'R' = (1,0)
decode 'L' = (-1,0)
decode 'U' = (0,1)
decode 'D' = (0,-1)
genMoves [[d],ct] = replicate (read ct) (decode d)
headStep (hx,hy) (mx,my) = (hx+mx,hy+my)
follow (tx,ty) (hx,hy)
  | abs(tx-hx)<=1, abs(ty-hy)<=1 = (tx,ty)
  | otherwise = (tx+signum(hx-tx),ty+signum(hy-ty))
main = do
  lns <- lines <$> readFile "in/09.txt"
  let moves = lns >>= genMoves.words
      trip0 = scanl headStep (0,0) moves
  print $ S.size $ S.fromList $ iterate (scanl follow (0,0)) trip0 !! 1 -- part A
  print $ S.size $ S.fromList $ iterate (scanl follow (0,0)) trip0 !! 9 -- part B
