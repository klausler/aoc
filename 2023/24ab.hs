cook ch = if ch == '\n' || ch == '-' || ch >= '0' && ch <= '9' then ch else ' '
pairs [] = []
pairs (x:xs) = [(x,x') | x' <- xs] ++ pairs xs
inRange v = v >= 200000000000000.0 && v <= 400000000000000.0
solveA :: [Double] -> [Double] -> [(Double,Double)]
solveA [apx,apy,_,avx,avy,_] [bpx,bpy,_,bvx,bvy,_] =
    [ (ta,tb) | avx /= 0, avy /= 0, bvx*avy /= bvy*avx, ta >= 0, tb >= 0
              , all inRange [ax_ta, ay_ta, bx_tb, by_tb] ]
  -- apx + avx * ta = bpx + bvx * tb
  -- apy + avy * ta = bpy + bvy * tb
  -- ta = (bpx + bvx * tb - apx) / avx = (bpx-apx)/avx + (bvx/avx)*tb
  --    = (bpy + bvy * tb - apy) / avy = (bpy-apy)/avy + (bvy/avy)*tb
  -- -> (bvx/avx - bvy/avy)*tb = (bpy-apy)/avy - (bpx-apx)/avx
  where tb = ((bpy-apy)/avy - (bpx-apx)/avx) / (bvx/avx - bvy/avy)
        ta = (bpx + bvx * tb - apx) / avx
        (ax_ta,ay_ta) = (apx + avx * ta, apy + avy * ta)
        (bx_tb,by_tb) = (bpx + bvx * tb, bpy + bvy * tb)
isInt x = fromInteger (truncate x) == x
isPosInt x = x >= 0 && isInt x
isGood [rpx,rpy,rpz,rvx,rvy,rvz] [px,py,pz,vx,vy,vz]
    = rvx /= vx && isPosInt t && rpy + rvy*t == py + vy*t && rpz + rvz*t == pz + vz*t
  where t = (px - rpx) / (rvx - vx)
solveB ([apx,apy,apz,avx,avy,avz]:[bpx,bpy,bpz,bvx,bvy,bvz]:rest) =
    [ soln | rvx <- tries, rvy <- tries, soln <- solutions rvx rvy, all (isGood soln) rest ]
  where
    tries = [-10000..10000]
    solutions rvx rvy
     | avx /= rvx, avy /= rvy, (avy-rvy)*(bvx-rvx) /= (avx-rvx)*(bvy-rvy)
     , isPosInt ta, isPosInt tb, ta /= tb, isInt rvz = [[rpx, rpy, rpz, rvx, rvy, rvz]]
     | otherwise = []
     where rpx = apx + (avx-rvx) * ta -- = bpx + (bvx-rvx) * tb
           rpy = apy + (avy-rvy) * ta -- = bpy + (bvy-rvy) * tb
           ta = (bpx + (bvx-rvx)*tb - apx) / (avx-rvx) -- = (bpy + (bvy-rvy)*tb - apy) / (avy-rvy)
           -- so (avy-rvy)*(bpx + (bvx-rvx)*tb - apx) = (avx-rvx)*(bpy + (bvy-rvy)*tb - apy)
           -- so ((avy-rvy)*(bvx-rvx) - (avx-rvx)*(bvy-rvy)) * tb
           --  = (avy-rvy)*(apx - bpx) + (avx-rvx)*(bpy - apy), so
           tb = ((avy-rvy)*(apx - bpx) + (avx-rvx)*(bpy - apy)) /
                ((avy-rvy)*(bvx-rvx) - (avx-rvx)*(bvy-rvy))
           rpz = apz + (avz-rvz) * ta -- = bpz + (bvz-rvz) * tb, so
           -- so apz + avz*ta - rvz*ta = bpz + bvz*tb - rvz*tb
           -- so rvz * (tb - ta) = bpz + bvz*tb - apz - avz*ta, so
           rvz = (bpz + bvz*tb - apz - avz*ta) / (tb-ta)
main = do
  lns <- fmap (map (map read . words) . lines . map cook) $ readFile "in/24.txt"
  print $ length $ pairs lns >>= uncurry solveA -- part A
  print [ truncate $ rpx + rpy +  rpz | [rpx,rpy,rpz,_,_,_] <- solveB lns] -- part B
