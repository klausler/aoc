import Data.Bits
import qualified Data.Map as M
run mem last ip (a,b,c)
  | ip >= last = []
  | opc == 0 = run mem last (ip+2) (a `shiftR` combo, b, c) -- ADV
  | opc == 6 = run mem last (ip+2) (a, a `shiftR` combo, c) -- BDV
  | opc == 7 = run mem last (ip+2) (a, b, a `shiftR` combo) -- CDV
  | opc == 1 = run mem last (ip+2) (a, b `xor` opd, c) -- BXL
  | opc == 2 = run mem last (ip+2) (a, combo `mod` 8, c) -- BST
  | opc == 3 = run mem last (if a == 0 then ip+2 else opd) (a, b, c) -- JNZ
  | opc == 4 = run mem last (ip+2) (a, b `xor` c, c) -- BXC
  | opc == 5 = (combo `mod` 8) : run mem last (ip+2) (a, b, c) -- OUT
  where
    opc = mem M.! ip
    opd = mem M.! (ip + 1)
    combo
      | opd < 4 = opd
      | opd == 4 = a
      | opd == 5 = b
      | opd == 6 = c
main = do
  [ra,rb,rc,_,p] <- lines <$> readFile "in/17.txt"
  let a = read $ last $ words ra :: Int
      b = read $ last $ words rb :: Int
      c = read $ last $ words rc :: Int
      prog = read $ '[' : (last $ words p) ++ "]"
      mem = M.fromList $ zip [0..] prog :: M.Map Int Int
  putStrLn $ tail $ init $ show $ run mem (length prog) 0 (a,b,c) -- part 1
