import BitStream
import Data.Bits(testBit)
import Data.Char(ord)
fromHex ch = let o = ord ch in if o >= ord 'A' then o - ord 'A' + 10 else o - ord '0'
toBits n = [ fromEnum $ testBit n j | j <- [3,2,1,0] ]
data Packet = Packet Int Int (Either Int [Packet]) -- deriving (Show)
literal soFar = do
  group <- getInt 5
  let n = soFar * 16 + group
  if group < 16 then pure n else literal (n - 16)
onePacket = do
  vers <- getInt 3
  op <- getInt 3
  payload <- if op == 4 then Left <$> literal 0 else do
    oneBit <- getInt 1
    Right <$> (if oneBit == 0 then do
                 n <- getInt 15
                 got <- getBits n
                 pure $ fst $ runBits allPackets got
               else do
                 n <- getInt 11
                 sequence $ take n $ repeat onePacket)
  pure $ Packet vers op payload
allPackets = do
  done <- isEmpty
  if done then pure [] else do
    packet <- onePacket
    (packet :) <$> allPackets
part1Score (Packet vers _ payload) = vers + either (const 0) (sum . (part1Score <$>)) payload
evaluate (Packet _ op (Right ps)) | op <= 3 = [sum, product, minimum, maximum] !! op $ evaluate <$> ps
evaluate (Packet _ 4 (Left n)) = n
evaluate (Packet _ op (Right [p1,p2])) = fromEnum $ ([(>), (<), (==)] !! (op - 5)) (evaluate p1) (evaluate p2)
main = do
  hex <- (concat . lines) <$> readFile "in/16.txt"
  let top = fst $ runBits onePacket $ hex >>= (toBits . fromHex)
  print $ part1Score top -- part 1
  print $ evaluate top -- part 2
