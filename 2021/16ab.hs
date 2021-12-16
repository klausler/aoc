import Data.Bits(bit, testBit)
import Data.Char(ord)
fromHex ch = let o = ord ch in if o >= ord 'A' then o - ord 'A' + 10 else o - ord '0'
toBits n = [ fromEnum $ testBit n j | j <- [3,2,1,0] ]
twoPows = iterate (2*) 1
get n bits = let (b, rest) = splitAt n bits in (sum $ zipWith (*) twoPows $ reverse b, rest)
data Packet = Packet Int Int (Either Int [Packet]) -- deriving (Show)
getOnePacket :: [Int] -> (Packet, [Int])
getOnePacket bits = (Packet vers ident payload, rest)
  where
    (vers, r1) = get 3 bits
    (ident, r2) = get 3 r1
    (payload, rest)
      | ident == 4, (lit, r3) <- getLiteral 0 r2 = (Left lit, r3)
      | (0, r3) <- get 1 r2, (nBits, r4) <- get 15 r3, (bits', r5) <- splitAt nBits r4 = (Right $ packetSeq bits', r5)
      | (1, r3) <- get 1 r2, (nPackets, r4) <- get 11 r3, (packets, r5) <- getPackets nPackets r4 = (Right packets, r5)
getLiteral soFar bits
  | n < 16 = (n', rest)
  | otherwise = getLiteral (n' - 16) rest
  where
    (n, rest) = get 5 bits
    n' = soFar * 16 + n
getPackets n bits
  | n == 0 = ([], bits)
  | (pack, r1) <- getOnePacket bits, (more, rest) <- getPackets (n-1) r1 = (pack:more, rest)
packetSeq [] = []
packetSeq bits = let (p,rest) = getOnePacket bits in p : packetSeq rest
part1Score (Packet vers _ (Left _)) = vers
part1Score (Packet vers _ (Right subs)) = vers + sum (part1Score <$> subs)
evaluate (Packet _ 0 (Right ps)) = sum $ evaluate <$> ps
evaluate (Packet _ 1 (Right ps)) = product $ evaluate <$> ps
evaluate (Packet _ 2 (Right ps)) = minimum $ evaluate <$> ps
evaluate (Packet _ 3 (Right ps)) = maximum $ evaluate <$> ps
evaluate (Packet _ 4 (Left n)) = n
evaluate (Packet _ 5 (Right [p1,p2])) = fromEnum $ evaluate p1 > evaluate p2
evaluate (Packet _ 6 (Right [p1,p2])) = fromEnum $ evaluate p1 < evaluate p2
evaluate (Packet _ 7 (Right [p1,p2])) = fromEnum $ evaluate p1 == evaluate p2
main = do
  hex <- (concat . lines) <$> readFile "in/16.txt"
  let bits = hex >>= (toBits . fromHex)
      (top, _) = getOnePacket bits
  print $ part1Score top -- part 1
  print $ evaluate top -- part 2
