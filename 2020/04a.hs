import Data.List((\\))

nlToSpace ('\n':'\n':rest) = '\n':nlToSpace rest
nlToSpace ('\n':rest) = ' ':nlToSpace rest
nlToSpace (x:rest) = x:nlToSpace rest
nlToSpace "" = ""

field x = (f, nc v)
  where
    (f,v) = break (==':') x
    nc (':':rest) = rest
    nc x = x

fieldNames = [ "byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid", "cid" ]
optional = ["cid"]
required = fieldNames \\ optional

isValid rec = (all (`elem` fieldNames) fs) && (all (`elem` fs) required)
  where
    fs = map fst $ map field $ words rec

main = do
  txt <- readFile "input04.txt"
  let
    recs = lines $ nlToSpace txt
    valid = filter isValid recs
  print $ length valid
