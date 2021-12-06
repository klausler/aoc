import Data.Char(isDigit, isHexDigit, isUpper)
import Data.List((\\), isSuffixOf)
import Data.Maybe(fromMaybe)

nlToSpace ('\n':'\n':rest) = '\n':nlToSpace rest
nlToSpace ('\n':rest) = ' ':nlToSpace rest
nlToSpace (x:rest) = x:nlToSpace rest
nlToSpace "" = ""

field x = (f, nc v)
  where
    (f,v) = break (==':') x
    nc (':':rest) = rest
    nc x = x

year a z x = length x == 4 && all isDigit x && x' >= a && x' <= z
  where x' = read x
height x
    | "cm" `isSuffixOf` x = n >= 150 && n <= 193
    | "in" `isSuffixOf` x = n >= 59 && n <= 76
    | otherwise = False
  where
    n = read $ takeWhile isDigit x
hcolor ('#':xs) = length xs == 6 && all isHexDigit xs && not (any isUpper xs)
hcolor _ = False
ecolor = (`elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])
ppid x = length x == 9 && all isDigit x

fieldDefs = [ ("byr", year 1920 2002), ("iyr", year 2010 2020), ("eyr", year 2020 2030),
              ("hgt", height), ("hcl", hcolor), ("ecl", ecolor),
              ("pid", ppid), ("cid", const True) ]

fieldNames = map fst fieldDefs
optional = ["cid"]
required = fieldNames \\ optional

isValid rec = all isValidField fvs && all (`elem` fs) required
  where
    fvs = map field $ words rec
    fs = map fst fvs
    isValidField (f,v) = (fromMaybe (const False) (lookup f fieldDefs)) v

main = do
  txt <- readFile "in/04.txt"
  let
    recs = lines $ nlToSpace txt
    valid = filter isValid recs
  print $ length valid
