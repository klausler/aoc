import qualified Data.Map as M
import qualified Data.Set as S
parse ln = (w1, map init (init items) ++ [last items])
  where (w1:"->":items) = words ln
data Module = Broadcaster [String] | FlipFlop [String] Bool | Nand [String] (M.Map String Bool) | Receiver Bool
fixName ('%':name) = ('%', name)
fixName ('&':name) = ('&', name)
fixName name = (' ', name) -- broadcaster
makeModule _ _ ' ' tos = Broadcaster tos
makeModule _ _ '%' tos = FlipFlop tos False
makeModule name nandsFroms '&' tos = Nand tos $ M.fromList [ (from,False) | from <- nandsFroms M.! name ]
step rxFrom state (from,at,isHigh)
  | Broadcaster tos <- mod = (state, [(at,to,isHigh) | to<-tos], [])
  | FlipFlop tos was <- mod, isHigh = (state, [], [])
  | FlipFlop tos was <- mod = (M.insert at (FlipFlop tos $ not was) state,
                               [(at,to,not was)|to<-tos], [])
  | Nand tos ist <- mod =
      let ist' = M.insert from isHigh ist
          nowFull = and $ M.elems ist'
      in (M.insert at (Nand tos ist') state,
          [(at,to,not $ and $ M.elems ist') | to<-tos],
          [ from | at == rxFrom, isHigh])
  | Receiver _ <- mod, isHigh = (state, [], [])
  | Receiver _ <- mod = (M.insert at (Receiver False) state, [], [])
  where mod = state M.! at
simulate rxFrom state lows highs log [] = (state,(lows,highs),log)
simulate rxFrom state lows highs log ((p@(from,to,isHigh)):ps) =
    simulate rxFrom state' (if isHigh then lows else lows+1)
      (if isHigh then highs+1 else highs) (log'++log) (ps++nps)
  where (state',nps,log') = step rxFrom state p
firstPulse = [("", "broadcaster", False)]
push state lows highs 0 = (state,(lows,highs))
push state lows highs pushes = push state' lows' highs' (pushes-1)
  where (state',(lows',highs'),_) = simulate "" state lows highs [] firstPulse
getLog rxFrom clock state = [(clock,x)|x<-reverse log'] ++ getLog rxFrom (clock+1) state'
  where (state',_,log') = simulate rxFrom state 0 0 [] firstPulse
main = do
  parsed <- (map parse . lines) <$> readFile "in/20.txt"
  let nands = S.fromList
        [ name' | (name,_) <- parsed, let (ch,name') = fixName name, ch == '&' ]
      nandsFroms = M.fromListWith (++)
        [ (to, [snd $ fixName name])
        | (name, tos) <- parsed, to <- tos, to `S.member` nands ]
      state0 = M.fromList $
        ("rx", Receiver True) :
          [ (name', makeModule name' nandsFroms ch tos)
          | (name,tos) <- parsed, let (ch,name') = fixName name ]
      [rxFrom] = [ snd $ fixName name | (name,tos) <- parsed, tos==["rx"] ]
      Nand ["rx"] rxFroms = state0 M.! rxFrom
      log = getLog rxFrom 0 state0
  print $ let (_,(lows,highs)) = push state0 0 0 1000 in lows * highs -- part A
  print $ product [ end-start | f <- M.keys rxFroms -- part B
                  , let (start:end:_) = [ clock | (clock,lf)<-log, lf==f ] ]
