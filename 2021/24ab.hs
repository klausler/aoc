-- In the input are 14 rounds; each is a "push" or a potential "pop" on z.
-- The only way to have z==0 at the end is to pop everything that was pushed.
-- Push rounds look like:
--    w = input
--    if (TOP + A != w) push(w + B)
-- Potential pop rounds look like:
--    w = input
--    if (TOP + A != w) push(w + B) else pop();
-- But the A values on push rounds turn out to not matter, and the input
-- has zeven potential pop rounds that all have to be pops, so their B
-- values are also irrelevant.
-- To make the pop rounds pop, their w+A values must be equal to the w+B
-- value of their corresponding posh round.  The A constants are all
-- negative in pop rounds.  So the input of the pop round = input of the
-- push round plus the pop's A and the push's B.  When that A+B offset is
-- positive, we can maximize the input values by setting the pop round's
-- input to 9; otherwise the push round's input is 9.
-- That solves part 1.  For part 2, minimize the inputs by forcing one
-- of the inputs to 1.
main = do
  print 65984919997939 -- part 1 by inspection
  print 11211619541713 -- part 2 by inspeciion
