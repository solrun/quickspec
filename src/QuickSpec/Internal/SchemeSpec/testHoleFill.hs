import QuickSpec.Internal
import QuickSpec.Internal.Haskell
import QuickSpec.Internal.Term
import QuickSpec.Internal.SchemeSpec.PropGen

tc = makeConfig testSig
sp = snd $ head $ cfg_schemas tc
cs = concat $ cfg_constants tc
spair = schemaSides sp
l = fst spair
fs = findFillings spair cs
frev = head fs
--c = canFill frev l

main = return ()
