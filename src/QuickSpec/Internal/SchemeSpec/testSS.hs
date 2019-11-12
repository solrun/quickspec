import QuickSpec.Internal
import QuickSpec.Internal.Type hiding (defaultTo)
import QuickSpec.Internal.Term
import QuickSpec.Internal.SchemeSpec
import qualified QuickSpec.Internal.Haskell as Haskell
import QuickSpec.Internal.SchemeSpec.PropGen

mySig = [
  con "reverse" (reverse :: [A] -> [A]),
  con "++" ((++) :: [A] -> [A] -> [A]),
  con "[]" ([] :: [A]),
  con "map" (map :: (A -> B) -> [A] -> [B]),
  con "length" (length :: [A] -> Int),
  con "concat" (concat :: [[A]] -> [A])
  ,schema "distributive" "?F(?G(X)) = ?G(?F(X))"--
  ,schema "2-distributive" "?F(?G(X),?G(Y)) = ?G(?F(X,Y))"
  ,schema "commutative" "?F(?G(X,Y)) = ?F(?G(Y,X))"
  --schema "" "?F(?G(A),?H(A)) = ?F(?H(A),?G(A))",
  --schema "" "?F(A) = ?G(A)",
  --schema "" "?F(A) = ?F(?F(A))",
  --schema "commutative" "?F(X,Y) = ?F(Y,X)"
  ]

tc = makeConfig mySig

main = do
  let scs = Haskell.cfg_schemas tc
      cs = concat $ Haskell.cfg_constants tc
      ps = schemaProps (snd (scs !! 2)) cs
      comlen = ps !! 2
  schemeSpec tc
