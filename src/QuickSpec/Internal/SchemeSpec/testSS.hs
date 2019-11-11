import QuickSpec.Internal
import QuickSpec.Internal.Type hiding (defaultTo)
import QuickSpec.Internal.Term
import QuickSpec.Internal.SchemeSpec

mySig = [
  con "reverse" (reverse :: [A] -> [A]),
  con "++" ((++) :: [A] -> [A] -> [A]),
  con "[]" ([] :: [A]),
  con "map" (map :: (A -> B) -> [A] -> [B]),
  con "length" (length :: [A] -> Int),
  con "concat" (concat :: [[A]] -> [A]),
  schema "" "?F(?G(A),?H(A)) = ?F(?H(A),?G(A))",
  schema "" "?F(A) = ?G(A)",
  schema "" "?F(A) = ?F(?F(A))",
  schema "commutative" "?F(X,Y) = ?F(Y,X)"]

tc = makeConfig mySig

main = schemeSpec tc
