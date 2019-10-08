import QuickSpec.Internal.Parse
import QuickSpec.Internal.Haskell(Constant,defaultConfig)
import QuickSpec.Internal.Term
import QuickSpec.Internal.Prop
import QuickSpec.Internal.Explore.Polymorphic
import qualified Data.Map.Strict as Map
import QuickSpec.Internal
import QuickSpec
testSig = [
  con "reverse" (reverse :: [A] -> [A]),
  con "++" ((++) :: [A] -> [A] -> [A]),
  con "[]" ([] :: [A]),
  con "map" (map :: (A -> B) -> [A] -> [B]),
  con "length" (length :: [A] -> Int),
  con "concat" (concat :: [[A]] -> [A]),
  schema "" "?F(A) = ?F(?F(A))",
  schema "commutative" "?F(X,Y) = ?F(Y,X)"]

simpleParse :: String -> Term Constant
simpleParse = parseProp (parseFromConfig defaultConfig)

parseFromSig :: (Signature a) => a -> String -> Prop (Term Constant)
parseFromSig sig = parseProp $ parseFromConfig (makeConfig sig)

main :: IO ()
main = do
  let p = parseProp (parseFromConfig defaultConfig) "?F(A) = ?F(?F(A))" :: Prop (Term Constant)
  print $ prettyShow p
  return ()
