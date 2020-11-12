import QuickSpec

listFuns = [
  con "reverse" (reverse :: [A] -> [A]),
  con "++" ((++) :: [A] -> [A] -> [A]),
  con "[]" ([] :: [A]),
  con "map" (map :: (A -> B) -> [A] -> [B]),
  con "length" (length :: [A] -> Int),
  con "concat" (concat :: [[A]] -> [A]),
  arith (Proxy :: Proxy Int)
  ,defaultTemplates
          ]

boolFuns = [
  "||"    `con` (||),
  "&&"    `con` (&&),
  "not"   `con` not,
  "True"  `con` True,
  "False" `con` False]

arithFuns = [
  "0" `con` (0   :: Int),
  "1" `con` (1   :: Int),
  con "+" ((+) :: Int -> Int -> Int),
  con "*" ((*) :: Int -> Int -> Int)
  ]

main = do
  roughSpec listFuns
  roughSpecDefault boolFuns
