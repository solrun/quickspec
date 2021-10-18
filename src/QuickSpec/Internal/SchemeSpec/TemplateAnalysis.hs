module QuickSpec.Internal.SchemeSpec.TemplateAnalysis where
import QuickSpec.Internal.Parse
import QuickSpec.Internal.Prop
import QuickSpec.Internal.Haskell
import QuickSpec.Internal.Term
import QuickSpec.Internal.Utils

import qualified Data.Map.Strict as Map
import Data.List(isInfixOf,permutations)


type Template = Prop (Term Constant)

readTemplate :: String -> Template
readTemplate = parseProp (parseFromConfig defaultConfig)

rawTemplatesFromFile :: String -> [(String,String)]
rawTemplatesFromFile tfile = rawtreps
  where
    rawtreps = map tpairs tstrings
    tpairs t = (fst $ tpair t, t2 $ snd $ tpair t)
    tpair t = break (== ';') t
    t2 x = dropWhile (== ' ' ) $ dropWhile (== ';') x
    tstrings = lines tfile

templatesFromFile :: String -> [(String, Template)]
templatesFromFile tfile = templates
  where
    templates = map (\(a,b) -> (a,readTemplate b)) treps
    treps = removeLeqGeq $ removeNegations $ equations rawtreps
    rawtreps = rawTemplatesFromFile tfile
    equations = filter (\(_,b) -> isInfixOf " = " b)
    removeNegations = filter (\(_,b) -> not $ isInfixOf "not" b)
    removeLeqGeq = filter (\(_,b) -> not $ isInfixOf "<=" b || isInfixOf ">=" b)

data TemplateStats = TemplateStats {
  count :: Int,
  props :: [String],
  normalForm :: Template
                                   }

compileTemplateStats :: [(String,Template)] -> Map.Map String TemplateStats
compileTemplateStats ts = compileTemplateStats' Map.empty ts where
  compileTemplateStats' stats [] = stats
  compileTemplateStats' stats (t@(n,p):ps) = case Map.lookup (srep p) stats of
               Nothing -> compileTemplateStats' (Map.insert (srep p) (makeStats t) stats) ps
               Just _  -> compileTemplateStats' ((Map.adjust (insertName n)) (srep p) stats) ps
  srep = prettyShow . normalizeTemplate
  insertName :: String -> TemplateStats -> TemplateStats
  insertName propname t = TemplateStats {count = count t + 1, props = props t ++ [propname], normalForm = normalForm t}
  makeStats :: (String,Template) -> TemplateStats
  makeStats (propname,prop) = TemplateStats {count = 1, props = [propname], normalForm = normalizeTemplate prop}

compareTemplates :: Template -> Template -> Bool
compareTemplates ([] :=>: eq1) ([] :=>: eq2) = canonicalise eq1 == canonicalise eq2
                                             || canonicalise eq1 == canonicalise (flipEq eq2)
  where flipEq (l :=: r) = r :=: l
compareTemplates t1@(cs1 :=>: e1) t2@(cs2@(c:cs) :=>: e2) = compareConditions cs1 cs2 && normalizeEq e1 == normalizeEq e2

compareConditions :: [Equation (Term Constant)] -> [Equation (Term Constant)] -> Bool
compareConditions cs1 cs2 = (map normalizeEq cs1) `elem` (permutations (map normalizeEq cs2))

normalizeTemplate :: Template -> Template
normalizeTemplate (cs :=>: e) = (usort  (map normalizeEq cs) :=>: normalizeEq e)

normalizeEq :: Equation (Term Constant) -> Equation (Term Constant)
normalizeEq e = canonicalise (ordered e)
  where ordered eq@(l :=: r) = if measure l >= measure r then eq else r :=: l

main :: IO ()
main = do
  temps <- readFile "template-examples/templates.txt"
  let tm = templatesFromFile temps
  putStrLn $ "parsed properties: " ++ (show $ length tm)
  putStrLn $ "all properties: " ++ (show $ length $ lines temps)
  let stats = compileTemplateStats tm
  putStrLn $ "unique properties: " ++ (show $ Map.size stats)
