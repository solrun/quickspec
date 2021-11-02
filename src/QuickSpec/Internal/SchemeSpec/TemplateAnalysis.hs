module QuickSpec.Internal.SchemeSpec.TemplateAnalysis where
import QuickSpec.Internal.Parse
import QuickSpec.Internal.Prop
import QuickSpec.Internal.Haskell
import QuickSpec.Internal.Term
import QuickSpec.Internal.Utils
import qualified QuickSpec.Internal.Explore.Polymorphic as Polymorphic
import Data.Lens.Light
import qualified Data.Map.Strict as Map
import Data.List(isInfixOf,permutations,sortOn,nub)


type Template = Prop (Term Constant)

readTemplate :: String -> Template
readTemplate = Polymorphic.regeneralise . parseProp (parseFromConfig cfg)
  where cfg = modL lens_constants ((:) [true]) defaultConfig

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
  normalForm :: Template,
  isPredicate :: Bool
                                   }

instance Show TemplateStats where
  show (TemplateStats c _ n _) = render (prettyProp (names baseInstances) n) ++ " count: " ++ (show c)

compileTemplateStats :: [(String,Template)] -> Map.Map String TemplateStats
compileTemplateStats ts = compileTemplateStats' Map.empty ts where
  compileTemplateStats' stats [] = stats
  compileTemplateStats' stats (t@(n,p):ps) = case Map.lookup (srep p) stats of
               Nothing -> compileTemplateStats' (Map.insert (srep p) (makeStats t) stats) ps
               Just _  -> compileTemplateStats' ((Map.adjust (insertName n)) (srep p) stats) ps
  srep = prettyShow . normalizeTemplate
  insertName :: String -> TemplateStats -> TemplateStats
  insertName propname t = TemplateStats {count = count t + 1, props = props t ++ [propname], normalForm = normalForm t, isPredicate = isPredicate t}
  makeStats :: (String,Template) -> TemplateStats
  makeStats (propname,prop) = TemplateStats {count = 1, props = [propname], normalForm = normalizeTemplate prop, isPredicate = isPred prop}
  isPred :: Template -> Bool
  isPred (_ :=>: l :=: r) = isTrue l || isTrue r
  isTrue x = show (pPrint x) == "True"

--compareTemplates :: Template -> Template -> Bool
--compareTemplates ([] :=>: eq1) ([] :=>: eq2) = canonicalise eq1 == canonicalise eq2
--                                             || canonicalise eq1 == canonicalise (flipEq eq2)
--  where flipEq (l :=: r) = r :=: l
--compareTemplates t1@(cs1 :=>: e1) t2@(cs2@(c:cs) :=>: e2) = compareConditions cs1 cs2 && normalize e1 == normalize e2
--  where normalize = canonicalise . ordered

compareConditions :: [Equation (Term Constant)] -> [Equation (Term Constant)] -> Bool
compareConditions cs1 cs2 = (map normalize cs1) `elem` (permutations (map normalize cs2))
  where normalize = canonicaliseHoles . canonicalise . ordered

normalizeTemplate :: Template -> Template
normalizeTemplate (cs :=>: e) = canonicaliseHoles $ canonicalise (usort  (map ordered cs) :=>: ordered e)

--normalizeEq :: Equation (Term Constant) -> Equation (Term Constant)
--normalizeEq e = canonicalise (ordered e)
--  where ordered eq@(l :=: r) = if measure l >= measure r then eq else r :=: l
ordered :: Equation (Term Constant) -> Equation (Term Constant)
ordered eq@(l :=: r) = if measure l >= measure r then eq else r :=: l

canonicaliseHoles :: Symbolic fun a => a -> a
canonicaliseHoles t = holesubst (\x -> Map.findWithDefault undefined x sub) t
  where
    sub =
      Map.fromList
        [(x, Hole (MV ("f_"++ show n) ty))
        | (x@(MV _ ty), n) <- zip (nub (mvars t)) [0..]]

main :: IO ()
main = do
  temps <- readFile "template-examples/templates.txt"
  let tm = templatesFromFile temps
  putStrLn $ "parsed properties: " ++ (show $ length tm)
  putStrLn $ "all properties: " ++ (show $ length $ lines temps)
  let stats = compileTemplateStats tm
  putStrLn $ "unique properties: " ++ (show $ Map.size stats)
  let ordered = reverse $ sortOn (count . snd) (Map.toList stats)
  print $ head ordered
