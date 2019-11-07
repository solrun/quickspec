module QuickSpec.Internal.SchemeSpec.PropGen where
import QuickSpec.Internal.Term
import QuickSpec.Internal.Prop
import QuickSpec.Internal.Haskell(Constant,con_type)
import QuickSpec.Internal.Type

import Data.Maybe(isJust)
import qualified Data.Map.Strict as Map

import Debug.Trace

-----------------------------------------------------------------
-- Schema-based term generation
-----------------------------------------------------------------

-- TODO: make sure variables with the same name end up with the same type (otherwise shouldn't be an accepted filling)

schemaProps :: Prop (Term Constant) -> [Constant] -> [(Term Constant, Term Constant)]
schemaProps (_ :=>: (sl :=: sr)) = fillHoles (sl,sr)

schemaSides :: Prop (Term Constant) -> (Term Constant, Term Constant)
schemaSides (_ :=>: (sl :=: sr)) = (sl,sr)

fillHoles :: (Term Constant, Term Constant) -> [Constant] -> [(Term Constant, Term Constant)]
fillHoles (lh, rh) cs = concatMap (tryFill (lh,rh)) options
  where options = findFillings (lh,rh) cs
        tryFill (t1,t2) m = case canFill m t1 of
          Nothing -> []
          Just t1' -> case canFill m t2 of
            Nothing -> []
            Just t2' -> [(t1',t2')]

-- Take schema and list of functions, returns a list of maps with hole names as keys and
-- possible fillings for those holes as values
findFillings :: (Term Constant, Term Constant) -> [Constant] -> [Map.Map String Constant]
findFillings s cs = findFillings' $ allFillings (allHoles s) cs

-- Takes a list of holes and a map of fillings found so far
findFillings' :: [(MetaVar, [Constant])] -> [Map.Map String Constant]
findFillings' l = map Map.fromList (crossProd $ map fillings l)
  where fillings (mv, cs) = [(hole_id mv, c)| c <- cs]

-- move to utils?
crossProd :: [[a]] -> [[a]]
crossProd [] = [[]]
crossProd (xs:xss) = [x:ys| x <- xs, ys <- yss]
  where yss = crossProd xss

feasibleFill :: MetaVar -> Constant -> Bool
feasibleFill mv c = isJust $ matchType (hole_ty mv) (con_type c)

allFillings :: [MetaVar] -> [Constant] -> [(MetaVar, [Constant])]
allFillings holes cs = map (\h -> (h, filter (feasibleFill h) cs)) holes

-- Try to fill the holes in the given term using the given map of fillings
canFill :: Map.Map String Constant -> Term Constant -> Maybe (Term Constant)
canFill fillings (Hole mv) = case Map.lookup (hole_id mv) fillings of
  Nothing -> trace ("no filling for hole " ++ (hole_id mv)) Nothing
  Just f -> --trace ("filling hole " ++ (hole_id mv) ++ " with " ++ con_name f)
    Just $ Fun f
canFill _ (Var v) = Just (Var v)
canFill _ (Fun f) = Just (Fun f)
canFill fillings (t1 :$: t2) = case canFill fillings t2 of
  Nothing -> --trace ("couldn't fill argument")
    Nothing
  Just t2' -> --trace ("filled argument: " ++ prettyShow t2') $
    case canFill fillings t1 of
      Nothing -> --trace ("couldn't fill applied")
        Nothing
      Just t1' -> --trace ("filled applied: "++ prettyShow t1') $
        case tryApply (poly t1') (poly t2') of
          Just v -> Just $ unPoly v
          Nothing -> Nothing

allHoles :: (Term Constant, Term Constant) -> [MetaVar]
allHoles (t1,t2) = allHoles' t1 (allHoles' t2 [])
  where allHoles' (Hole mv) hs = if mv `notElem` hs then hs ++ [mv] else hs
        allHoles' (tl :$: tr) hs = allHoles' tl (allHoles' tr hs)
        allHoles' _ hs = hs
