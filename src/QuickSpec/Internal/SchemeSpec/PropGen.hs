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

schemaProps :: Prop (Term Constant) -> [Constant] -> [Prop (Term Constant)]
schemaProps (_ :=>: (sl :=: sr)) cs = map (\(lt,rt) -> ([] :=>: (lt :=: rt))) $ fillHoles (sl,sr) cs

schemaSides :: Prop (Term Constant) -> (Term Constant, Term Constant)
schemaSides (_ :=>: (sl :=: sr)) = (sl,sr)


fillHoles :: (Term Constant, Term Constant) -> [Constant] -> [(Term Constant, Term Constant)]
fillHoles (lh, rh) cs = concatMap (tryFill (lh,rh)) options
  where options = findFillings (lh,rh) cs
        tryFill (t1,t2) m = case canFill m Map.empty t1 of
          Nothing -> []
          Just (t1', vts) -> case canFill m vts t2 of
            Nothing -> []
            Just (t2', _) ->  if (t1' == t2') then [] else [(t1',t2')]

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
canFill :: Map.Map String Constant -> Map.Map Int Type -> Term Constant -> Maybe (Term Constant, Map.Map Int Type)
canFill fillings vartypes (Hole mv) = case Map.lookup (hole_id mv) fillings of
  Nothing -> --trace ("no filling for hole " ++ (hole_id mv))
    Nothing
  Just f -> --trace ("filling hole " ++ (hole_id mv) ++ " with " ++ con_name f)
    Just (Fun f, vartypes)
canFill _ vartypes (Var v) = case Map.lookup vid vartypes of
  Nothing -> --trace ("variable: "++ prettyShow v)
    Just (Var v, Map.insert vid vtype vartypes)
  Just t -> --trace ("types: "++ prettyShow t ++ prettyShow vtype) $
            if (isJust $ matchType t vtype) && (isJust $ matchType t vtype)
            then Just (Var v, vartypes)
            else Nothing
  where vid = var_id v
        vtype = var_ty v
canFill _ vartypes (Fun f) = Just ((Fun f), vartypes)
canFill fillings vartypes (t1 :$: t2) = case canFill fillings vartypes t2 of
  Nothing -> --trace ("couldn't fill argument")
    Nothing
  Just (t2', vts) -> --trace ("filled argument: " ++ prettyShow t2') $
    case canFill fillings vts t1 of
      Nothing -> --trace ("couldn't fill applied")
        Nothing
      Just (t1', vts') -> --trace ("filled applied: "++ prettyShow t1') $
        case tryApply (poly t1') (poly t2') of
          Just v -> case checkVars Map.empty upv of
            Nothing -> Nothing
            Just _  -> Just $ (upv, vts')
            where upv = unPoly v
          Nothing -> Nothing

-- Make sure variables with same name also have the same type
checkVars :: Map.Map Int Type -> Term Constant -> Maybe (Map.Map Int Type)
checkVars vartypes (Var v) = case Map.lookup vid vartypes of
  Nothing -> Just (Map.insert vid vtype vartypes)
  Just t -> if t == vtype
    then Just vartypes
    else Nothing
  where vtype = var_ty v
        vid   = var_id v
checkVars vartypes (t1 :$: t2) = case checkVars vartypes t1 of
  Nothing -> Nothing
  Just vts -> checkVars vts t2
checkVars vartypes _ = Just vartypes


allHoles :: (Term Constant, Term Constant) -> [MetaVar]
allHoles (t1,t2) = allHoles' t1 (allHoles' t2 [])
  where allHoles' (Hole mv) hs = if mv `notElem` hs then hs ++ [mv] else hs
        allHoles' (tl :$: tr) hs = allHoles' tl (allHoles' tr hs)
        allHoles' _ hs = hs
