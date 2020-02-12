module QuickSpec.Internal.SchemeSpec.PropGen where
import QuickSpec.Internal.Term
import QuickSpec.Internal.Prop
import QuickSpec.Internal.Haskell(Constant, con_type)
import QuickSpec.Internal.Type
import qualified QuickSpec.Internal.Explore.Polymorphic as Polymorphic

import Data.Maybe(isJust)
import Data.List(nub)
import qualified Data.Map.Strict as Map

import Debug.Trace

-----------------------------------------------------------------
-- Template-based term generation
-----------------------------------------------------------------
-- TODO: Better support for polymorphism
-- (what if different instances of the same hole-name in one property
-- can have different types?)

-- TODO: Renaming away from schema/scheme
-- TODO: Documentation

-- TODO: smarter support for HOF - fill holes with i.e. map f? (HOF + fresh var)?

-- TODO: conditionals

-- Take a schema and a list of functions, return a list of properties generated by using
-- the functions to fill the holes of the schema
schemaProps :: Prop (Term Constant) -> [Constant] -> [Constant] -> [Prop (Term Constant)]
schemaProps p allcs currcs = map (\(lt,rt) -> Polymorphic.regeneralise
                                          ([] :=>: (lt :=: rt))) $ fillHoles (sides p) (allcs, currcs)

sides :: Prop (Term Constant) -> (Term Constant, Term Constant)
sides (_ :=>: (sl :=: sr)) = (sl,sr)

fillHoles :: (Term Constant, Term Constant) -> ([Constant], [Constant]) -> [(Term Constant, Term Constant)]
fillHoles (lh, rh) (allcs, currcs) = concatMap (tryFill (lh,rh)) options
  where options = findFillings (lh,rh) (allcs, currcs)
        tryFill (t1,t2) m = case canFill m Map.empty t1 of
          Nothing -> []
          Just (t1', vts) -> case canFill m vts t2 of
            Nothing -> []
            Just (t2', _) ->
              if ((t1' /= t2') &&
                  (isJust $ polyMgu (poly $ typ t1') (poly $ typ t2')))
              then [(t1',t2')]
              else []

-- Take schema and list of functions, returns a list of maps with hole names as keys and
-- possible fillings for those holes as values
findFillings :: (Term Constant, Term Constant) ->([Constant], [Constant])-> [Map.Map String Constant]
findFillings s (allcs, currcs) = findFillings' currcs $ allFillings (allHoles s) allcs
  where findFillings' :: [Constant] -> [(MetaVar, [Constant])] -> [Map.Map String Constant]
        findFillings' curr l = map Map.fromList $ filter (containsAny curr . map snd) (crossProd $ map fillings l) -- filter out those that have nothing form currcs
        fillings (mv, cons) = [(hole_id mv, c)| c <- cons]
        allFillings :: [MetaVar] -> [Constant] -> [(MetaVar, [Constant])]
        allFillings holes cons = map (allFeasibleFillings cons) holes
        allFeasibleFillings fs h = (h, filter (feasibleFill h) fs)
        feasibleFill :: MetaVar -> Constant -> Bool
        feasibleFill mv c = (isJust $ matchType (hole_ty mv) (con_type c))
                         && (typeArity (hole_ty mv) == typeArity (con_type c))
        containsAny xs ys = or $ map (flip elem ys) xs

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
            if (isJust $ matchType t vtype)
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

-- Find all holes in a property
allHoles :: (Term Constant, Term Constant) -> [MetaVar]
allHoles (t1,t2) = allHoles' t1 (allHoles' t2 [])
  where allHoles' (Hole mv) hs = if mv `notElem` hs then hs ++ [mv] else hs
        allHoles' (tl :$: tr) hs = allHoles' tl (allHoles' tr hs)
        allHoles' _ hs = hs

-- move to utils?
crossProd :: [[a]] -> [[a]]
crossProd [] = [[]]
crossProd (xs:xss) = [x:ys| x <- xs, ys <- yss]
  where yss = crossProd xss
