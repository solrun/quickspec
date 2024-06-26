module QuickSpec.Internal.RoughSpec.Matching where
import QuickSpec.Internal.Term
import QuickSpec.Internal.Prop
import qualified Data.Map.Strict as Map
import Data.Maybe(isJust)
import Debug.Trace

----------------------------------------------------------------------
-- * Pruning
----------------------------------------------------------------------

-- Should p2 be pruned based on p1?
simplePrune ::  (Eq f, PrettyTerm f) => Prop (Term f) -> Prop (Term f) -> Bool
simplePrune ([] :=>: p1) ([] :=>: p2) = (matchEqs p1 p2) || (appMatch p1 p2)
simplePrune _ _ = False

-- FIXME: This behaves strangely sometimes .. do we really need it?
-- Check whether p2 is an instance of applying the same function to both
-- sides of p1
-- appMatch p1 p2 is true if p1 = t1 == t2 and p2 = f t1 == f t2
appMatch :: (Eq f, PrettyTerm f) => Equation (Term f) -> Equation (Term f) -> Bool
appMatch e1 ((l21 :$: l22) :=: (r21 :$: r22)) =
  ((isJust $ matchTerms Map.empty [(l21,r21)]) &&
  (appMatch e1 (l22 :=: r22) || matchEqs e1 (l22 :=: r22)))
  || (appMatch' e1 (l21,r21) (l22,r22))
appMatch _ _ = False

appMatch' :: (Eq f, PrettyTerm f) => Equation (Term f) -> (Term f, Term f) -> (Term f, Term f) -> Bool
appMatch' e (f1, f2) (lt@(l1 :$: l2), rt@(r1 :$: r2)) =
  (matchEqs e (f1 :=: f2) && (isJust $ matchTerms Map.empty [(lt,rt)])) ||
  appMatch' e (f1 :$: l1, f2 :$: r1) (l2,r2)
appMatch' e (f1, f2) (lt, rt) =
  (matchEqs e (f1 :=: f2) && (isJust $ matchTerms Map.empty [(lt,rt)]))

matchEqs :: (Eq f, PrettyTerm f) => Equation (Term f) -> Equation (Term f) -> Bool
matchEqs (l1 :=: r1) (l2 :=: r2) = matchTermPairs [(l1,l2),(r1,r2)] ||
                                   matchTermPairs [(l1,r2),(l2,r1)]
  where matchTermPairs = isJust . matchTerms Map.empty

matchTerms :: (Eq f, PrettyTerm f) => (Map.Map Int String) -> [(Term f, Term f)] -> Maybe (Map.Map Int String)
matchTerms env [] = Just env
matchTerms env ((Var v, t):ts) =
  case Map.lookup vid env of
    Nothing -> --trace "hello" $
      matchTerms (Map.insert vid pt env) ts
    Just t' -> --trace ("Found term " ++ (prettyShow t') ++ "for var "++ (show vid)) $
      if pt == t' then matchTerms env ts else Nothing
  where vid = var_id v
        pt = prettyShow t
matchTerms env ((l1 :$: l2,r1 :$: r2):ts) =
  case matchTerms env [(l1,r1)] of
    Nothing -> Nothing
    Just s  -> matchTerms s ((l2,r2):ts)
matchTerms env ((t1@(Fun _), t2@(Fun _)):ts) =
  if prettyShow t1 == prettyShow t2
     then --trace "matching functions"
          matchTerms env ts
     else --trace "what"
          Nothing
matchTerms _ _ = Nothing


----------------------------------------------------------------------
-- * Matching properties to templates
----------------------------------------------------------------------
fitSchema :: (Eq f, PrettyTerm f) => Prop (Term f) -> Prop (Term f) -> Bool
fitSchema (_ :=>: s) (_ :=>: p)= matchEquations s p

matchEquations :: (Eq f, PrettyTerm f) => Equation (Term f) -> Equation (Term f) -> Bool
matchEquations (s1 :=: s2) (p1 :=: p2) = (matchSchemaTermPairs [(s1,p1),(s2,p2)]) ||
                                         (matchSchemaTermPairs [(s1,p2),(s2,p1)])
  where matchSchemaTermPairs = isJust . matchSchemaTermEnv Map.empty Map.empty

matchSchemaTerm :: (Eq f, PrettyTerm f) => Term f -> Term f -> Bool
matchSchemaTerm s t = isJust $ matchSchemaTermEnv Map.empty Map.empty [(s,t)]

type SubstEnv = Map.Map String String
type VarEnv   = Map.Map Int Int

matchSchemaTermEnv :: (Eq f, PrettyTerm f) => SubstEnv -> VarEnv -> [(Term f, Term f)] -> Maybe (SubstEnv,VarEnv)
matchSchemaTermEnv holesubst varsubst [] = Just (holesubst,varsubst)
matchSchemaTermEnv holesubst varsubst ((Var v1, Var v2):ts) =
  case matchVars varsubst v1 v2 of
    (False,_) -> Nothing
    (True, vs) -> matchSchemaTermEnv holesubst vs ts
matchSchemaTermEnv holesubst varsubst ((Hole mv, t2):ts) =
  case Map.lookup hid holesubst of
    Nothing -> matchSchemaTermEnv (Map.insert hid st2 holesubst) varsubst ts
    Just t -> if t == st2 then matchSchemaTermEnv holesubst varsubst ts else Nothing
  where hid = hole_id mv
        st2 = prettyShow t2
matchSchemaTermEnv holesubst varsubst (((s1 :$: s2),(t1 :$: t2)):ts) =
  case matchSchemaTermEnv holesubst varsubst [(s1,t1)] of
    Nothing -> Nothing
    Just (hs,vs) -> matchSchemaTermEnv hs vs ((s2,t2):ts)
matchSchemaTermEnv hs vs  ((Fun f1, Fun f2):ts) = if f1 == f2
  then matchSchemaTermEnv hs vs ts
  else Nothing
matchSchemaTermEnv _ _ _ = Nothing

matchVars :: VarEnv -> Var -> Var -> (Bool, VarEnv)
matchVars varsubsts v1 v2 = case Map.lookup vid1 varsubsts of
  Nothing -> (True, Map.insert vid1 vid2 varsubsts)
  Just k -> (k == vid2, varsubsts)
  where vid1 = var_id v1
        vid2 = var_id v2
