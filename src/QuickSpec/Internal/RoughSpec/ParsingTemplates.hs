module QuickSpec.Internal.RoughSpec.ParsingTemplates where

import QuickSpec.Internal.Prop
import QuickSpec.Internal.Term hiding (char)
import QuickSpec.Internal.Type
import QuickSpec.Internal.Haskell(Constant)

import Text.ParserCombinators.ReadP
import Data.Char

{- go from
("Prod_prop_01.add_zero", "add ?y Z = ?y",
     template_equation
      (template_app (template_app (template_hole 1, template_var 0), template_hole 0)
to
(template "Prod_prop_01.add_zero" "?F(X,?Y) = X")
-}
type PropTemplate = Prop (Term Constant)
type LabeledTemplate = (String, PropTemplate)
type TemplateEq = Equation (Term Constant)

parseTemplates :: String -> [LabeledTemplate]
parseTemplates s = case
  readP_to_S (between (char '[') (char ']') (sepBy parseLabTemplate (char ',')))
  (filter (\c -> (not $ isSpace c)) s) of
  [(x, [])] -> x
  --ps -> error ("parse': got result " ++ show ps ++ " while parsing " ++ s) -- replace with empty list?
  _ -> []

parseLabTemplate :: ReadP LabeledTemplate
parseLabTemplate = do
  _ <- char '('
  label <- between (char '"') (char '"') (munch (/= '"'))
  _ <- char ','
  lemmastring <- between (char '"') (char '"') (munch (/= '"'))
  _ <- char ','
  t <- parseTemplate
  _ <- char ')'
  --let tinfo = (label,lemmastring,src)
  return (label,t)

-- To start with lets assume it's just template_equation
parseTemplate :: ReadP PropTemplate
parseTemplate = do
  e <- parseEq
  return ([] :=>: e)

parseEq :: ReadP TemplateEq
parseEq = do
  _ <- string "template_equation"
  _ <- char '('
  lhs <- parseTerm
  _ <- char ','
  rhs <- parseTerm
  _ <- char ')'
  return $ (lhs :=: rhs)

parseTerm :: ReadP (Term Constant)
parseTerm = parseApp <++ parseHole <++ parseVar
--parseTerm = parseEmpty <++ parseApp <++ parseHole <++ parseVar

parseVar :: ReadP (Term Constant)
parseVar = do
  _ <- string "template_var"
  n <- munch1 isNumber
  let k = sum [(digitToInt i)*(10^(length n - j)) | (i,j) <- zip n [1..]]
    in return $ Var (V typeVar k)
{-
-- | A variable, which has a type and a number.
data Var = V { var_ty :: !Type, var_id :: {-# UNPACK #-} !Int }
  deriving (Eq, Ord, Show, Generic)
-}

parseHole :: ReadP (Term Constant)
parseHole = do
  _ <- string "template_hole"
  n <- munch1 isNumber
  let k = sum [(digitToInt i)*(10^(length n - j)) | (i,j) <- zip n [1..]]
      name = "f" ++ (show k)
    in return $ Hole (MV name typeVar)
{-
TODO: Figure out names of holes (have numbers sometimes going high up)
data MetaVar = MV { hole_id :: String, hole_ty :: Type }
  deriving (Eq, Ord, Show)
-}

parseApp :: ReadP (Term Constant)
parseApp = do
  _ <- string "template_app"
  _ <- char '('
  f <- parseTerm
  _ <- char ','
  arg <- parseTerm
  _ <- char ')'
  return $ (f :$: arg)

{-
Not sure how to handle t_empty
parseEmpty :: ReadP (Term Constant)
parseEmpty = do
  _ <- string "t_empty"
  return TEmpty

-}
-- cfg_templates :: [(String,Prop (Term Constant))],
{-data PropTemplate = Predicate TermTemplate
              | Equation Sign (TermTemplate, TermTemplate)
              | Implication [PropTemplate] PropTemplate
              | Negation PropTemplate
              | BImplication PropTemplate PropTemplate
              | Unknown
  deriving (Show, Eq, Ord)
-}
