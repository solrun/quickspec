-- | Parsing strings into properties.
{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, GADTs, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module QuickSpec.Internal.Parse where

import Control.Monad
import Data.Char
import QuickSpec.Internal.Prop
import QuickSpec.Internal.Term hiding (char)
import QuickSpec.Internal.Type
import qualified Data.Label as Label
import Text.ParserCombinators.ReadP
import QuickSpec.Internal.Haskell hiding (con)
import Data.List(find)

class Parse fun a where
  parse :: ReadP fun -> ReadP a

instance Parse fun MetaVar where
  parse _ = do
    string "?" -- we use ? as a prefix to denote a metavariable
    xs <- munch isAlphaNum
    let name = xs
    return (MV name typeVar)

instance Parse fun Var where
  parse _ = do
    x <- satisfy isUpper
    xs <- munch isAlphaNum
    let name = x:xs
    -- Use Data.Label as an easy way to generate a variable number
    return (V typeVar (fromIntegral (Label.labelNum (Label.label name))))

instance (fun1 ~ fun, Apply (Term fun)) => Parse fun1 (Term fun) where
  parse pfun =
    parseApp
    where
      parseFun = Fun <$> pfun
      parseVar = Var <$> parse pfun
      parseMetaVar = Hole <$> parse pfun
      parseApp = do
        f <- parseFun <++ parseMetaVar <++ parseVar
        args <- parseArgs <++ return []
        return (unPoly (foldl apply (poly (f)) (map poly args)))
      parseArgs = between (char '(') (char ')') (sepBy (parse pfun) (char ','))

instance (Parse fun a, Typed a) => Parse fun (Equation a) where
  parse pfun = do
    t <- parse pfun
    string "="
    u <- parse pfun
    -- Compute type unifier of t and u
    -- "maybe mzero return" injects Maybe into MonadPlus
    pt <- maybe mzero return (polyMgu (poly (typ t)) (poly (typ u)))
    t <- maybe mzero return (cast (unPoly pt) t)
    u <- maybe mzero return (cast (unPoly pt) u)
    return (t :=: u)

instance (Parse fun a, Typed a) => Parse fun (Prop a) where
  parse pfun = do
    lhs <- sepBy (parse pfun) (string "&")
    unless (null lhs) (void (string "=>"))
    rhs <- parse pfun
    return (lhs :=>: rhs)

parseProp :: (Parse fun a, Pretty a) => ReadP fun -> String -> a
parseProp pfun xs =
  case readP_to_S (parse pfun <* eof) (filter (not . isSpace) xs) of
    [(x, [])] -> x
    ps -> error ("parse': got result " ++ prettyShow ps ++ " while parsing " ++ xs)

parseFromConfig :: Config -> ReadP Constant
parseFromConfig conf = do
  fname <- munch1 $ flip notElem ['(',')',' ']
  case find (\x -> con_name x == fname) (concat $ cfg_constants conf) of
    Just c -> return c
    Nothing -> mzero
