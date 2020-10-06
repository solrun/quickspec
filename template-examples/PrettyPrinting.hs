-- Pretty-printing combinators.
{-# LANGUAGE DeriveDataTypeable, TypeOperators, StandaloneDeriving, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}
--module PrettyPrinting where
import Prelude hiding ((<>))
import Control.Monad
import Test.QuickCheck
import QuickSpec
import Text.PrettyPrint.HughesPJ hiding (Str)
import Data.Proxy
import Data.Constraint

deriving instance Typeable Doc

instance Arbitrary Doc where
  arbitrary =
    sized $ \n ->
      let bin = resize (n `div` 2) arbitrary
          un = resize (n-1) arbitrary in
      oneof $
        [ liftM2 ($$) bin bin | n > 0 ] ++
        [ liftM2 (<>) bin bin | n > 0 ] ++
        [ liftM2 nest arbitrary un | n > 0 ] ++
        [ fmap text arbitrary ]

-- Observational equality.
instance Observe Context Str Doc where
  observe (Context ctx) d = Str (render (ctx d))
newtype Str = Str String deriving (Eq, Ord)

newtype Context = Context (Doc -> Doc)

instance Arbitrary Context where
  arbitrary = Context <$> ctx
    where
      ctx =
        sized $ \n ->
        oneof $
          [ return id ] ++
          [ liftM2 (\x y d -> op (x d) y) (resize (n `div` 2) ctx) (resize (n `div` 2) arbitrary) | n > 0, op <- [(<>), ($$)] ] ++
          [ liftM2 (\x y d -> op x (y d)) (resize (n `div` 2) arbitrary) (resize (n `div` 2) ctx) | n > 0, op <- [(<>), ($$)] ] ++
          [ liftM2 (\x y d -> nest x (y d)) arbitrary (resize (n-1) ctx) | n > 0 ]

unindented :: Doc -> Bool
unindented d = render (nest 100 (text "" <> d)) == render (nest 100 d)

nesting :: Doc -> Int
nesting d = head [ i | i <- nums, unindented (nest (-i) d) ]
  where
    nums = 0:concat [ [i, -i] | i <- [1..] ]
roughSpec = qqSpec





ppSig = [
  withMaxTermSize 9,
  monoTypeObserve (Proxy :: Proxy Doc),
  defaultTo (Proxy :: Proxy Bool),

  background [
    con "[]" ([] :: [A]),
    con "++" ((++) :: [A] -> [A] -> [A]),
    con "0" (0 :: Int),
    con "+" ((+) :: Int -> Int -> Int),
    con "length" (length :: [A] -> Int)
    ],

  con "empty" empty,
  con "text" text,
  con "nest" nest,
  con "<>" (<>),
  con "<+>" (<+>),
  con "$$" ($$),
  con "hcat" hcat,
  con "hsep" hsep,
  con "vcat" vcat,
  con "sep"  sep,
  con "fsep" fsep,

  template "fix-point" "?F(?X) = ?X",
  template "empty" "?F(?X) = empty",
  template "left-id-elem" "?F(?G,X) = X",
  template "right-id-elem" "?F(X,?G) = X",
  template "commutative" "?F(X,Y) = ?F(Y,X)",
  template "op-commute" "?F(?G(X)) = ?G(?F(X))",
  template "2-distributive" "?F(?G(X,Y)) = ?G(?F(X),?F(Y))",
  template "analogy-distributive" "?F(?G(X),?G(Y)) = ?G(?H(X,Y))",
  template "associative-3" "?F(?F(X,Y),Z) = ?F(X,?F(Y,Z))"]

main = roughSpec ppSig