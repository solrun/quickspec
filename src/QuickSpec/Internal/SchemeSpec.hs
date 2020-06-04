{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}
module QuickSpec.Internal.SchemeSpec where

import qualified QuickSpec.Internal.Testing.QuickCheck as QuickCheck
import qualified QuickSpec.Internal.Pruning.Twee as Twee
import QuickSpec.Internal.Pruning
import QuickSpec.Internal.Haskell
import QuickSpec.Internal.Term
import QuickSpec.Internal.Prop
import QuickSpec.Internal.Type
import Test.QuickCheck hiding (total, classify, subterms, Fun)
import QuickSpec.Internal.Explore.Conditionals
import QuickSpec.Internal.Explore hiding (quickSpec)
import Control.Monad
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
import QuickSpec.Internal.Terminal
import Text.Printf
import QuickSpec.Internal.SchemeSpec.PropGen
import QuickSpec.Internal.Testing
import QuickSpec.Internal.SchemeSpec.Matching
import QuickSpec.Internal.Explore.Polymorphic
import Debug.Trace
import Data.Functor.Identity
import QuickSpec.Internal.SetCover
import QuickSpec.Internal.Explore.Schemas



-- TODO: renaming
-- TODO: documentation

schemeSpec :: Config -> IO [Prop (Term Constant)]
schemeSpec cfg@Config{..} = do
  let
    constantsOf f =
      [true | any (/= Function) (map classify (f cfg_constants))] ++
      f cfg_constants ++ concatMap selectors (f cfg_constants)
    constants = constantsOf concat
    univ = conditionalsUniverse (instanceTypes instances cfg) constants
    instances = cfg_instances `mappend` baseInstances
    eval = evalHaskell cfg_default_to instances

  env <- runIdentity <$> generate (QuickCheck.run cfg_quickCheck (arbitraryTestCase cfg_default_to instances) eval (QuickCheck.Tester ask))
  let
    evalProp tc ([] :=>: t :=: u) = eval tc t == eval tc u
    testcases = QuickCheck.env_tests env

--    putP funs prop = do
--      let prop' = prettyDefinition funs (conditionalise prop)
--      --putLine $ "putting" ++ (prettyShow prop')
--      (n :: Int,props) <- get
--      put (n,prop':props)
--    present funs prop = do
--      --putLine $ prettyShow prop
--      let prop' = prettyDefinition funs (conditionalise prop)
--      when (cfg_print_filter prop) $ do
--        (n :: Int, props) <- get
--        put (n+1, prop':props)
--        putLine $
--          printf "%3d. %s" n $ show $
--            prettyProp (names instances) prop' <+> disambiguatePropType prop
--    provable (([] :=>: t :=: u), True) = do
--      t' <- normalise (oneTypeVar t)
--      u' <- normalise (oneTypeVar u)
--      return (t' == u')
--    provable _ = return False

    mainOf n current sofar = do
      unless (null (current cfg_constants)) $ do
        putLine $ show $ pPrintSignature
          (map (Fun . unhideConstraint) (current cfg_constants))
        putLine ""
      when (n > 0) $ do
        putText (prettyShow (warnings univ instances cfg))
        putLine "== Laws =="

      let
        literals =
          concatMap (expandedTemplateProps (sofar cfg_constants) (current cfg_constants)) (map snd cfg_schemas)

        testResults =
          [ (lit, passingTestcases testcases evalProp lit)
          | (lit, _) <- literals ]

        props = covers testResults

      mapM_ (putLine . prettyShow) literals
      mapM_ (putLine . prettyShow) props

      when (n > 0) $ do
        putLine ""

    main = Twee.run cfg_twee { Twee.cfg_max_term_size = 10, Twee.cfg_max_cp_depth = 1} $ do
      (n :: Int, props) <- lift get
      lift $ put (n, cfg_background ++ props)
      forM_ cfg_background $ \prop -> do
        add prop
      mapM_ round [0..numrounds-1]
      where
        round n        = mainOf n (currentRound n) (roundsSoFar n)
        currentRound n = (concat . take 1 . drop n)
        roundsSoFar n  = (concat . take (n+1))
        numrounds      = length cfg_constants

  join $
    fmap withStdioTerminal $
    generate $
    QuickCheck.run cfg_quickCheck (arbitraryTestCase cfg_default_to instances) eval $
    --runConditionals constants $
    fmap (reverse . snd) $ flip execStateT (1, []) main

passingTestcases :: [testcase] -> (testcase -> Prop t -> Bool) -> Prop t -> [Int]
passingTestcases tcs eval t =
  [i | (i, tc) <- zip [0..] tcs, eval tc t]

expandedTemplateProps :: [Constant] -> [Constant] -> Prop (Term Constant) -> [(Prop (Term Constant), Bool)]
expandedTemplateProps sofar current template =
  [ (prop', b)
  | (expanded, b) <- expandedTemplates,
    prop <- templateProps expanded sofar current,
    prop' <- allUnifications (\_ -> False) prop ]
  where
    expandedTemplates = expandTemplate maxArity template
    maxArity = maximum (0:map (typeArity . typ) current)
