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
module QuickSpec.Internal.RoughSpec where

import qualified QuickSpec.Internal.Testing.QuickCheck as QuickCheck
import qualified QuickSpec.Internal.Pruning.Twee as Twee
import QuickSpec.Internal.Pruning
import QuickSpec.Internal.Haskell
import QuickSpec.Internal.Term
import QuickSpec.Internal.Prop
import QuickSpec.Internal.Type
import QuickSpec.Internal.Utils
import Test.QuickCheck hiding (total, classify, subterms, Fun)
import QuickSpec.Internal.Explore.Conditionals
import QuickSpec.Internal.Explore hiding (quickSpec)
import Control.Monad
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class (lift)
import Data.IORef
import QuickSpec.Internal.Terminal
import Text.Printf
import QuickSpec.Internal.RoughSpec.PropGen
import QuickSpec.Internal.Testing
import QuickSpec.Internal.RoughSpec.Matching
import QuickSpec.Internal.Explore.Polymorphic
import Debug.Trace



-- TODO: documentation
-- TODO: simplify/beautify?

-- TODO: Decrease code duplication between this function and Haskell/quickSpec function?
roughSpec :: Config -> IO [Prop (Term Constant)]
roughSpec cfg@Config{..} = do
  propNo <- newIORef 1
  props <- newIORef ([] :: [Prop (Term Constant)])
  let
    constantsOf f =
      usort (concatMap funs $
        [clas_true | Predicate{..} <- map classify (f cfg_constants)] ++
        [clas_true (classify clas_pred) | Selector{..} <- map classify (f cfg_constants)]) ++
      f cfg_constants ++ concatMap selectors (f cfg_constants)
    constants = constantsOf concat

    univ = conditionalsUniverse (instanceTypes instances cfg) constants
    instances = cfg_instances `mappend` baseInstances

    eval = evalHaskell cfg_default_to instances
    addPoly aprop = do
      let insts = typeInstances univ (canonicalise (regeneralise aprop))
      mapM_ add insts

    -- Present property to user and keep for future pruning.
    present funs prop = do
      --putLine $ prettyShow prop
      let prop' = prettyDefinition funs (conditionalise prop)
      when (cfg_print_filter prop) $ do
        (n :: Int, props) <- get
        put (n+1, prop':props)
        putLine $
          printf "%3d. %s" n $ show $
            prettyProp (names instances) prop' <+> disambiguatePropType prop

    -- Keep track of property for pruning without presenting it.
    putP funs prop = do
      let prop' = prettyDefinition funs (conditionalise prop)
      --putLine $ "putting" ++ (prettyShow prop')
      (n :: Int,props) <- get
      put (n,prop':props)

    -- Check whether property should be pruned
    provable ((_ :=>: t :=: u), True) = do
      --putLine $ "attempting to prove" ++ (prettyShow t) ++ (prettyShow u)
      t' <- normalise (oneTypeVar t)
      --putLine ("lhs" ++ (prettyShow t'))
      u' <- normalise (oneTypeVar u)
      --putLine ("rhs" ++ (prettyShow u'))
      return (t' == u')
    provable _ = do
      --putLine "not attempting to prove"
      return False

    testProp n current p'@(p,expanded) = do
      -- Background properties are not printed but kept for future pruning
      let pres = if n == 0 then putP (constantsOf current) else present (constantsOf current)
      --putLine ("Pruner" ++ (prettyShow p))
      prune <- provable p'
      (_, props) <- lift get
      --putLine ("props" ++ (prettyShow props))
      let extraprune = (or $ map (flip simplePrune p) props)
      -- For toggle pruning:
      --if prune
      --if extraprune
      if (prune || extraprune)
        then do
          --putLine "pruned"
          return ()
        else do
          --putLine ("Testing...")
          res <- test p
          --TODO: handle failed tests in some way?
          case res of
            TestPassed -> do
              _ <- addPoly p
                  --putLine (show expanded)
              lift $ pres p
            _ -> return ()

      --do
      --  --putLine ("Testing...")
      --  res <- test p
      --  case res of
      --    Nothing -> do
      --      _ <- addPoly p
      --          --putLine (show expanded)
      --      lift $ pres p
      --        where
      --          addPoly aprop = do
      --            let insts = typeInstances univ (canonicalise (regeneralise aprop))
      --            mapM_ add insts
      --    _ -> return ()
    mainOf n current sofar = do
      unless (null (current cfg_constants)) $ do
        putLine $ show $ pPrintSignature
          (map (Fun . unhideConstraint) (current cfg_constants))
        putLine ""
      when (n > 0) $ do
        putText (prettyShow (warnings univ instances cfg))
        putLine "== Laws =="
      let testpres prop = testProp n current prop
      let testprops (t,b) = zip (templateProps t (constantsOf sofar) (constantsOf current)) (repeat b)
      let maxArity = maxA $ map (typeArity . typ) (constantsOf current)
            where maxA [] = 0
                  maxA xs = maximum xs
      let runschemespec schema = do
            when (n > 0) $ do
              putLine ("Searching for " ++ fst schema ++ " properties...")
            --putLine ("Generating expanded templates...")
            let expandedTemplates = expandTemplate maxArity $ snd schema
            --putLine $ "Expanded templates: " ++ (show $ length expandedTemplates)
            --putLine "Generating properties for testing..."
            let testps = concatMap testprops expandedTemplates
            -- Turn off expansion
            --let testps = concatMap testprops $ [(snd schema, False)]
            --putLine "Testing properties ..."
            mapM_ testpres testps

      mapM_ runschemespec cfg_templates
      when (n > 0) $ do
        putLine ""

    main = Twee.run cfg_twee { Twee.cfg_max_term_size = 10, Twee.cfg_max_cp_depth = 1} $ do
      (n :: Int, props) <- lift get
      lift $ put (n, cfg_background ++ props)
      forM_ cfg_background $ \prop -> do
        addPoly prop
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
