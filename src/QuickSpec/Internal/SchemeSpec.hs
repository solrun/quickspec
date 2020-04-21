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
import QuickSpec.Internal.Terminal
import Text.Printf
import QuickSpec.Internal.SchemeSpec.PropGen
import QuickSpec.Internal.Testing
import QuickSpec.Internal.SchemeSpec.Matching

import Debug.Trace



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

    present funs prop = do
      --putLine $ prettyShow prop
      --norm <- normaliser
      let prop' = prettyDefinition funs (conditionalise prop)
      --(prettyAC norm (conditionalise prop))
      when (cfg_print_filter prop) $ do
        (n :: Int, props, cprops) <- get
        put (n+1, prop':props, prop':cprops)
        putLine $
          printf "%3d. %s" n $ show $
            prettyProp (names instances) prop' <+> disambiguatePropType prop

    testProp n current p = do
      let pres = if n == 0 then \_ -> return () else present (constantsOf current)
      --putLine "Testing..."
      res <- test p
      case res of
        Nothing -> do
          (_, props, _) <- get
          --putLine "Pruning..."
          let thing = (or $ map (flip simplePrune p) props) -- pruning or testing first?
          if thing
            then return ()
            else pres p
        _ -> return ()

    mainOf n current sofar = do
      unless (null (current cfg_constants)) $ do
        putLine $ show $ pPrintSignature
          (map (Fun . unhideConstraint) (current cfg_constants))
        putLine ""
      when (n > 0) $ do
        putText (prettyShow (warnings univ instances cfg))
        putLine "== Laws =="
      let testpres prop = testProp n current prop
      let testprops t = schemaProps t (constantsOf sofar) (constantsOf current)
      let maxArity = maximum $ map (typeArity . typ) (constantsOf current)
      let runschemespec schema = when (n > 0) $ do
          putLine ("Searching for " ++ fst schema ++ " properties...")
          putLine ("Generating expanded templates...")
          let expandedTemplates = expandTemplate maxArity $ snd schema
          putLine $ "Expanded templates: " ++ (show $ length expandedTemplates)
          putLine "Generating properties for testing..."
          let testps = concatMap testprops expandedTemplates
          putLine "Testing properties ..."
          mapM_ (lift . testpres) testps
      let runwithPruning schema = do
          (m :: Int, props, _) <- get
          put (m, props, []) -- We only want to give the pruner props found from the current schema
          let runschema = runschemespec schema :: Twee.Pruner Constant (StateT
                                 (Int, [Prop (Term Constant)], [Prop (Term Constant)])
                                 (QuickCheck.Tester
                                    TestCase
                                    (Term Constant)
                                    (Either (Value Ordy) (Term Constant))
                                    Terminal)) ()
          Twee.run cfg_twee { Twee.cfg_max_term_size = Twee.cfg_max_term_size cfg_twee `max` cfg_max_size} runschema

      mapM_ runwithPruning cfg_schemas
      when (n > 0) $ do
        putLine ""

    main = do
      (n :: Int, props, _) <- get
      put (n, cfg_background ++ props, [])
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
    fmap (reverse . (\(_,x,_) -> x)) $ flip execStateT (1, [], []) main
