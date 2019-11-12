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

import qualified Twee.Base as Twee
import qualified QuickSpec.Internal.Testing.QuickCheck as QuickCheck
import qualified QuickSpec.Internal.Pruning.Twee as Twee
import QuickSpec.Internal.Haskell
import QuickSpec.Internal.Term
import QuickSpec.Internal.Prop
import QuickSpec.Internal.Type
import Test.QuickCheck hiding (total, classify, subterms, Fun)
import QuickSpec.Internal.Explore.Conditionals
import QuickSpec.Internal.Haskell.Resolve
import QuickSpec.Internal.Pruning
import QuickSpec.Internal.Explore hiding (quickSpec)
import Control.Monad
import Control.Monad.Trans.State.Strict
import QuickSpec.Internal.Terminal
import Text.Printf
import QuickSpec.Internal.SchemeSpec.PropGen
import QuickSpec.Internal.Testing

import Debug.Trace
-- Generate properties based on schema + functions in scope
-- Test properties
-- Present properties where testing didn't find counterexample

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
      norm <- normaliser
      --let sf = schema_filter cfg_schemas prop
      let prop' = prettyDefinition funs (prettyAC norm (conditionalise prop))
      --when (cfg_print_filter prop && (fst sf)) $ do -- post-filtering
      when (cfg_print_filter prop) $ do -- no post-filtering
        (n :: Int, props) <- get
        put (n+1, prop':props)
        putLine $
          --printf "%3d. %s" n $ show $
          --printf "%3d. %s%s" n (showSchema $ snd sf)$ show $
          printf "%3d. %s" n $ show $
            prettyProp (names instances) prop' <+> disambiguatePropType prop

    mainOf n f g = do
      unless (null (f cfg_constants)) $ do
        putLine $ show $ pPrintSignature
          (map (Fun . unhideConstraint) (f cfg_constants))
        putLine ""
      when (n > 0) $ do
        putText (prettyShow (warnings univ instances cfg))
        putLine "== Laws =="
      let pres = if n == 0 then \_ -> return () else present (constantsOf f)
      let testpres prop = testProp pres prop
      let runschemespec schema = do
            --putLine (fst schema)
            let testprops = schemaProps (snd schema) (constantsOf g)
            mapM_ testpres testprops
      mapM_ runschemespec cfg_schemas
      when (n > 0) $ do
        putLine ""

    main = do
      forM_ cfg_background $ \prop -> do
        add prop
      mapM_ round [0..numrounds-1]
      where
        round n = mainOf n (concat . take 1 . drop n) (concat . take (n+1))
        numrounds = length cfg_constants

  join $
    fmap withStdioTerminal $
    generate $
    QuickCheck.run cfg_quickCheck (arbitraryTestCase cfg_default_to instances) eval $
    Twee.run cfg_twee { Twee.cfg_max_term_size = Twee.cfg_max_term_size cfg_twee `max` cfg_max_size } $
    --runConditionals constants $
    fmap (reverse . snd) $ flip execStateT (1, []) main

-- TODO : handy to actually keep track of/present counterexamples for debugging purposes?
testProp :: (MonadTester testcase (Term Constant) m) =>
            (Prop (Term Constant) -> m ()) ->
            --(Term Constant -> testcase -> result) ->
            Prop (Term Constant) -> m ()
testProp present p = do
  res <- test p
  case res of
    Nothing -> do
      present p
    Just tc ->
      return ()
