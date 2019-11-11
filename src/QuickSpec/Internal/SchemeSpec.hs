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

-- Generate properties based on schema + functions in scope
-- Test properties
-- Present properties where testing didn't find counterexample

schemeSpec :: Config -> IO ()
schemeSpec cfg@Config{..} = do
  let
    constantsOf f =
      [true | any (/= Function) (map classify (f cfg_constants))] ++
      f cfg_constants ++ concatMap selectors (f cfg_constants)
    constants = constantsOf concat

    univ = conditionalsUniverse (instanceTypes instances cfg) constants
    instances = cfg_instances `mappend` baseInstances

    eval = evalHaskell cfg_default_to instances

   -- present funs prop = do
   --   norm <- normaliser
   --   --let sf = schema_filter cfg_schemas prop
   --   let prop' = prettyDefinition funs (prettyAC norm (conditionalise prop))
   --   --when (cfg_print_filter prop && (fst sf)) $ do -- post-filtering
   --   when (cfg_print_filter prop) $ do -- no post-filtering
   --     (n :: Int, props) <- get
   --     put (n+1, prop':props)
   --     putLine $
   --       --printf "%3d. %s" n $ show $
   --       --printf "%3d. %s%s" n (showSchema $ snd sf)$ show $
   --       printf "%3d. %s%s" n $ show $
   --         prettyProp (names instances) prop' <+> disambiguatePropType prop

    mainOf n f g = do
      unless (null (f cfg_constants)) $ do
        putStrLn $ show $ pPrintSignature
          (map (Fun . unhideConstraint) (f cfg_constants))
        putStrLn ""
      when (n > 0) $ do
        putStr (prettyShow (warnings univ instances cfg))
        putStrLn "== Laws =="
      --let pres = if n == 0 then \_ -> return () else present (constantsOf f)
      let testpres prop = do
            p <- testProp prop
            case p of
              Just pp -> putStrLn $ prettyShow pp
      let runschemespec schema = do
            let testprops = schemaProps (snd schema) (constantsOf f)
            mapM_ testpres testprops
      mapM_ runschemespec cfg_schemas
      when (n > 0) $ do
        putStrLn ""

    --main = do
 -- forM_ cfg_background $ \prop -> do
 --   add prop
    round n = mainOf n (concat . take 1 . drop n) (concat . take (n+1))
    numrounds = length cfg_constants
  mapM_ round [0..numrounds-1]
--  where

  --join $
  --  fmap withStdioTerminal $
  --  generate $
  --  QuickCheck.run cfg_quickCheck (arbitraryTestCase cfg_default_to instances) eval $
  --  Twee.run cfg_twee { Twee.cfg_max_term_size = Twee.cfg_max_term_size cfg_twee `max` cfg_max_size } $
  --  runConditionals constants $
  --  fmap (reverse . snd) $ flip execStateT (1, []) main

testProp :: Prop (Term Constant) -> IO (Maybe (Prop (Term Constant)))
testProp p@(_ :=>: (lt :=: rt)) = do
  r <- Test.QuickCheck.quickCheckResult (lt == rt)
  if isSuccess r then return (Just p) else return Nothing
