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

-- FIXME!
-- What we want here:
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
      let sf = schema_filter cfg_schemas prop
      let prop' = prettyDefinition funs (prettyAC norm (conditionalise prop))
      --when (cfg_print_filter prop && (fst sf)) $ do -- post-filtering
      when (cfg_print_filter prop) $ do -- no post-filtering
        (n :: Int, props) <- get
        put (n+1, prop':props)
        putLine $
          --printf "%3d. %s" n $ show $
          printf "%3d. %s%s" n (showSchema $ snd sf)$ show $
            prettyProp (names instances) prop' <+> disambiguatePropType prop

    -- XXX do this during testing
    constraintsOk = memo $ \con ->
      or [ and [ isJust (findValue instances (defaultTo cfg_default_to constraint)) | constraint <- con_constraints (typeSubst sub con) ]
         | ty <- Set.toList (univ_types univ),
           sub <- maybeToList (matchType (typeRes (typ con)) ty) ]

    --conditions t = usort [p | f <- funs t, Selector _ p _ <- [classify f]]

    --singleUse ty =
    --  isJust (findInstance instances ty :: Maybe (Value SingleUse))
    mainOf n f g = do
      unless (null (f cfg_constants)) $ do
        putLine $ show $ pPrintSignature
          (map (Fun . unhideConstraint) (f cfg_constants))
        putLine ""
      when (n > 0) $ do
        putText (prettyShow (warnings univ instances cfg))
        putLine "== Laws =="
      let pres = if n == 0 then \_ -> return () else present (constantsOf f)
      --let runquickspec False tsize schemas =
      --      QuickSpec.Internal.Explore.quickSpec
      --      pres (flip eval) tsize cfg_max_commutative_size singleUse univ
      --      (enumerator schemas (map Fun (constantsOf g)))
      --    runquickspec True tsize schemas =
      --      QuickSpec.Internal.Explore.quickSpec
      --      pres (flip eval) tsize cfg_max_commutative_size singleUse univ
      --      (enumerator schemas (map Fun (constantsOf g)))
            --(concatMap schema_terms schemas) (concatMap schema_subterms schemas)
      --runquickspec True cfg_max_size cfg_schemas -- all schemas at once
      --runquickspec False cfg_max_size [] -- no schema pre-filtering
      --let small_size = 3
      --runquickspec False small_size [] -- exhaustively explore small terms
      mapM_ (runquickspec True cfg_max_size) $ map (\s -> [s]) cfg_schemas -- one schema at a time
      when (n > 0) $ do
        putLine ""

    main = do
      forM_ cfg_background $ \prop -> do
        add prop
      mapM_ round [0..rounds-1]
      where
        round n = mainOf n (concat . take 1 . drop n) (concat . take (n+1))
        rounds = length cfg_constants

  join $
    fmap withStdioTerminal $
    generate $
    QuickCheck.run cfg_quickCheck (arbitraryTestCase cfg_default_to instances) eval $
    Twee.run cfg_twee { Twee.cfg_max_term_size = Twee.cfg_max_term_size cfg_twee `max` cfg_max_size } $
    runConditionals constants $
    fmap (reverse . snd) $ flip execStateT (1, []) main
