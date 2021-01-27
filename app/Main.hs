{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleInstances   #-}

module Main where

import SimpleIntegralShader
import GuidedFilter
import Prelude

import Control.Monad
  ( void )

-- filepath
import System.FilePath
  ( (</>) )

  
import Data.Foldable


-- fir
import FIR


compileComputeShader :: forall a. (_) => Module a -> FilePath -> IO ( )
compileComputeShader shader path =
  (show <$> (compileTo path [] shader)) Prelude.>>= Prelude.putStrLn

compileAllShaders :: FilePath -> IO ()
compileAllShaders shaderDir = sequence_
  [
    compileComputeShader guidedFilterPass1 (shaderDir </> "guidedFilterPass1.spv"),
    compileComputeShader naiveHorizontalInclusiveScan (shaderDir </> "naiveHorizontalInclusiveScan.spv"),
    compileComputeShader naiveVerticalInclusiveScan (shaderDir </> "naiveVerticalInclusiveScan.spv"),
    compileComputeShader guidedFilterPass2 (shaderDir </> "guidedFilterPass2.spv"),
    compileComputeShader meanAandBPass1 (shaderDir </> "meanAandBPass1.spv"),
    compileComputeShader meanAandBPass2 (shaderDir </> "meanAandBPass2.spv"),
    compileComputeShader guidedFilterFinal (shaderDir </> "guidedFilterFinal.spv")

  ]


main :: IO ()
main = compileAllShaders ""