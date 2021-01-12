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

import Control.Monad
  ( void )

-- filepath
import System.FilePath
  ( (</>) )

  
import Data.Foldable


-- fir
import FIR


compileComputeShader :: forall a. (_) => Module a -> FilePath -> IO ( )
compileComputeShader shader path = void $ compileTo path [] shader

compileAllShaders :: FilePath -> IO ()
compileAllShaders shaderDir = sequence_
  [
    compileComputeShader guidedFilterPass1 (shaderDir </> "guidedFilterPass1.spv"),
    compileComputeShader naiveRowSumShader (shaderDir </> "test_naiveHorizontalSumShader.spv"),
    compileComputeShader naiveColumnSumShader (shaderDir </> "test_naiveVerticalSumShader.spv"),
    compileComputeShader guidedFilterPass2 (shaderDir </> "guidedFilterPass2.spv"),
    compileComputeShader meanAandBPass1 (shaderDir </> "meanAandBPass1.spv"),
    compileComputeShader meanAandBPass2 (shaderDir </> "meanAandBPass2.spv"),
    compileComputeShader guidedFilterFinal (shaderDir </> "guidedFilterFinal.spv")

  ]


main :: IO ()
main = compileAllShaders ""