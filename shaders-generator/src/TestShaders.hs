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

module TestShaders where

-- base
import qualified Prelude
import Prelude
  ( Functor, map )
import Control.Monad
  ( void )
import Data.Maybe
  ( fromJust )
import GHC.TypeLits
  ( KnownNat )

-- filepath
import System.FilePath
  ( (</>) )

-- vector-sized
import qualified Data.Vector.Sized as Vector
  ( fromList )

-- text-short
import Data.Text.Short
  ( ShortText )

-- fir
import FIR
  hiding ( Triangle )
import Math.Linear

import GHC.TypeLits

import Data.Foldable

import Common



----------------------------------------------------------
-- Second implem shaders

type DGPUFriendlyComputeDefs =
  '[ "ubo"  ':-> Uniform '[ DescriptorSet 1, Binding 0 ]
                    ( Struct
                       '[ "line" ':-> Int32,
                        "width" ':-> Word32,
                        "height" ':-> Word32 ]
                    )
    , "sharedfloat" ':-> Workgroup '[] (Array 1024 Float)
    , "input"  ':-> Image2D '[ DescriptorSet 0, Binding 0 ]
                    ( R32 F )                    
    , "rowsMatrix"  ':-> Image2D '[ DescriptorSet 0, Binding 1 ]
                    ( R32 F )
    , "colsMatrix"  ':-> Image2D '[ DescriptorSet 0, Binding 2 ]
                    ( R32 F )
    , "output"  ':-> Image2D '[ DescriptorSet 0, Binding 3 ]
                    ( R32 F )      
    , "main" ':-> EntryPoint '[ LocalSize 16 32 1 ]
                    Compute
   ]

vanillaLoader :: forall (s::ProgramState) . (_) =>
  Code Int32 -> Code Int32 -> Program s s (Code ())
vanillaLoader   i_groupIDx i_groupIDy = locally do
    ~(Vec3 i_x i_y _) <- get @"gl_LocalInvocationID"  
    loadToSharedMem i_x i_y i_groupIDx i_groupIDy

writeToColumnReducedMatrix:: forall (s::ProgramState) . (_) =>
  Code Word32 -> Code Word32 -> Code Word32 -> Code Float -> Program s s (Code ())
writeToColumnReducedMatrix i_groupIDx i_groupIDy columnIndex value = locally do
      imageWrite @"rowsMatrix" (Vec2 i_groupIDx (32 * i_groupIDy + columnIndex)) $ Vec4 value value value value

writeToRowReducedMatrix::forall (s::ProgramState) . (_) =>
  Code Word32 -> Code Word32 -> Code Word32 -> Code Float -> Program s s (Code ())
writeToRowReducedMatrix i_groupIDx i_groupIDy columnIndex value = locally do
      imageWrite @"colsMatrix" (Vec2 (32 * i_groupIDx + columnIndex) i_groupIDy) $ Vec4 value value value value  

nonWavefrontIntegralShader :: Module DGPUFriendlyComputeDefs
nonWavefrontIntegralShader = Module $ entryPoint @"main" @Compute do
  integralPass1Shader @"sharedfloat" @32 vanillaLoader (0 :: Code Float) writeToColumnReducedMatrix writeToRowReducedMatrix

type NaiveRowSumDefs =
  '[ "ubo"  ':-> Uniform '[ DescriptorSet 1, Binding 0 ]
                    ( Struct
                       '[ "line" ':-> Int32,
                        "width" ':-> Word32,
                        "height" ':-> Word32 ]
                    )
    , "input"  ':-> Image2D '[ DescriptorSet 0, Binding 0 ]
                    ( R32 F )                    
    , "output"  ':-> Image2D '[ DescriptorSet 0, Binding 1 ]
                    ( R32 F )
    , "main" ':-> EntryPoint '[ LocalSize 512 1 1 ]
                    Compute
   ]

naiveRowSumShader :: Module NaiveRowSumDefs
naiveRowSumShader = Module $ entryPoint @"main" @Compute do
    ~(Vec3 i_x _ _) <- get @"gl_GlobalInvocationID"

    _ <- def @"i" @RW @Word32 0
    width <- use @(Name "ubo" :.: Name "width")
    height <- use @(Name "ubo" :.: Name "height")

    _ <- def @"acc" @RW @Float 0
    while (get @"i" < pure width) do
      i <- get @"i"
      texel <- view @(Swizzle "x") <<$>> imageRead @"input" (Vec2 i_x i)
      modify @"acc" (+ texel)
      imageWrite @"output" (Vec2 i_x i) =<< ((\x -> Vec4 x x x x) <<$>> get @"acc")
      modify @"i" (+1)

type NaiveColumnSumDefs =
  '[ "ubo"  ':-> Uniform '[ DescriptorSet 1, Binding 0 ]
                    ( Struct
                       '[ "line" ':-> Int32,
                        "width" ':-> Word32,
                        "height" ':-> Word32 ]
                    )
    , "input"  ':-> Image2D '[ DescriptorSet 0, Binding 0 ]
                    ( R32 F )                    
    , "output"  ':-> Image2D '[ DescriptorSet 0, Binding 1 ]
                    ( R32 F )
    , "main" ':-> EntryPoint '[ LocalSize 1 512 1 ]
                    Compute
   ]

naiveColumnSumShader :: Module NaiveColumnSumDefs
naiveColumnSumShader = Module $ entryPoint @"main" @Compute do
    ~(Vec3 _ i_y _) <- get @"gl_GlobalInvocationID"

    _ <- def @"i" @RW @Word32 0
    width <- use @(Name "ubo" :.: Name "width")
    height <- use @(Name "ubo" :.: Name "height")

    _ <- def @"acc" @RW @Float 0
    while (get @"i" < pure height) do
      i <- get @"i"
      texel <- view @(Swizzle "x") <<$>> imageRead @"input" (Vec2 i i_y)
      modify @"acc" (+ texel)
      imageWrite @"output" (Vec2 i i_y) =<< ((\x -> Vec4 x x x x) <<$>> get @"acc")
      modify @"i" (+1)


rowReducedMatrixCollector :: forall (s::ProgramState). (_) =>
  Code Int32 -> Code Int32 -> Program s s (Code Float)
rowReducedMatrixCollector x y =
  view @(Swizzle "x") <<$>> imageRead @"rowsMatrix"  (Vec2 x y)    

colReducedMatrixCollector :: forall (s::ProgramState). (_) =>
  Code Int32 -> Code Int32 -> Program s s (Code Float)
colReducedMatrixCollector x y =
  view @(Swizzle "x") <<$>> imageRead @"colsMatrix"  (Vec2 x y)


imageIntegralPass2ShaderTest :: Module DGPUFriendlyComputeDefs
imageIntegralPass2ShaderTest = Module $ entryPoint @"main" @Compute do
  imageIntegralPass2Shader @"sharedfloat" @32 vanillaLoader rowReducedMatrixCollector colReducedMatrixCollector (writeFromSharedMem @"output") getZero

type AverageShaderGlobals =
  '[ 
      "input"  ':-> Image2D '[ DescriptorSet 0, Binding 0 ]
                    ( R32 F )                    
    , "output"  ':-> Image2D '[ DescriptorSet 0, Binding 1 ]
                    ( R32 F )
    , "main" ':-> EntryPoint '[ LocalSize 16 16 1 ]
                    Compute
   ]


averageShader :: Module AverageShaderGlobals
averageShader = Module $ entryPoint @"main" @Compute do
    ~(Vec3 u_gx u_gy _) <- get @"gl_GlobalInvocationID"
    let i_gx = fromIntegral u_gx :: Code Int32
    let i_gy = fromIntegral u_gy :: Code Int32

    result <- average @"input" i_gx i_gy 1
    imageWrite @"output" (Vec2 i_gx i_gy) (Vec4 result result result result)


------------------------------------------------
-- compiling

compileComputeShader :: forall a. (_) => Module a -> FilePath -> IO ( )
compileComputeShader shader path = void $ compileTo path [] shader

compileAllShaders :: FilePath -> IO ()
compileAllShaders shaderDir = sequence_
  [
    compileComputeShader transposeShader (shaderDir </> "test_transpose.spv"),
    compileComputeShader partialHorizontalPrefixSumShader (shaderDir </> "test_partialHorizontalPrefixSumShader.spv"),
    compileComputeShader partialVerticalPrefixSumShader (shaderDir </> "test_partialVerticalPrefixSumShader.spv"),
    compileComputeShader horizontalPrefixSumShader (shaderDir </> "test_horizontalPrefixSumShader.spv"),
    compileComputeShader partialVerticalPrefixSumShader (shaderDir </> "test_partialVerticalPrefixSumShader.spv"),
    compileComputeShader verticalPrefixSumShader (shaderDir </> "test_verticalPrefixSumShader.spv"),
    compileComputeShader averageShader (shaderDir </> "test_average.spv"),
    compileComputeShader nonWavefrontIntegralShader (shaderDir </> "test_nonWavefrontIntegralShader.spv"),
    compileComputeShader imageIntegralPass2ShaderTest (shaderDir </> "test_imageIntegralPass2Shader.spv")
  ]
