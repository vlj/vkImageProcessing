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

module GuidedFilter where

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


import Data.Proxy  

-- fir
import FIR
  hiding ( Triangle )
import Math.Linear


import GHC.TypeLits

import Data.Foldable

import Common
--import FirstImplem



----------------------------------------------------------
-- Common

type FullSize = 16
type HalfSize = 8


type GuidedFilterPass1ComputeDef =
  '[ "ubo"  ':-> Uniform '[ DescriptorSet 1, Binding 0 ]
                    ( Struct
                       '[ "line" ':-> Int32,
                        "width" ':-> Word32,
                        "height" ':-> Word32 ]
                    )
    , "sharedVec2" ':-> Workgroup '[] (Array 272 (V 2 Float))
    , "input"  ':-> Image2D '[ DescriptorSet 0, Binding 0 ]
                    ( R32 F )                    
    , "columnReducedMatrix"  ':-> Image2D '[ DescriptorSet 0, Binding 1 ]
                    ( R32 F )
    , "squaredColumnReducedMatrix"  ':-> Image2D '[ DescriptorSet 0, Binding 2 ]
                    ( R32 F )                    
    , "rowReducedMatrix"  ':-> Image2D '[ DescriptorSet 0, Binding 3 ]
                    ( R32 F )
    , "squaredRowReducedMatrix"  ':-> Image2D '[ DescriptorSet 0, Binding 4 ]
                    ( R32 F )
    , "main" ':-> EntryPoint '[ LocalSize HalfSize FullSize 1 ]
                    Compute
   ]


pictureAndSquare :: forall (blockEdge :: Nat) (s::ProgramState) . (_) =>
  Code Int32 -> Code Int32 -> Program s s (Code ())
pictureAndSquare  i_groupIDx i_groupIDy = locally do
    ~(Vec3 i_x i_y _) <- get @"gl_LocalInvocationID"
    let blockEdgeVal = cast (natVal (Proxy @blockEdge))
    let halfBlockEdgeVal = cast (natVal (Proxy @(Div blockEdge 2)))
    let halfBlockEdgeVal2 = cast (natVal (Proxy @(Div blockEdge 2)))
    let i_gx = blockEdgeVal * i_groupIDx + fromIntegral i_x
    let i_gy = blockEdgeVal * i_groupIDy + fromIntegral i_y
    let idx = index2dTo1d @blockEdge i_x i_y
    let idxp16 = index2dTo1d @blockEdge (i_x + halfBlockEdgeVal) i_y

    value <- imageRead @"input" ( Vec2 i_gx i_gy )
    assign @(Name "sharedVec2" :.: AnIndex Word32) idx $ Vec2 value (value * value)
    value2 <- imageRead @"input" ( Vec2 (i_gx + halfBlockEdgeVal2) i_gy )
    assign @(Name "sharedVec2" :.: AnIndex Word32) idxp16 $ Vec2 value2 (value2 * value2)
    controlBarrier Workgroup Nothing

writeValAndSquareToColumnReducedMatrixes:: forall (blockEdge :: Nat) (columnReducedMatrix :: Symbol) (squaredColumnReducedMatrix :: Symbol) (s::ProgramState) . (_) =>
  Code Word32 -> Code Word32 -> Code Word32 -> Code (V 2 Float) -> Program s s (Code ())
writeValAndSquareToColumnReducedMatrixes i_groupIDx i_groupIDy columnIndex (Vec2 i isq) = locally do
      let blockEdgeVal = cast (natVal (Proxy @blockEdge))
      imageWrite @columnReducedMatrix (Vec2 i_groupIDx (blockEdgeVal * i_groupIDy + columnIndex)) i
      imageWrite @squaredColumnReducedMatrix (Vec2 i_groupIDx (blockEdgeVal * i_groupIDy + columnIndex)) isq

writeValAndSquareToRowReducedMatrix::forall (blockEdge :: Nat) (rowReducedMatrix :: Symbol) (squaredRowReducedMatrix :: Symbol) (s::ProgramState) . (_) =>
  Code Word32 -> Code Word32 -> Code Word32 -> Code (V 2 Float) -> Program s s (Code ())
writeValAndSquareToRowReducedMatrix i_groupIDx i_groupIDy columnIndex  (Vec2 i isq) = locally do
      let blockEdgeVal = cast (natVal (Proxy @blockEdge))  
      imageWrite @rowReducedMatrix (Vec2 (blockEdgeVal * i_groupIDx + columnIndex) i_groupIDy) i
      imageWrite @squaredRowReducedMatrix (Vec2 (blockEdgeVal * i_groupIDx + columnIndex) i_groupIDy) isq

guidedFilterPass1 :: Module GuidedFilterPass1ComputeDef
guidedFilterPass1 = Module $ entryPoint @"main" @Compute do
  integralPass1Shader @"sharedVec2" @FullSize (pictureAndSquare @FullSize) (Vec2 0 0) (writeValAndSquareToColumnReducedMatrixes @FullSize @"columnReducedMatrix" @"squaredColumnReducedMatrix") (writeValAndSquareToRowReducedMatrix @FullSize @"rowReducedMatrix" @"squaredRowReducedMatrix")



----------------------------------------------------------
-- Second implem shaders

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
      texel <- imageRead @"input" (Vec2 i_x i)
      modify @"acc" (+ texel)
      imageWrite @"output" (Vec2 i_x i) =<< get @"acc"
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
      texel <- imageRead @"input" (Vec2 i i_y)
      modify @"acc" (+ texel)
      imageWrite @"output" (Vec2 i i_y) =<< get @"acc"
      modify @"i" (+1)


type GuidedFilterPass2ComputeDef =
  '[ "ubo"  ':-> Uniform '[ DescriptorSet 1, Binding 0 ]
                    ( Struct
                       '[ "line" ':-> Int32,
                        "width" ':-> Word32,
                        "height" ':-> Word32 ]
                    )
    , "sharedVec2" ':-> Workgroup '[] (Array 272 (V 2 Float))
    , "input"  ':-> Image2D '[ DescriptorSet 0, Binding 0 ]
                    ( R32 F )                    
    , "summedColumnReducedMatrix"  ':-> Image2D '[ DescriptorSet 0, Binding 1 ]
                    ( R32 F )
    , "summedSquaredColumnReducedMatrix"  ':-> Image2D '[ DescriptorSet 0, Binding 2 ]
                    ( R32 F )                    
    , "summedRowReducedMatrix"  ':-> Image2D '[ DescriptorSet 0, Binding 3 ]
                    ( R32 F )
    , "summedSquaredRowReducedMatrix"  ':-> Image2D '[ DescriptorSet 0, Binding 4 ]
                    ( R32 F )                    
    , "outputMean"  ':-> Image2D '[ DescriptorSet 0, Binding 5 ]
                    ( R32 F )
    , "outputSquaredMean"  ':-> Image2D '[ DescriptorSet 0, Binding 6 ]
                    ( R32 F )                    
    , "main" ':-> EntryPoint '[ LocalSize HalfSize FullSize 1 ]
                    Compute
   ]

writeFromSharedMem2 :: forall (blockEdge :: Nat) (sharedName :: Symbol) (destination :: Symbol) a b (s :: ProgramState). ( _ )
  => (Code a -> Code b) ->
    Code Word32 -> Code Word32 ->
    Code Int32 -> Code Int32 ->
    Program s s (Code ())
writeFromSharedMem2 shared2glob i_x i_y blockIDx blockIDy = locally do
    let blockEdgeVal = cast (natVal (Proxy @blockEdge))
    let halfBlockEdgeVal = cast (natVal (Proxy @(Div blockEdge 2)))
    let halfBlockEdgeVal2 = cast (natVal (Proxy @(Div blockEdge 2)))
    let i_gx = blockEdgeVal * blockIDx + fromIntegral i_x
    let i_gy = blockEdgeVal * blockIDy + fromIntegral i_y
    let idx = index2dTo1d @blockEdge i_x i_y
    let idxp16 = index2dTo1d @blockEdge (i_x + halfBlockEdgeVal) i_y

    v1 <- use @(Name sharedName :.: AnIndex Word32) idx
    imageWrite @destination ( Vec2 i_gx i_gy ) $ shared2glob v1
    v2 <- use @(Name sharedName :.: AnIndex Word32) idxp16
    imageWrite @destination ( Vec2(i_gx + halfBlockEdgeVal2) i_gy ) $ shared2glob v2

writeFromSharedMem2Global :: forall (blockEdge :: Nat) (sharedName :: Symbol) (destination1 :: Symbol) (destination2 :: Symbol) (s :: ProgramState). ( _ )
  => Code Word32 -> Code Word32 ->
    Code Int32 -> Code Int32 ->
    Program s s (Code ())
writeFromSharedMem2Global i_x i_y blockIDx blockIDy = locally do
  writeFromSharedMem2 @blockEdge @sharedName @destination1 (\(Vec2 x _) -> x) i_x i_y blockIDx blockIDy
  writeFromSharedMem2 @blockEdge @sharedName @destination2  (\(Vec2 _ x) -> x) i_x i_y blockIDx blockIDy
  

getV2Zero :: forall (s :: ProgramState). ( _ )
  => () -> Program s s (Code (V 2 Float))
getV2Zero _ = locally do
  return $ Vec2 0 0

rowReducedMatrixCollectorV2 :: forall (summedColumnReducedMatrix :: Symbol) (summedSquaredColumnReducedMatrix :: Symbol) (s::ProgramState). (_) =>
  Code Int32 -> Code Int32 -> Program s s (Code (V 2 Float))
rowReducedMatrixCollectorV2 ix iy = locally do
  x <- imageRead @summedColumnReducedMatrix  (Vec2 ix iy)    
  y <- imageRead @summedSquaredColumnReducedMatrix  (Vec2 ix iy)
  return $ Vec2 x y

colReducedMatrixCollectorV2 :: forall (summedRowReducedMatrix :: Symbol) (summedSquaredRowReducedMatrix :: Symbol) (s::ProgramState). (_) =>
  Code Int32 -> Code Int32 -> Program s s (Code (V 2 Float))
colReducedMatrixCollectorV2 ix iy = locally do
  x <- imageRead @summedRowReducedMatrix  (Vec2 ix iy)
  y <- imageRead @summedSquaredRowReducedMatrix  (Vec2 ix iy)
  return $ Vec2 x y

guidedFilterPass2 :: Module GuidedFilterPass2ComputeDef
guidedFilterPass2 = Module $ entryPoint @"main" @Compute do
  imageIntegralPass2Shader @"sharedVec2" @FullSize (pictureAndSquare @FullSize) (rowReducedMatrixCollectorV2 @"summedColumnReducedMatrix" @"summedSquaredColumnReducedMatrix") (colReducedMatrixCollectorV2 @"summedRowReducedMatrix" @"summedSquaredRowReducedMatrix") (writeFromSharedMem2Global @FullSize @"sharedVec2" @"outputMean" @"outputSquaredMean") getV2Zero

------------------------------------------------
-- A and B


type IntegralAandBPass1ComputeDef =
  '[ "ubo"  ':-> Uniform '[ DescriptorSet 1, Binding 0 ]
                    ( Struct
                       '[ "line" ':-> Int32,
                        "width" ':-> Word32,
                        "height" ':-> Word32 ]
                    )
    , "sharedVec2" ':-> Workgroup '[] (Array 272 (V 2 Float))
    , "I"  ':-> Image2D '[ DescriptorSet 0, Binding 0 ]
                    ( R32 F )
    , "squaredI"  ':-> Image2D '[ DescriptorSet 0, Binding 1 ]
                    ( R32 F )                    
    , "AcolumnReducedMatrix"  ':-> Image2D '[ DescriptorSet 0, Binding 2 ]
                    ( R32 F )
    , "BColumnReducedMatrix"  ':-> Image2D '[ DescriptorSet 0, Binding 3 ]
                    ( R32 F )                    
    , "AReducedMatrix"  ':-> Image2D '[ DescriptorSet 0, Binding 4 ]
                    ( R32 F )
    , "BRowReducedMatrix"  ':-> Image2D '[ DescriptorSet 0, Binding 5 ]
                    ( R32 F )
    , "main" ':-> EntryPoint '[ LocalSize HalfSize FullSize 1 ]
                    Compute
   ]

fillAandBfromIsquaredI :: forall (blockEdge :: Nat) (s::ProgramState) . (_) =>
  Code Word32 -> Code Word32 -> Code Int32 -> Code Int32 -> Program s s (Code ())
fillAandBfromIsquaredI i_x i_y i_gx i_gy = locally do
    let idx = (index2dTo1d @blockEdge) i_x i_y

    let epsilon = 0.01

    i <- average @"I" i_gx i_gy 10
    squareI <- average @"squaredI" i_gx i_gy 10
    _ <- def @"a" @RW $ div (squareI - i * i) (squareI - i * i + epsilon)
    a <- get @"a"
    let b = i * (1 - a)
    assign @(Name "sharedVec2" :.: AnIndex Word32) idx $ Vec2 a b

aandBfromIsquaredI :: forall (blockEdge :: Nat) (s::ProgramState) . (_) =>
  Code Int32 -> Code Int32 -> Program s s (Code ())
aandBfromIsquaredI  i_groupIDx i_groupIDy = locally do
    let blockEdgeVal = cast (natVal (Proxy @blockEdge))
    let halfBlockEdgeVal = cast (natVal (Proxy @(Div blockEdge 2)))
    let halfBlockEdgeVal2 = cast (natVal (Proxy @(Div blockEdge 2)))
    ~(Vec3 i_x i_y _) <- get @"gl_LocalInvocationID"  
    let i_gx = blockEdgeVal * i_groupIDx + fromIntegral i_x
    let i_gy = blockEdgeVal * i_groupIDy + fromIntegral i_y

    fillAandBfromIsquaredI @blockEdge i_x i_y i_gx i_gy
    fillAandBfromIsquaredI @blockEdge (i_x + halfBlockEdgeVal) i_y (i_gx + halfBlockEdgeVal2) i_gy

    controlBarrier Workgroup Nothing   


meanAandBPass1 :: Module IntegralAandBPass1ComputeDef
meanAandBPass1 = Module $ entryPoint @"main" @Compute do
  integralPass1Shader @"sharedVec2" @FullSize (aandBfromIsquaredI @FullSize) (Vec2 0 0) (writeValAndSquareToColumnReducedMatrixes @FullSize @"AcolumnReducedMatrix" @"BColumnReducedMatrix") (writeValAndSquareToRowReducedMatrix @FullSize @"AReducedMatrix" @"BRowReducedMatrix")



type IntegralAandBPass2ComputeDef =
  '[ "ubo"  ':-> Uniform '[ DescriptorSet 1, Binding 0 ]
                    ( Struct
                       '[ "line" ':-> Int32,
                        "width" ':-> Word32,
                        "height" ':-> Word32 ]
                    )
    , "sharedVec2" ':-> Workgroup '[] (Array 272 (V 2 Float))
    , "I"  ':-> Image2D '[ DescriptorSet 0, Binding 0 ]
                    ( R32 F )
    , "squaredI"  ':-> Image2D '[ DescriptorSet 0, Binding 1 ]
                    ( R32 F )                    
    , "AcolumnReducedMatrix"  ':-> Image2D '[ DescriptorSet 0, Binding 2 ]
                    ( R32 F )
    , "BColumnReducedMatrix"  ':-> Image2D '[ DescriptorSet 0, Binding 3 ]
                    ( R32 F )                    
    , "AReducedMatrix"  ':-> Image2D '[ DescriptorSet 0, Binding 4 ]
                    ( R32 F )
    , "BRowReducedMatrix"  ':-> Image2D '[ DescriptorSet 0, Binding 5 ]
                    ( R32 F )
    , "Amean"  ':-> Image2D '[ DescriptorSet 0, Binding 6 ]
                    ( R32 F )
    , "Bmean"  ':-> Image2D '[ DescriptorSet 0, Binding 7 ]
                    ( R32 F )                    
    , "main" ':-> EntryPoint '[ LocalSize HalfSize FullSize 1 ]
                    Compute
   ]

meanAandBPass2 :: Module IntegralAandBPass2ComputeDef
meanAandBPass2 = Module $ entryPoint @"main" @Compute do
  imageIntegralPass2Shader @"sharedVec2" @FullSize (aandBfromIsquaredI @FullSize) (rowReducedMatrixCollectorV2 @"AcolumnReducedMatrix" @"BColumnReducedMatrix") (colReducedMatrixCollectorV2 @"AReducedMatrix" @"BRowReducedMatrix") (writeFromSharedMem2Global @FullSize @"sharedVec2" @"Amean" @"Bmean") getV2Zero

------------------------------------------------
-- merge pass


type GuidedFilterLastPassComputeDef =
  '[ "ubo"  ':-> Uniform '[ DescriptorSet 1, Binding 0 ]
                    ( Struct
                       '[ "line" ':-> Int32,
                        "width" ':-> Word32,
                        "height" ':-> Word32 ]
                    )
    , "I"  ':-> Image2D '[ DescriptorSet 0, Binding 0 ]
                    (R32 F)
    , "Amean"  ':-> Image2D '[ DescriptorSet 0, Binding 1 ]
                    ( R32 F )
    , "Bmean"  ':-> Image2D '[ DescriptorSet 0, Binding 2 ]
                    ( R32 F )
    , "output"  ':-> Image2D '[ DescriptorSet 0, Binding 3 ]
                    ( R32 F )                        
    , "main" ':-> EntryPoint '[ LocalSize 16 32 1 ]
                    Compute
   ]

guidedFilterFinal :: Module GuidedFilterLastPassComputeDef
guidedFilterFinal = Module $ entryPoint @"main" @Compute do
    ~(Vec3 u_gx u_gy _) <- get @"gl_GlobalInvocationID"  
    let i_gx = fromIntegral u_gx :: Code Int32
    let i_gy = fromIntegral u_gy :: Code Int32
    meanA <- average @"Amean" i_gx i_gy 10
    meanB <- average @"Bmean" i_gx i_gy 10
    i <- imageRead @"I" (Vec2 i_gx i_gy)
    let res = meanA * i + meanB
    imageWrite @"output" (Vec2 i_gx i_gy) res
  




------------------------------------------------
-- compiling

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
