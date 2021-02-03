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
module Common
  ( cast,
    index2dTo1d,
    integralPass1SharedMem,
    integralPass1Subgroup,
    imageIntegralPass2Shader,
    average,
  )
where

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

import GHC.TypeNats
  (Log2, Div)

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

import Data.Vector.Sized


cast x = fromInteger $ Prelude.toInteger x

index2dTo1d :: forall (blockEdge :: Nat) a. (KnownNat blockEdge, Semiring a) => a -> a -> a
index2dTo1d x y =
  let stride = cast (natVal (Proxy @blockEdge)) in
    x + (stride + 1) * y


class Summer a where
  summing :: a -> a -> a

instance Summer Float where
  summing x y = x + y

instance Summer (Code Float) where
  summing x y = x + y  

instance KnownNat n => Summer (V n Float) where
  summing x y = x ^+^ y  

instance KnownNat n => Summer (Code (V n Float)) where
  summing x y = x ^+^ y  


upsweepPhase
  :: forall (sharedName :: Symbol) (blockEdge :: Nat) (s :: ProgramState). (_)
  => (Code Int32 -> Code Int32 -> Code Int32) ->
     Code Word32 -> Code Word32 ->
     Program s s  ()
upsweepPhase index2dTo1d x y = locally do
  let i_y = (fromIntegral y) :: Code Int32
  _ <- def @"current_x" @RW $ (fromIntegral (2 * x + 1) :: Code Int32)
  _ <- def @"curr_offset" @RW @Int32 1
  _ <- def @"level" @RW @Word32 0
  

  while (get @"level" < cast (natVal (Proxy  @(Log2 blockEdge)))) do
    current_x <- get @"current_x"
    curr_offset <- get @"curr_offset"
    let write_idx = current_x + curr_offset - 1
    let read_idx = current_x - 1
    
    if (write_idx < cast (natVal (Proxy @blockEdge))) then do
        read_value <- use @(Name sharedName :.: AnIndex Int32) $ index2dTo1d read_idx i_y
        write_value <- use @(Name sharedName :.: AnIndex Int32) $ index2dTo1d write_idx i_y
        assign @(Name sharedName :.: AnIndex Int32) (index2dTo1d write_idx i_y) $ summing read_value write_value
        pure ()
      else do
        pure ()
    
    controlBarrier Workgroup Nothing

    modify @"current_x" (2 *)
    modify @"curr_offset" (2 *)
    modify @"level" (1+)
  pure ()

downsweepPhase :: forall (sharedName :: Symbol) (blockEdge :: Nat) (s :: ProgramState). (_)
  => (Code Int32 -> Code Int32 -> Code Int32) ->
     Code Word32 -> Code Word32 ->
     Program s s  ()
downsweepPhase index2dTo1d x y = locally do
  let i_y = (fromIntegral y) :: Code Int32
  _ <- def @"stride" @RW @Int32 (cast $ natVal $ Proxy @(Div blockEdge 2))

  while (get @"stride" > 0) do
    stride <- get @"stride"
    let index = (fromIntegral x + 1) * stride * 2 - 1
    
    if (index + stride < cast (natVal $ Proxy @blockEdge)) then do
        temp <- use @(Name sharedName :.: AnIndex Int32) $ index2dTo1d (index + stride) i_y
        read_val <- use @(Name sharedName :.: AnIndex Int32) $ index2dTo1d index i_y
        assign @(Name sharedName :.: AnIndex Int32) (index2dTo1d (index + stride) i_y) $ summing temp read_val
        pure ()
      else do
        pure ()
    
    controlBarrier Workgroup Nothing

    put @"stride" (div stride  2)
  pure ()


inclusivePrefixSumInSharedMem :: forall (sharedName :: Symbol) (blockEdge :: Nat) (s :: ProgramState). (_ )
  =>  (Code Int32 -> Code Int32 -> Code Int32) ->
     Code Word32 -> Code Word32 ->
     Program s s ()
inclusivePrefixSumInSharedMem index2dTo1d i_x i_y = locally do
    upsweepPhase @sharedName @blockEdge index2dTo1d i_x i_y
    downsweepPhase @sharedName @blockEdge index2dTo1d i_x i_y

sumBorders :: forall (blockEdge :: Nat) (s::ProgramState). (_) => Code Word32 -> Code Word32 -> Code Int32 -> Code Int32 -> Program s s ()
sumBorders u_x u_y blockIDx blockIDy = locally do
    let blockEdgeVal = cast $ natVal $ Proxy @blockEdge
    let i_gx = blockEdgeVal * blockIDx + fromIntegral u_x
    let i_gy = blockEdgeVal * blockIDy + fromIntegral u_y
    topLeftCornerV4 <- imageRead @"output" (Vec2 (blockEdgeVal * blockIDx - 1) (blockEdgeVal * blockIDy - 1))
    leftBorderV4 <- imageRead @"output" (Vec2 (blockEdgeVal * blockIDx - 1) i_gy)
    topBorderV4 <- imageRead @"output" (Vec2 i_gx (blockEdgeVal * blockIDy - 1))
    let topLeftCorner = topLeftCornerV4
    let leftBorder = leftBorderV4
    let topBorder = topBorderV4

    prevVal <- use @(Name "sharedfloat" :.: AnIndex Word32) (index2dTo1d @32 u_x u_y)
    assign @(Name "sharedfloat" :.: AnIndex Word32) (index2dTo1d @32 u_x u_y) (prevVal + topLeftCorner - leftBorder - topBorder)
    pure () 



loadToSharedMem
  :: forall (s :: ProgramState). ( _ )
  => Code Word32 -> Code Word32 ->
     Code Int32 -> Code Int32 ->
     Program s s (Code ())
loadToSharedMem i_x i_y blockIDx blockIDy = locally do
    let i_gx = 32 * blockIDx + fromIntegral i_x
    let i_gy = 32 * blockIDy + fromIntegral i_y
    let idx = index2dTo1d @32 i_x i_y
    let idxp16 = index2dTo1d @32 (i_x + 16) i_y

    indexcontent <- imageRead @"input" ( Vec2 i_gx i_gy )
    assign @(Name "sharedfloat" :.: AnIndex Word32) idx $ (view @(Swizzle "x") indexcontent) 
    indexcontent2 <- imageRead @"input" ( Vec2  (i_gx + 16)  i_gy  )
    assign @(Name "sharedfloat" :.: AnIndex Word32) idxp16 (view @(Swizzle "x") indexcontent2 )
    controlBarrier Workgroup Nothing


writeFromSharedMem
  :: forall (image::Symbol) (s :: ProgramState). ( _ )
  => Code Word32 -> Code Word32 ->
     Code Int32 -> Code Int32 ->
     Program s s (Code ())
writeFromSharedMem i_x i_y blockIDx blockIDy = locally do
    let i_gx = 32 * blockIDx + fromIntegral i_x
    let i_gy = 32 * blockIDy + fromIntegral i_y
    let idx = index2dTo1d @32 i_x i_y
    let idxp16 = index2dTo1d @32 (i_x + 16) i_y

    tmp <- use @(Name "sharedfloat" :.: AnIndex Word32) idx
    imageWrite @image ( Vec2 i_gx i_gy ) (Vec4 tmp tmp tmp tmp)    
    tmp <- use @(Name "sharedfloat" :.: AnIndex Word32) idxp16
    imageWrite @image ( Vec2(i_gx + 16) i_gy ) (Vec4 tmp tmp tmp tmp)

writeBottomRightBorderFromSharedMem :: forall (image::Symbol) (s :: ProgramState). ( _ )
  => Code Word32 -> Code Word32 ->
     Code Int32 -> Code Int32 ->
     Program s s (Code ())
writeBottomRightBorderFromSharedMem i_x i_y blockIDx blockIDy = locally do
    let i_gx = 32 * blockIDx + fromIntegral i_x
    let i_gy = 32 * blockIDy + fromIntegral i_y
    let idx = index2dTo1d @32 i_x i_y
    let idxp16 = index2dTo1d @32 (i_x + 16) i_y

    when (i_y == 31) do
      tmp <- use @(Name "sharedfloat" :.: AnIndex Word32) idx
      imageWrite @image ( Vec2 i_gx i_gy ) (Vec4 tmp tmp tmp tmp)    
      tmp <- use @(Name "sharedfloat" :.: AnIndex Word32) idxp16
      imageWrite @image ( Vec2(i_gx + 16) i_gy ) (Vec4 tmp tmp tmp tmp)
    when (i_x == 15) do
      tmp <- use @(Name "sharedfloat" :.: AnIndex Word32) idxp16
      imageWrite @image ( Vec2(i_gx + 16) i_gy ) (Vec4 tmp tmp tmp tmp)

swapSharedMemory :: forall (sharedArrayName :: Symbol) (s :: ProgramState). ( _ )
  => Code Word32 -> Code Word32 -> Program s s ()
swapSharedMemory i_x i_y =
  locally do
    if  i_x < i_y then do
      let idxy = index2dTo1d @32 i_x i_y
      let idyx = index2dTo1d @32 i_y i_x
      shared_xy <- use @(Name sharedArrayName :.: AnIndex Word32) idxy
      _ <- def @"tmp" @R shared_xy
      assign @(Name sharedArrayName :.: AnIndex Word32) idxy =<< use @(Name sharedArrayName :.: AnIndex Word32) idyx
      assign @(Name sharedArrayName :.: AnIndex Word32) idyx =<< get @"tmp"
      pure ()
    else do
      pure ()

transposeSharedMemory :: forall  (sharedName::Symbol) (s :: ProgramState). ( _ )
  => Code Word32 -> Code Word32 ->
     Program s s (Code ())
transposeSharedMemory i_x i_y = locally do
  swapSharedMemory @sharedName i_x i_y
  swapSharedMemory @sharedName (i_x + 16) i_y 
  controlBarrier Workgroup Nothing

  
getPreviousHorizontalBlock :: forall (s0 :: ProgramState). ( _ )
  => () -> Program s0 s0 (Code Float)
getPreviousHorizontalBlock _ = locally do
    ~(Vec3 _ u_y _) <- get @"gl_LocalInvocationID"
    ~(Vec3 u_groupIDx _ _) <- get @"gl_WorkgroupID"
    
    line <- use @(Name "ubo" :.: Name "line")
    let i_groupIDx = (fromIntegral u_groupIDx)
    let i_y = fromIntegral u_y
    tmp <- imageRead @"output" $ Vec2 (32 * i_groupIDx - 1) (32 * (line - i_groupIDx) + i_y)
    return $ view @(Swizzle "x") tmp


getPreviousVerticalBlock :: forall (s0 :: ProgramState). ( _ )
  => () -> Program s0 s0 (Code Float)
getPreviousVerticalBlock _ = locally do
    ~(Vec3 _ u_y _) <- get @"gl_LocalInvocationID"
    ~(Vec3 u_groupIDx _ _) <- get @"gl_WorkgroupID"
    
    line <- use @(Name "ubo" :.: Name "line")
    let i_groupIDx = (fromIntegral u_groupIDx)
    let i_y = fromIntegral u_y
    tmp <- imageRead @"output" $ Vec2 (32 * i_groupIDx + i_y) (32 * (line - i_groupIDx) - 1)
    return $ view @(Swizzle "x") tmp    


getLineSum :: forall (sharedName::Symbol) a (s0 :: ProgramState). ( _ )
  => Code Word32 -> Program s0 s0 (Code a)
getLineSum line = locally do
    _ <- def @"i" @RW @Word32 0
    _ <- def @"acc" @RW @a 0
    while (get @"i" < 32) do
      i <- get @"i"
      texVal <- use @(Name sharedName :.: AnIndex Word32) $ index2dTo1d @32 i line
      modify @"acc" (+ texVal)
      modify @"i" (+1)
    acc <- get @"acc"
    return acc



getZero
  :: forall (s0 :: ProgramState). ( _ )
  => () -> Program s0 s0 (Code Float)
getZero _ = locally do
  return 0


--------------------------------------------------------------
-- Second impl


inputToShared :: forall (blockEdge :: Nat) (sharedMem :: Symbol) (s::ProgramState) . (_) =>
  (Code Int32 -> Code Int32 -> Program s s (Code (V 2 Float))) ->
  Code Int32 -> Code Int32 -> Program s s (Code ())
inputToShared inputImageGetter i_groupIDx i_groupIDy = locally do
    ~(Vec3 i_x i_y _) <- get @"gl_LocalInvocationID"
    let blockEdgeVal = cast (natVal (Proxy @blockEdge))
    let halfBlockEdgeVal = cast (natVal (Proxy @(Div blockEdge 2)))
    let halfBlockEdgeVal2 = cast (natVal (Proxy @(Div blockEdge 2)))
    
    let i_gx = blockEdgeVal * i_groupIDx + fromIntegral i_x
    let i_gy = blockEdgeVal * i_groupIDy + fromIntegral i_y
    let idx = index2dTo1d @blockEdge i_x i_y
    let idxp16 = index2dTo1d @blockEdge (i_x + halfBlockEdgeVal) i_y

    assign @(Name sharedMem :.: AnIndex Word32) idx =<< (inputImageGetter i_gx i_gy)
    assign @(Name sharedMem :.: AnIndex Word32) idxp16 =<< (inputImageGetter (i_gx + halfBlockEdgeVal2) i_gy)
    controlBarrier Workgroup Nothing


class ZeroGetter a where
  initialVal :: Code a

instance ZeroGetter (V 2 Float ) where
  initialVal = (Vec2 0 0)    

getLineSum2 :: forall (sharedName::Symbol) (blockEdge :: Nat) a (s0 :: ProgramState). (Summer a, ( _ ))
  => (Code Word32 -> Code Word32 -> Code Word32) ->
     Code Word32 -> Program s0 s0 (Code a)
getLineSum2 index2dTo1d line = locally do
    _ <- def @"i" @RW @Word32 0
    acc <- def @"acc" @RW initialVal
    while (get @"i" < cast (natVal (Proxy @blockEdge))) do
      i <- get @"i"
      texVal <- use @(Name sharedName :.: AnIndex Word32) $ index2dTo1d i line
      modify @"acc" (summing texVal)
      modify @"i" (+1)
    acc <- get @"acc"
    return acc


-- Integral image algorithm based on "A fast integral image generation algorithm on GPUs" by Dang and al.
-- Integral image is a 5 step process:
-- - Compute row sum and column sum of every block and put it into "reduced rows" and "reduced cols" image.
-- - Inclusive scan of reduced row image.
-- - Inclusive scan of reduced col image.
-- - Inclusive scan of the inclusively scanned reduced col image.
-- - Last step do inclusive scan of every block, adding the col and row offset from the inclusively scanned row and col image.

-- integralPass1* takes 3 functions as argument:
-- - loadInput which loads the content of the input image.
-- - writeToColumnReducedMatrix which write a row to the column reduced image.
-- - writeToRowReducedMatrix which write a row to the row reduced image.

-- Implementation of the first pass of the algorithm, uses shared memory
integralPass1SharedMem :: forall (sharedName::Symbol) (blockEdge :: Nat) a (s::ProgramState) . (_) =>
  (Code Int32 -> Code Int32 -> Program s s (Code (V 2 Float))) ->
  (Code Word32 -> Code Word32 -> Code Word32 -> Code a -> Program s s (Code ())) ->
  (Code Word32 -> Code Word32 -> Code Word32 -> Code a -> Program s s (Code ())) ->
  Program s s (Code ())
integralPass1SharedMem loadInput writeToColumnReducedMatrix writeToRowReducedMatrix = locally do
    ~(Vec3 i_x i_y _) <- get @"gl_LocalInvocationID"
    ~(Vec3 i_groupIDx i_groupIDy _) <- get @"gl_WorkgroupID"

    inputToShared @blockEdge @sharedName loadInput (fromIntegral i_groupIDx) (fromIntegral i_groupIDy)
    when (i_x == 0) do
      horizontalSum <- getLineSum2 @sharedName @blockEdge (index2dTo1d @blockEdge) i_y
      writeToColumnReducedMatrix i_groupIDx i_groupIDy i_y horizontalSum
    
    controlBarrier Workgroup Nothing

    when (i_x == 0) do    
      verticalSum <- getLineSum2 @sharedName @blockEdge (flip (index2dTo1d @blockEdge)) i_y
      writeToRowReducedMatrix i_groupIDx i_groupIDy i_y verticalSum

integralPass1Subgroup :: forall (blockEdge :: Nat) a (s::ProgramState) (s2::ProgramState) (s3::ProgramState). (_) =>
  (Code Int32 -> Code Int32 -> Program s2 s2 (Code a)) ->
  (Code Word32 -> Code Word32 -> Code Word32 -> Code a -> Program s2 s2 (Code ())) ->
  (Code Word32 -> Code Word32 -> Code Word32 -> Code a -> Program s3 s3 (Code ())) ->
  Program s s (Code ())
integralPass1Subgroup loadInput writeToColumnReducedMatrix writeToRowReducedMatrix = locally do
    ~(Vec3 i_groupIDx i_groupIDy _) <- get @"gl_WorkgroupID"

    let blockSize = cast (natVal (Proxy @blockEdge))

    _ <- def @"columnValue" @RW $ (undefined :: Code (Array blockEdge a))
    _ <- def @"i" @RW @Word32 0
    gl_SubgroupInvocationID <- get @"gl_SubgroupInvocationID"
    while (get @"i" < pure blockSize) do
      i <- get @"i"
      let coordx = fromIntegral $ blockSize * i_groupIDx + i
      let coordy = fromIntegral $ blockSize * i_groupIDy + gl_SubgroupInvocationID
      input <- loadInput coordx coordy
      assign @(Name "columnValue" :.: AnIndex Word32) i input
      summedInput <- groupAdd @'Subgroup @Reduce input
      when (gl_SubgroupInvocationID == 0) do
        writeToColumnReducedMatrix i_groupIDx i_groupIDy i summedInput
      modify @"i" (+1)
    
    _ <- def @"acc" @RW @a initialVal 
    _ <- def @"j" @RW @Word32 0
    while (get @"j" < pure blockSize) do
      modify @"acc" =<< ((^+^) <<$>> (use @(Name "columnValue" :.: AnIndex Word32) =<< get @"j"))
      modify @"j" (+1)
    writeToRowReducedMatrix i_groupIDx i_groupIDy gl_SubgroupInvocationID =<< get @"acc"


sumColFromRowMatrix :: forall (sharedName :: Symbol) (blockEdge :: Nat) a (s::ProgramState). (Summer a, _) =>
  (Code Int32 -> Code Int32 -> Program s s (Code a)) ->
  Code Word32 -> Code Word32 -> Code Int32 -> Code Int32 -> Program s s ()
sumColFromRowMatrix imageCollector u_x u_y blockIDx blockIDy = locally do
    let blockEdgeVal = cast (natVal (Proxy @blockEdge))
    let i_gx = blockEdgeVal * blockIDx + fromIntegral u_x
    let i_gy = blockEdgeVal * blockIDy + fromIntegral u_y
    valueFromRow <- imageCollector i_gx (blockIDy - 1)

    prevVal <- use @(Name sharedName :.: AnIndex Word32) (index2dTo1d @blockEdge u_x u_y)
    assign @(Name sharedName :.: AnIndex Word32) (index2dTo1d @blockEdge u_x u_y) (summing prevVal valueFromRow)
    pure ()

sumRowFromColMatrix :: forall (sharedName :: Symbol) (blockEdge :: Nat) a (s::ProgramState). (Summer a, _) =>
 (Code Int32 -> Code Int32 -> Program s s (Code a)) ->
 Code Word32 -> Code Word32 -> Code Int32 -> Code Int32 -> Program s s ()
sumRowFromColMatrix imageCollector u_x u_y blockIDx blockIDy = locally do
    let blockEdgeVal = cast (natVal (Proxy @blockEdge))  
    let i_gx = blockEdgeVal * blockIDx + fromIntegral u_x
    let i_gy = blockEdgeVal * blockIDy + fromIntegral u_y
    valueFromCol <- imageCollector (blockIDx - 1) i_gy

    prevVal <- use @(Name sharedName :.: AnIndex Word32) (index2dTo1d @blockEdge u_x u_y)
    assign @(Name sharedName :.: AnIndex Word32) (index2dTo1d @blockEdge u_x u_y) (summing prevVal valueFromCol)
    pure ()

imageIntegralPass2Shader :: forall (sharedName::Symbol) (blockEdge :: Nat) a (s::ProgramState). (Summer a, _) =>
  (Code Int32 -> Code Int32 -> Program s s (Code (V 2 Float))) ->
  (Code Int32 -> Code Int32 -> Program s s (Code a)) ->
  (Code Int32 -> Code Int32 -> Program s s (Code a)) ->
  (Code Word32 -> Code Word32 -> Code Int32 -> Code Int32 -> Program s s (Code ())) ->
  (() -> Program s s (Code a)) ->
  Program s s (Code ())
imageIntegralPass2Shader loadInput rowReducedMatrixCollector colReducedMatrixCollector writeFromSharedMemGeneric getZeroz = locally do
    let halfBlockEdgeVal = cast (natVal (Proxy @(Div blockEdge 2)))    
    ~(Vec3 i_x i_y _) <- get @"gl_LocalInvocationID"
    ~(Vec3 i_groupIDx i_groupIDy _) <- get @"gl_WorkgroupID"


    inputToShared @blockEdge @sharedName loadInput (fromIntegral i_groupIDx) (fromIntegral i_groupIDy)
    inclusivePrefixSumInSharedMem @sharedName @blockEdge (index2dTo1d @blockEdge) i_x i_y

    sumRowFromColMatrix @sharedName @blockEdge rowReducedMatrixCollector  i_x i_y (fromIntegral i_groupIDx) (fromIntegral i_groupIDy)
    sumRowFromColMatrix @sharedName @blockEdge rowReducedMatrixCollector  (i_x + halfBlockEdgeVal) i_y (fromIntegral i_groupIDx) (fromIntegral i_groupIDy)

    controlBarrier Workgroup Nothing

    inclusivePrefixSumInSharedMem  @sharedName @blockEdge (flip (index2dTo1d @blockEdge)) i_x i_y

    sumColFromRowMatrix @sharedName @blockEdge colReducedMatrixCollector i_x i_y (fromIntegral i_groupIDx) (fromIntegral i_groupIDy)
    sumColFromRowMatrix @sharedName @blockEdge colReducedMatrixCollector (i_x + halfBlockEdgeVal) i_y (fromIntegral i_groupIDx) (fromIntegral i_groupIDy)

    writeFromSharedMemGeneric i_x i_y (fromIntegral i_groupIDx) (fromIntegral i_groupIDy)  

average::forall (imageName :: Symbol) a (s::ProgramState) . (_) =>
  Code Int32 -> Code Int32 -> Int32 -> Program s s (Code a)
average i_gx i_gy _radius = locally do

    let radius = Lit _radius

    topLeftCorner <- imageRead @imageName (Vec2 (i_gx - radius) (i_gy - radius))
    topRightCorner <- imageRead @imageName (Vec2 (i_gx + radius) (i_gy - radius))
    bottomLeftCorner <- imageRead @imageName (Vec2 (i_gx - radius) (i_gy + radius))
    bottomRightCorner <- imageRead @imageName (Vec2 (i_gx + radius) (i_gy + radius))

    let surface = (fromIntegral $ (_radius + _radius + 1) * (_radius + _radius + 1) ) :: a

    return $ div (topLeftCorner + bottomRightCorner - bottomLeftCorner - topRightCorner) (Lit surface)