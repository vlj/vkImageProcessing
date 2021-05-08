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

module FirstImplem where

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
-- First implem

type ComputeDefs =
  '[ "ubo"  ':-> Uniform '[ DescriptorSet 1, Binding 0 ]
                    ( Struct
                       '[ "line" ':-> Int32,
                        "width" ':-> Word32,
                        "height" ':-> Word32 ]
                    )
    , "sharedfloat" ':-> Workgroup '[] (Array 1024 Float)
    , "input"  ':-> Image2D '[ DescriptorSet 0, Binding 0 ]
                    ( R32 F )                    
    , "output"  ':-> Image2D '[ DescriptorSet 0, Binding 1 ]
                    ( R32 F )
    , "main" ':-> EntryPoint '[ LocalSize 16 32 1 ]
                    Compute
   ]

transposeShader :: Module ComputeDefs
transposeShader = Module $ entryPoint @"main" @Compute do

    ~(Vec3 i_x i_y _) <- get @"gl_LocalInvocationID"
    ~(Vec3 i_groupIDx _ _) <- get @"gl_WorkgroupID"

    line <- use @(Name "ubo" :.: Name "line")

    --loadToSharedMem i_x i_y (fromIntegral i_groupIDx) $ line - (fromIntegral i_groupIDx) 
    transposeSharedMemory @"sharedfloat" i_x i_y
    writeFromSharedMem @"output" i_x i_y (fromIntegral i_groupIDx) $ line - (fromIntegral i_groupIDx) 


partialHorizontalPrefixSumShader :: Module ComputeDefs
partialHorizontalPrefixSumShader = Module $ entryPoint @"main" @Compute do

    ~(Vec3 i_x i_y _) <- get @"gl_LocalInvocationID"
    ~(Vec3 i_groupIDx _ _) <- get @"gl_WorkgroupID"

    line <- use @(Name "ubo" :.: Name "line")

    --loadToSharedMem i_x i_y (fromIntegral i_groupIDx) $ line - (fromIntegral i_groupIDx) 

    inclusivePrefixSumInSharedMem @"sharedfloat" @32 (index2dTo1d @32) i_x i_y
    writeFromSharedMem @"output" i_x i_y (fromIntegral i_groupIDx) $ line - (fromIntegral i_groupIDx) 

partialVerticalPrefixSumShader :: Module ComputeDefs
partialVerticalPrefixSumShader = Module $ entryPoint @"main" @Compute do

    ~(Vec3 i_x i_y _) <- get @"gl_LocalInvocationID"
    ~(Vec3 i_groupIDx _ _) <- get @"gl_WorkgroupID"

    line <- use @(Name "ubo" :.: Name "line")

    --loadToSharedMem i_x i_y (fromIntegral i_groupIDx) $ line - (fromIntegral i_groupIDx)
    transposeSharedMemory @"sharedfloat" i_x i_y
    inclusivePrefixSumInSharedMem @"sharedfloat" @32 (index2dTo1d @32) i_x i_y
    transposeSharedMemory @"sharedfloat" i_x i_y
    writeFromSharedMem @"output" i_x i_y (fromIntegral i_groupIDx) $ line - (fromIntegral i_groupIDx) 


horizontalPrefixSumShader :: Module ComputeDefs
horizontalPrefixSumShader = Module $ entryPoint @"main" @Compute do

    ~(Vec3 i_x i_y _) <- get @"gl_LocalInvocationID"
    ~(Vec3 u_groupIDx _ _) <- get @"gl_WorkgroupID"

    line <- use @(Name "ubo" :.: Name "line")

    let i_groupIDx = (fromIntegral u_groupIDx)

    --loadToSharedMem i_x i_y i_groupIDx $ line - i_groupIDx

    inclusivePrefixSumInSharedMem @"sharedfloat" @32 (index2dTo1d @32) i_x i_y
    writeFromSharedMem @"output" i_x i_y i_groupIDx $ line - i_groupIDx

verticalPrefixSumShader :: Module ComputeDefs
verticalPrefixSumShader = Module $ entryPoint @"main" @Compute do

    ~(Vec3 i_x i_y _) <- get @"gl_LocalInvocationID"
    ~(Vec3 i_groupIDx _ _) <- get @"gl_WorkgroupID"

    line <- use @(Name "ubo" :.: Name "line")

    --loadToSharedMem i_x i_y (fromIntegral i_groupIDx) $ line - (fromIntegral i_groupIDx)
    transposeSharedMemory @"sharedfloat" i_x i_y
    inclusivePrefixSumInSharedMem @"sharedfloat" @32 (index2dTo1d @32) i_x i_y
    transposeSharedMemory @"sharedfloat" i_x i_y
    writeFromSharedMem @"output" i_x i_y (fromIntegral i_groupIDx) $ line - (fromIntegral i_groupIDx) 

imageIntegralShader :: Module ComputeDefs
imageIntegralShader = Module $ entryPoint @"main" @Compute do
    ~(Vec3 i_x i_y _) <- get @"gl_LocalInvocationID"
    ~(Vec3 i_groupIDx _ _) <- get @"gl_WorkgroupID"

    line <- use @(Name "ubo" :.: Name "line")

    --loadToSharedMem i_x i_y (fromIntegral i_groupIDx) $ line - (fromIntegral i_groupIDx)
    inclusivePrefixSumInSharedMem @"sharedfloat" @32 (index2dTo1d @32) i_x i_y
    transposeSharedMemory @"sharedfloat" i_x i_y
    inclusivePrefixSumInSharedMem @"sharedfloat" @32 (index2dTo1d @32) i_x i_y
    transposeSharedMemory @"sharedfloat" i_x i_y

    --sumBorders @32 i_x i_y (fromIntegral i_groupIDx) $ line - (fromIntegral i_groupIDx)
    --sumBorders @32 (i_x + 16) i_y (fromIntegral i_groupIDx) $ line - (fromIntegral i_groupIDx)
    writeFromSharedMem @"output" i_x i_y (fromIntegral i_groupIDx) $ line - (fromIntegral i_groupIDx) 