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

module SimpleIntegralShader where

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

type NaiveRowSumDefBlockSize = 32

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
    , "main" ':-> EntryPoint '[ LocalSize NaiveRowSumDefBlockSize NaiveRowSumDefBlockSize 1 ]
                    Compute
   ]

naiveRowSumShader :: Module NaiveRowSumDefs
naiveRowSumShader = Module $ entryPoint @"main" @Compute do
    let subgroupSize = cast (natVal (Proxy @NaiveRowSumDefBlockSize))
    ~(Vec3 i_x _ _) <- get @"gl_WorkgroupID"
    _ <- def @"lineindex" @R =<< ((+) $ i_x * subgroupSize)<<$>> get @"gl_SubgroupID"

    _ <- def @"startingindex" @RW @Word32 0
    width <- use @(Name "ubo" :.: Name "width")
    height <- use @(Name "ubo" :.: Name "height")
    w <- imageSize @"input" @(V 2 Word32)

    _ <- def @"acc" @RW @Float 0
    while (get @"startingindex" < pure width) do
      lineindex <- get @"lineindex"
      subgroupInvocationID <- get @"gl_SubgroupInvocationID"
      coordinate <- (\x -> Vec2 lineindex (x + subgroupInvocationID)) <<$>> get @"startingindex"
      texel <- imageRead @"input" coordinate
      accumulatedLocalTexel <- nonUniformGroupAdd @Subgroup @InclusiveScan texel
      imageWrite @"output" coordinate =<< (+accumulatedLocalTexel) <<$>> get @"acc"
      accumulatedAllTexel <- nonUniformGroupAdd @Subgroup @Reduce texel
      modify @"acc" (+ accumulatedAllTexel)
      modify @"startingindex" (+subgroupSize)

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
    , "main" ':-> EntryPoint '[ LocalSize NaiveRowSumDefBlockSize NaiveRowSumDefBlockSize 1 ]
                    Compute
   ]

naiveColumnSumShader :: Module NaiveColumnSumDefs
naiveColumnSumShader = Module $ entryPoint @"main" @Compute do
    ~(Vec3 i_x _ _) <- get @"gl_WorkgroupID"
    let subgroupSize = 32
    colindex <- def @"colindex" @R =<< ((+) $ i_x * subgroupSize)<<$>> get @"gl_SubgroupID"

    _ <- def @"startindex" @RW @Word32 0
    width <- use @(Name "ubo" :.: Name "width")
    height <- use @(Name "ubo" :.: Name "height")

    _ <- def @"acc" @RW @Float 0
    while (get @"startindex" < pure height) do
      startindex <- get @"startindex"
      readindex <- (+startindex) <<$>> get @"gl_SubgroupInvocationID"
      let coordinate = (Vec2 readindex colindex)
      texel <- imageRead @"input" coordinate
      acc <- get @"acc"
      imageWrite @"output" coordinate =<< (+acc) <<$>> nonUniformGroupAdd @Subgroup @InclusiveScan texel
      accumulatedAllTexel <- nonUniformGroupAdd @Subgroup @Reduce texel      
      modify @"acc" (+ accumulatedAllTexel)
      modify @"startindex" (+subgroupSize)

