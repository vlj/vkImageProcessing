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

module SimpleIntegralShader(naiveHorizontalInclusiveScan, naiveVerticalInclusiveScan) where

-- base
import qualified Prelude
import Prelude
  ( Functor, map )
import Control.Monad
  ( void )
import Data.Maybe
  ( fromJust )

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

-- Given a 2D image "input", sum over one dimension.
-- dimensionLimitSelect returns how much elements constitues a lane given an image size.
-- coordinateBuilder x y returns the 2D coordinate in input for the y-th element of the x-th lane.
blockedSumLoop :: forall (blockSize::Nat) (i :: ProgramState). (KnownNat  blockSize, _) => (Code (V 2 Word32) -> Code Word32) -> (Code Word32 -> Code Word32 -> Code (V 2 Word32)) -> Program i i (Code ())
blockedSumLoop dimensionLimitSelect coordinateBuilder =
  locally do
    let subgroupSize = cast (natVal (Proxy @blockSize))
    ~(Vec3 concurrentlaneid _ _) <- get @"gl_WorkgroupID"
    _ <- def @"subgrouplane" @R =<< ((+) $ concurrentlaneid * subgroupSize)<<$>> get @"gl_SubgroupID"
    _ <- def @"startingindex" @RW @Word32 0
    _ <- def @"acc" @RW @Float 0
    dimensionLimit <- dimensionLimitSelect <<$>> imageSize @"input" @(V 2 Word32)

    while (get @"startingindex" < pure dimensionLimit) do
      let currentIndex = (+) <<$>> get @"startingindex" <<*>> get @"gl_SubgroupInvocationID"
      coordinate <- coordinateBuilder <<$>> get @"subgrouplane" <<*>> currentIndex
      texel <- imageRead @"input" coordinate
      
      imageWrite @"output" coordinate =<< (+) <<$>> get @"acc" <<*>> groupAdd @Subgroup @InclusiveScan texel
      modify @"acc" =<< (+) <<$>> groupAdd @Subgroup @Reduce texel
      modify @"startingindex" (+subgroupSize)

type NaiveRowSumDefBlockSize = 32

type NaiveRowSumDefs =
  '[
      "input"  ':-> Image2D '[ DescriptorSet 0, Binding 0 ]
                    ( R32 F )                    
    , "output"  ':-> Image2D '[ DescriptorSet 0, Binding 1 ]
                    ( R32 F )
    , "main" ':-> EntryPoint '[ LocalSize NaiveRowSumDefBlockSize NaiveRowSumDefBlockSize 1 ]
                    Compute
   ]

-- Inclusive scan over the horizontal axis of an image
naiveHorizontalInclusiveScan :: Module NaiveRowSumDefs
naiveHorizontalInclusiveScan = Module $ entryPoint @"main" @Compute do
  blockedSumLoop @NaiveRowSumDefBlockSize (\(Vec2 x y) -> x) Vec2

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


-- Inclusive scan over the vertical axis of an image
naiveVerticalInclusiveScan :: Module NaiveColumnSumDefs
naiveVerticalInclusiveScan = Module $ entryPoint @"main" @Compute do
  blockedSumLoop @NaiveRowSumDefBlockSize (\(Vec2 x y) -> x) (flip Vec2)

