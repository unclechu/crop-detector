-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/crop-detector/master/LICENSE

module Main where

import System.Environment (getArgs)
import Data.Maybe (isNothing)
import Data.List (find)
import Data.Word (Word8)

import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.IO.DevIL as IL

main :: IO ()
main = do

  [ thresholdArg, origF, cropF ] <- getArgs

  (IL.RGB orig) <- IL.runIL $ IL.readImage origF
  (IL.RGB crop) <- IL.runIL $ IL.readImage cropF

  let (ow, oh) = imgSize orig
      (cw, ch) = imgSize crop

  if cw > ow || ch > oh
     then error $ [ "Cropped image can't have size more than original image.\n"
                  , "Original image size: ", show ow, "x", show oh, ".\n"
                  , "Cropped image size: ",  show cw, "x", show ch, "."
                  ] & foldr1 (++)
     else do
          let threshold = (fromIntegral $ read thresholdArg) :: Float

              origRGB   = getRGBLazyMatrix orig
              cropRGB   = getRGBLazyMatrix crop

              found =
                findCropPos (origRGB, cropRGB) (ow, oh) (cw, ch) threshold

          if isNothing found
             then error "Cropped image not found in original image"
             else do
               let (Just (x, y)) = found
               putStrLn $ [ x, (oh-ch-y), cw, ch ] -- crop parameters "x y w h"
                        & (foldr (\v acc -> show v ++ " " ++ acc) "" ? init)

-- pipes
(&) = flip ($)
(?) = flip (.)

imgSize x = (w, h)
  where (R.Z R.:. h R.:. w R.:. _) = R.extent x

data RGBt = RGBt Word8 Word8 Word8 deriving Show

getRGBLazyMatrix img = [ [ getRGB img x y | y <- [0..] ] | x <- [0..] ]
  where getRGB img x y = RGBt (ch 0) (ch 1) (ch 2)
          where ch n = img R.! (R.Z R.:. y R.:. x R.:. n)

getRGBDiff a b =
  ( (abs $ (cR a) - (cR b))
  + (abs $ (cG a) - (cG b))
  + (abs $ (cB a) - (cB b))
  ) & fromIntegral & (/ 3)
  where cR (RGBt x _ _) = fromIntegral x
        cG (RGBt _ x _) = fromIntegral x
        cB (RGBt _ _ x) = fromIntegral x

hasCropByThisPos (orig, crop) (cw, ch) threshold (x, y) =

  all (all (\(mx, my) -> lowDiff mx my)) [
    [ (mx, my) | my <- ys ] | mx <- xs
  ]

  where xs = [0..(cw-1)]
        ys = [0..(ch-1)]
        lowDiff mx my =
          getRGBDiff (orig !! (x+mx) !! (y+my)) (crop !! mx !! my)
                   & (<= threshold)


findCropPos (orig, crop) (ow, oh) (cw, ch) threshold =
  find doWeHaveCropHere posMatrix
  where posMatrix = [ (x, y) | x <- xs, y <- ys ]
        xs = [0..(ow-cw-1)]
        ys = [0..(oh-ch-1)]
        doWeHaveCropHere = hasCropByThisPos (orig, crop) (cw, ch) threshold
