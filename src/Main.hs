-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/crop-detector/master/LICENSE

module Main where

import System.Environment (getArgs)
import Data.List (find, minimumBy)
import Data.Word (Word8)
import Data.Maybe (isJust, mapMaybe)

import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.IO.DevIL as IL

main :: IO ()
main = do

  [ modeArg, thresholdArg, origF, cropF ] <- getArgs

  (IL.RGB orig) <- IL.runIL $ IL.readImage origF
  (IL.RGB crop) <- IL.runIL $ IL.readImage cropF

  let (ow, oh)  = imgSize orig
      (cw, ch)  = imgSize crop
      threshold = fromIntegral (read thresholdArg) :: Float
      mode      = case modeArg of
                       "every-pixel"         -> [EveryPixelMode]
                       "perfect-every-pixel" -> [Perfect, EveryPixelMode]
                       "average"             -> [AverageMode]
                       "perfect-average"     -> [Perfect, AverageMode]
                       _                     -> error "Unknown mode"

  if cw > ow || ch > oh
     then error $ foldr1 (++)
                  [ "Cropped image can't have size more than original image.\n"
                  , "Original image size: ", show ow, "x", show oh, ".\n"
                  , "Cropped image size: ",  show cw, "x", show ch, "."
                  ]
     else let pairRGB   = (getRGBLazyMatrix orig, getRGBLazyMatrix crop)
              foundCrop = findCropPos pairRGB (ow, oh) (cw, ch) mode threshold
          in case foundCrop of
               Nothing     -> error "Cropped image not found in original image"
               Just (x, y) ->
                   -- print crop parameters "x y w h"
                   putStrLn $ init
                            $ foldr (\v acc -> show v ++ " " ++ acc) ""
                                    [ x, oh-ch-y, cw, ch ]

data RGBt = RGBt Word8 Word8 Word8 deriving (Show)
data Mode = EveryPixelMode
          | AverageMode
          | Perfect
            deriving (Enum, Show)

imgSize x = (w, h)
  where (R.Z R.:. h R.:. w R.:. _) = R.extent x

getRGBLazyMatrix img = [ [ getRGB img x y | y <- [0..] ] | x <- [0..] ]
  where getRGB img x y = RGBt (ch 0) (ch 1) (ch 2)
          where ch n = img R.! (R.Z R.:. y R.:. x R.:. n)

getRGBDiff a b = (diff cR + diff cG + diff cB) / 3
  where cR (RGBt x _ _) = fromIntegral x
        cG (RGBt _ x _) = fromIntegral x
        cB (RGBt _ _ x) = fromIntegral x
        diff f = abs $ f a - f b

hasCropByThisPos f (x, y) = isJust $ f (x, y)

-- threshold should be 0..255
cropByThisPos (orig, crop) (cw, ch) mode threshold (x, y) =

  case mode of

       [EveryPixelMode] ->
         if everyPixelHasLowDiff then Just (0, 0, 0)
                                 else Nothing

       -- it works like average mode,
       -- but with checking for every pixel matching.
       [Perfect, EveryPixelMode] ->
         if isEveryPixelHasLowDiff then Just (x, y, averagePixelDiff)
                                   else Nothing

       _ -> error "This mode is unimplemented yet"

  where xs = [0..(cw-1)]
        ys = [0..(ch-1)]

        everyPixelHasLowDiff = and [ hasLowDiff mx my | my <- ys, mx <- xs ]

        everyPixelDiff = [ diff mx my | my <- ys, mx <- xs ]
        isEveryPixelHasLowDiff = all (<= threshold) everyPixelDiff
        averagePixelDiff = sum everyPixelDiff
                         / fromIntegral (length everyPixelDiff)

        diff mx my = getRGBDiff (orig !! (x+mx) !! (y+my))
                                (crop !! mx     !! my    )
        hasLowDiff mx my = diff mx my <= threshold


-- threshold in percents
findCropPos (orig, crop) (ow, oh) (cw, ch) mode threshold = findIt
  where posMatrix = [ (x, y) | x <- xs, y <- ys ]
        xs = [0..(ow-cw-1)]
        ys = [0..(oh-ch-1)]
        threshold8bit = threshold * 255 / 100
        found = cropByThisPos (orig, crop) (cw, ch) mode threshold8bit
        doWeHaveCropHere = hasCropByThisPos found
        findIt =
          case mode of
               [EveryPixelMode] -> find doWeHaveCropHere posMatrix
               [Perfect, EveryPixelMode] ->
                 let cmpByDiff (_, _, a) (_, _, b) = compare a b
                     matches = mapMaybe found posMatrix
                     pos (x, y, _) = (x, y)
                     in case matches of
                             [] -> Nothing
                             _  -> Just $ pos $ minimumBy cmpByDiff matches
               _ -> error "This mode is unimplemented yet"
