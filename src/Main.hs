-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/crop-detector/master/LICENSE

module Main where

-- import Debug.Hood.Observe (runO, observe)

import System.Environment (getArgs)
import Data.List (find, minimumBy)
import Data.List.Split (chunksOf)
import Data.Word (Word8)
import Data.Maybe (isJust, mapMaybe, fromJust)
import Control.Parallel.Strategies (withStrategy, parList, rpar)

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

                       "every-pixel"          -> [EveryPixelMode]
                       "every-pixel-parallel" -> [ParallelMode, EveryPixelMode]
                       "perfect-every-pixel"  -> [PerfectMode, EveryPixelMode]
                       "perfect-every-pixel-parallel" ->
                         [PerfectMode, ParallelMode, EveryPixelMode]

                       "average"          -> [AverageMode]
                       "average-parallel" -> [ParallelMode, AverageMode]
                       "perfect-average"  -> [PerfectMode, AverageMode]
                       "perfect-average-parallel" ->
                         [PerfectMode, ParallelMode, AverageMode]

                       _                     -> error "Unknown mode"

  if cw > ow || ch > oh
     then error $ foldr1 (++)
                  [ "Cropped image can't have size more than original image.\n"
                  , "Original image size: ", show ow, "x", show oh, ".\n"
                  , "Cropped image size: ",  show cw, "x", show ch, "."
                  ]
     else let pairRGB   = (getRGBMatrix orig, getRGBMatrix crop)
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
          | PerfectMode
          | ParallelMode
            deriving (Enum, Eq, Show)

imgSize x = (w, h)
  where (R.Z R.:. h R.:. w R.:. _) = R.extent x

getRGBMatrix img = [ [ getRGB img x y | y <- ys ] | x <- xs ]
  where getRGB img x y = RGBt (ch 0) (ch 1) (ch 2)
          where ch n = img R.! (R.Z R.:. y R.:. x R.:. n)
        (w, h) = imgSize img
        xs = [0..(w-1)]
        ys = [0..(h-1)]

getRGBDiff a b = (diff cR + diff cG + diff cB) / 3
  where cR (RGBt x _ _) = fromIntegral x
        cG (RGBt _ x _) = fromIntegral x
        cB (RGBt _ _ x) = fromIntegral x
        diff f = abs $ f a - f b

-- threshold should be 0..255
cropByThisPos (orig, crop) (cw, ch) mode threshold (x, y) =

  case mode of

       [EveryPixelMode]                            -> everyPixelResult
       [ParallelMode, EveryPixelMode]              -> everyPixelResult

       -- perfect mode for every-pixel works like average mode,
       -- but with checking for every pixel matching.
       [PerfectMode, EveryPixelMode]               -> everyPixelResult
       [PerfectMode, ParallelMode, EveryPixelMode] -> everyPixelResult

       [AverageMode]                               -> average
       [ParallelMode, AverageMode]                 -> average
       [PerfectMode, AverageMode]                  -> average
       [PerfectMode, ParallelMode, AverageMode]    -> average

       _ -> error "This mode is unimplemented yet"

  where xs = [0..(cw-1)]
        ys = [0..(ch-1)]

        average = if averagePixelsDiff <= threshold
                     then Just (x, y, averagePixelsDiff)
                     else Nothing

        pixelsDiffs = [ diff mx my | my <- ys, mx <- xs ]

        isEveryPixelHasLowDiff = all (<= threshold) pixelsDiffs

        everyPixelResult =
          if isEveryPixelHasLowDiff then Just result
                                    else Nothing
          where result = if PerfectMode `elem` mode
                            then (x, y, averagePixelsDiff)
                            else (0, 0, 0)

        averagePixelsDiff = sum pixelsDiffs
                          / fromIntegral (length pixelsDiffs)

        diff mx my = getRGBDiff (orig !! (x+mx) !! (y+my))
                                (crop !! mx     !! my    )


-- threshold in percents
findCropPos (orig, crop) (ow, oh) (cw, ch) mode threshold = findIt
  where posMatrix  = [ (x, y) | x <- xs, y <- ys ]
          where xs = [0..posXlimit]
                ys = [0..posYlimit]

        posXlimit = ow - cw - 1
        posYlimit = oh - ch - 1
        posTotalCount = (posXlimit + 1) * (posYlimit + 1)

        finder = cropByThisPos (orig, crop) (cw, ch) mode threshold8bit
          where threshold8bit = threshold * 255 / 100

        findFirst = find doWeHaveCropHere posMatrix
          where doWeHaveCropHere = hasCropByThisPos finder
                hasCropByThisPos f (x, y) = isJust $ f (x, y)

        findBest =
          case matches of
               [] -> Nothing
               _  -> Just $ pos $ minimumBy cmpByDiff matches
          where matches = mapMaybe finder list
                list = if ParallelMode `elem` mode
                          then withStrategy (parList rpar) posMatrix
                          else posMatrix
                pos (x, y, _) = (x, y)
                cmpByDiff (_, _, a) (_, _, b) = compare a b

        findIt =
          case mode of

               [AverageMode]                               -> findFirst
               [ParallelMode, AverageMode]                 -> findFirst
               [PerfectMode, AverageMode]                  -> findBest
               [PerfectMode, ParallelMode, AverageMode]    -> findBest

               [EveryPixelMode]                            -> findFirst
               [ParallelMode, EveryPixelMode]              -> findFirst
               [PerfectMode, EveryPixelMode]               -> findBest
               [PerfectMode, ParallelMode, EveryPixelMode] -> findBest

               _ -> error "This mode is unimplemented yet"
