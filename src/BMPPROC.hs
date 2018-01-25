module BMPPROC (betterMeanValueGrouping, meanValueGrouping, meshImage, maybeThisIsOk) where

import Utils
import Codec.BMP
import System.Environment
import Data.ByteString
import Data.Word8
import Data.Map


type Mesher = (Int, Int) -> Int -> [RGBA] -> [Double]

instance Num RGBA where
  (RGBA red green blue alpha) + (RGBA red' green' blue' alpha') = RGBA (red + red') (green + green') (blue + blue') (alpha + alpha')
  (RGBA red green blue alpha) - (RGBA red' green' blue' alpha') = RGBA (red - red') (green - green') (blue - blue') (alpha - alpha')
  (RGBA red green blue alpha) * (RGBA red' green' blue' alpha') = RGBA (red * red') (green * green') (blue * blue') (alpha * alpha')
  negate (RGBA red green blue alpha) = RGBA (-1 * red) (-1 * green) (-1 * blue) (-1 * alpha)
  abs (RGBA red green blue alpha) = RGBA (abs red) (abs green) (abs blue) (abs alpha)
  fromInteger num = let num' = fromIntegral num in RGBA num' num' num' num'
  signum rgba = rgba


word8ToRGBA :: [Word8] -> RGBA
word8ToRGBA [red, green, blue, alpha] = RGBA (fromIntegral red) (fromIntegral green) (fromIntegral blue) (fromIntegral alpha)
word8ToRGBA [red, green, blue] = RGBA (fromIntegral red) (fromIntegral green) (fromIntegral blue) 0
word8ToRGBA [red, green] = RGBA (fromIntegral red) (fromIntegral green) 0 0
word8ToRGBA [red] = RGBA (fromIntegral red) 0 0 0
word8ToRGBA _ = error "Tried to make a to large word list to RGBA"


word8sToRGBA :: [Word8] -> [RGBA]
word8sToRGBA [] = []
word8sToRGBA words = let (rgba, rest) = Prelude.splitAt 4 words
                       in word8ToRGBA rgba : word8sToRGBA rest


greyScale :: RGBA -> RGBA
greyScale (RGBA red green blue alpha) = 
  let meanValue = (red + green + blue + alpha) `quot` 4
    in RGBA meanValue meanValue meanValue meanValue 




meshImage :: Mesher -> Int -> FilePath -> IO [Double] 
meshImage mesher rDim image = 
  do
    Right bmp <- readBMP image
    let rgbas = word8sToRGBA . unpack . bmpRawImageData $ bmp
    return $ mesher (bmpDimensions bmp) rDim rgbas



{-Different kinds of meshers follows -}

meanValueGrouping :: Mesher
meanValueGrouping _ rDim rgba = grouping rgba
  where
    groupSz = Prelude.length rgba `quot` rDim
    grouping [] = []
    grouping pixels = let (pgroup, rest) = Prelude.splitAt groupSz pixels
                          (RGBA red green blue alpha) = sum pgroup 
                          meanValue = red + green + blue + alpha `quot` Prelude.length pgroup
                        in fromIntegral meanValue : grouping rest


{- Still bad but better -}
betterMeanValueGrouping :: Mesher
betterMeanValueGrouping (x, y) rDim rgba =  Prelude.take rDim $ Prelude.map ((/ (fromIntegral pixelsPerGroup)) . sum . snd) $ toList grouper
  where
    value :: RGBA -> Double
    value (RGBA red green blue alpha) = fromIntegral $ red * 256^3 + green * 256^2 + blue*256 + alpha 

    pixelsPerGroup :: Int
    pixelsPerGroup = Prelude.length rgba `quot` rDim

    pixelsPerAxis :: Int
    pixelsPerAxis =  floor . sqrt . fromIntegral $ pixelsPerGroup

    xGroup :: Int -> Int
    xGroup x' = ((x' * pixelsPerAxis) `quot` x)

    yGroup :: Int -> Int
    yGroup y' = ((y' * pixelsPerAxis) `quot` y)

    idxToxy :: Int -> (Int, Int)
    idxToxy idx = (idx `mod` x, idx `quot` y)

    idxToGroup :: Int -> Int
    idxToGroup idx = let (x', y') = idxToxy idx
                      in yGroup x' + xGroup y'
                         

    updateMap :: (RGBA, Int) -> Map Int [Double] -> Map Int [Double]
    updateMap (v, i) m = let group = idxToGroup i
                            in if member group m
                                  then adjust (value v:) group m
                                  else insert group [value v] m

    grouper :: Map Int [Double]
    grouper = Prelude.foldr updateMap Data.Map.empty $ Prelude.zip rgba [0..]


maybeThisIsOk :: Mesher
maybeThisIsOk _ rDim rgba = groupr rgba
  where
    pixelsPerGroup :: Int 
    pixelsPerGroup = Prelude.length rgba `quot` rDim

    groupr [] = []
    groupr pixels = let (pg, rest) = Prelude.splitAt pixelsPerGroup pixels
                        (RGBA red green blue _) = sum pg
                        l = Prelude.length pg
                        meanRed = red `quot` l
                        meanGreen = green `quot` l
                        meanBlue = blue `quot` l
                        mv = meanRed^3 + meanGreen^2 + meanBlue
                      in fromIntegral mv : groupr rest

