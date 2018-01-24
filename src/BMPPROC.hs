module BMPPROC (meanValueGrouping, meshImage) where

import Utils
import Codec.BMP
import System.Environment
import Data.ByteString
import Data.Word8


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
