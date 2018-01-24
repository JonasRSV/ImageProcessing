module Utils where

data RGBA = RGBA Int Int Int Int

greyScale :: RGBA -> RGBA
greyScale (RGBA red green blue alpha) = 
  let meanValue = (red + green + blue + alpha) `quot` 4
    in RGBA meanValue meanValue meanValue meanValue 
