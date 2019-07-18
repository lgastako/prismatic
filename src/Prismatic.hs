{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Prismatic
  ( AsHSL
  , AsRGB
  , HSL
  , IsHSL
  , IsRGB
  , RGB
  , _HSL
  , _RGB
  , blue
  , green
  , hsl
  , hue
  , lightness
  , red
  , rgb
  , saturation
  ) where

import           Control.Lens
import           Data.ByteString                   ( ByteString )
import qualified Data.ByteString.Lazy     as Lazy
import qualified Data.Colour.RGBSpace     as C.RGB
import qualified Data.Colour.RGBSpace.HSL as C.HSL
import           Data.String.Conversions           ( cs )
import           Data.Text                         ( Text )
import qualified Data.Text.Lazy           as Lazy  ( Text )
import           Numeric                           ( readHex )
import           Text.Printf                       ( printf )

-- TODO convert red, green, blue, hue, saturation, lightness to work
-- on any AsRGB, so you can just do, eg:
-- "#000000" & red +~ 128

data RGB = RGB
  { _red'   :: Int
  , _green' :: Int
  , _blue'  :: Int
  } deriving (Eq, Ord, Read, Show)

data HSL = HSL
  { _hue'        :: Double
  , _saturation' :: Double
  , _lightness'  :: Double
  } deriving (Eq, Ord, Read, Show)

makeLenses ''RGB
makeLenses ''HSL

class AsHSL a where
  _HSL :: Prism' a HSL

class AsRGB a where
  _RGB :: Prism' a RGB

class IsHSL a where
  hsl :: Iso' a HSL

class IsRGB a where
  rgb :: Iso' a RGB

instance IsHSL RGB where
  hsl = hsl'

instance IsRGB HSL where
  rgb = rgb'

encodeRGB :: RGB -> ByteString
encodeRGB = cs . encodeRGBStr

encodeRGBStr :: RGB -> String
encodeRGBStr (RGB r g b) = printf "#%02x%02x%02x" r g b

decodeRGB :: ByteString -> Maybe RGB
decodeRGB = decodeRGBStr . cs

decodeRGBStr :: String -> Maybe RGB
decodeRGBStr ('#':r0:r1:g0:g1:b0:b1:[])
  | all valid [r,g,b] = Just $ RGB r g b
  where
    valid :: Int -> Bool
    valid n = 0 <= n && n <= 255

    r = fromHex [r0, r1]
    g = fromHex [g0, g1]
    b = fromHex [b0, b1]

decodeRGBStr _ = Nothing

fromHex :: String -> Int -- TODO maybe
fromHex = fst . head . readHex

instance AsRGB RGB where
  _RGB = id

instance AsRGB ByteString where
  _RGB = prism encodeRGB (maybeEither decodeRGB)

instance AsRGB String where
  _RGB = prism (cs . encodeRGB) (maybeEither (decodeRGB . cs))

instance AsRGB Text where
  _RGB = prism (cs . encodeRGB) (maybeEither (decodeRGB . cs))

instance AsRGB Lazy.ByteString where
  _RGB = prism (cs . encodeRGB) (maybeEither (decodeRGB . cs))

instance AsRGB Lazy.Text where
  _RGB = prism (cs . encodeRGB) (maybeEither (decodeRGB . cs))

instance AsHSL HSL where
  _HSL = id

instance AsHSL ByteString where
  _HSL = _RGB . hsl

instance AsHSL String where
  _HSL = _RGB . hsl

instance AsHSL Text where
  _HSL = _RGB . hsl

instance AsHSL Lazy.ByteString where
  _HSL = _RGB . hsl

instance AsHSL Lazy.Text where
  _HSL = _RGB . hsl

red :: AsRGB a => Traversal'  a Int
red = _RGB . red'

green :: AsRGB a => Traversal'  a Int
green = _RGB . green'

blue :: AsRGB a => Traversal'  a Int
blue = _RGB . blue'

hue :: AsHSL a => Traversal'  a Double
hue = _HSL . hue'

saturation :: AsHSL a => Traversal'  a Double
saturation = _HSL . saturation'

lightness :: AsHSL a => Traversal'  a Double
lightness = _HSL . lightness'

hsl' :: Iso' RGB HSL
hsl' = iso rgbToHsl hslToRgb

rgb' :: Iso' HSL RGB
rgb' = from hsl'

rgbToHsl :: RGB -> HSL
rgbToHsl (RGB r g b) = HSL h s l
  where
    cRgb :: C.RGB.RGB Double
    cRgb = C.RGB.RGB (fromIntegral r) (fromIntegral g) (fromIntegral b)

    (h, s, l) = C.HSL.hslView cRgb

hslToRgb :: HSL -> RGB
hslToRgb (HSL h s l) = RGB
  (fromDouble . C.RGB.channelRed   $ cRgb)
  (fromDouble . C.RGB.channelGreen $ cRgb)
  (fromDouble . C.RGB.channelBlue  $ cRgb)
  where
    cRgb :: C.RGB.RGB Double
    cRgb = C.HSL.hsl h s l

fromDouble :: Double -> Int
fromDouble = floor

maybeEither :: (a -> Maybe b)-> a -> Either a b
maybeEither f x = maybe (Left x) Right . f $ x
