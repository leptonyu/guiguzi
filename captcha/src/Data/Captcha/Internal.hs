{-# LANGUAGE TemplateHaskell #-}
module Data.Captcha.Internal where

import           Codec.Picture
import           Data.ByteString.Lazy        (ByteString)
import           Data.Char
import           FileEmbedLzma
import           Graphics.Rasterific
import           Graphics.Rasterific.Texture
import           Graphics.Text.TrueType
import           System.Random.SplitMix

font :: Font
font = case decodeFont $(embedLazyByteString "cn.ttf") of
    Right f -> f
    Left  e -> error e

newCaptcha :: Font -> IO (String, ByteString)
newCaptcha f = do
  gen <- initSMGen
  let colors =
        [ PixelRGBA8 0 0x86 0xc1 255
        ]
      (_,ps) = foldr go (gen, []) ([1..4] :: [Int])
      go _ (g1,xs) =
        let (i, g2) = nextInt g1
            (d1,r1) = divMod i  26
            r2      = d1 `mod` length colors
            a       = chr $ ord 'A' + r1
        in (g2, (a, TextRange f (PointSize 50) [a]
                (Just $ uniformTexture $ colors !! r2)) :xs)
  return (
      fmap fst ps
    , encodePng
      $ renderDrawing 192 64 (PixelRGBA8 255 255 255 255)
      $ printTextRanges (V2 10 50)
      $ fmap snd ps)

