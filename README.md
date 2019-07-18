# prismatic

Data types and optics for manipulating HTML color codes.

> My work is a self-portrait of my mind, a prism of my convictions.
> - Fernando Botero

## Examples

    λ> "#ff33cc" ^? _RGB
    Just (RGB {_red = 255, _green = 51, _blue = 204})

    λ> "#ff33cc" ^? _RGB . green
    Just 51

    λ> "#ff33cc" ^? _HSL
    Just (HSL {_hue = 315.0, _saturation = -0.6710526315789473, _lightness = 153.0})

    λ> "#000000" & _RGB.red +~ 100 & _RGB.green +~ 50 & _RGB.blue +~ 5
    "#643205"

    λ> "#000000" & _HSL.lightness +~ 30.0 & _HSL.hue +~ 0.3 & _HSL.saturation +~ 0.5
    "#0f2c2c"
