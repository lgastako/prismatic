# prismatic

> My work is a self-portrait of my mind, a prism of my convictions.
> - Fernando Botero

Data types and optics for manipulating HTML color codes.

## Examples

    λ> "#000000" & _RGB.red +~ 100 & _RGB.green +~ 50 & _RGB.blue +~ 5
    "#643205"
