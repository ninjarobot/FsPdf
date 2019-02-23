namespace FsPdf

open Instructions

module Shapes =
    type Point = {
        x:int
        y:int
    }
    
    let rectange (bottomLeft:Point) (topRight:Point) (fill:System.Drawing.Color) (border:System.Drawing.Color * float) =
        [
            Move (bottomLeft.x, bottomLeft.y)
            LineTo (bottomLeft.x, topRight.y)
            LineTo (topRight.x, topRight.y)
            LineTo (topRight.x, bottomLeft.y)
            fill |> toRGBFill
            border |> fst |> toRGBStroke
            border |> snd |> Width
            CloseFillStroke
        ]
