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
    
    let triangle (vertex1:Point) (vertex2:Point) (vertex3:Point) (fill:System.Drawing.Color) (border:System.Drawing.Color * float) =
        [
            Move (vertex1.x, vertex1.y)
            LineTo (vertex2.x, vertex2.y)
            LineTo (vertex3.x, vertex3.y)
            fill |> toRGBFill
            border |> fst |> toRGBStroke
            border |> snd |> Width
            CloseFillStroke
        ]
    
    
