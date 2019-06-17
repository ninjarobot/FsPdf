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
    
    let circle (center:Point) (radius:float) (fill:System.Drawing.Color) (border:System.Drawing.Color * float) =
        // Based off http://spencermortensen.com/articles/bezier-circle/
        let cX = center.x |> float
        let cY = center.y |> float
        let factor = 0.551915024494
        [
            Move (center.x, int(float(center.y) + radius))
            Curve (
                      int(cX + radius * factor),
                      int(cY + radius),
                      int(cX + radius),
                      int(cY + radius * factor),
                      int(cX + radius),
                      int(cY)
                  )
            Curve (
                      int(cX + radius),
                      int(cY - radius * factor),
                      int(cX + radius * factor),
                      int(cY - radius),
                      int(cX),
                      int(cY - radius)
                  )
            Curve (
                      int(cX - radius * factor),
                      int(cY - radius),
                      int(cX - radius),
                      int(cY - radius * factor),
                      int(cX - radius),
                      int(cY)
                  )
            Curve (
                      int(cX - radius),
                      int(cY + radius * factor),
                      int(cX - radius * factor),
                      int(cY + radius),
                      int(cX),
                      int(cY + radius)
                  )
            fill |> toRGBFill
            border |> fst |> toRGBStroke
            border |> snd |> Width
            CloseFillStroke
        ]
    
    module Holiday =
        
        let candleFlame (bottom:Point) (top:Point) (fill:System.Drawing.Color) (border:System.Drawing.Color * float) =
            let height = top.y - bottom.y |> float
            let c1x = 1./4. * height |> int
            let c1y = 1./4. * height |> int
            let c2x = 0.5 * height |> int
            let c2y = 0.6 * height |> int
            [
                Move (bottom.x, bottom.y)
                Curve (c1x + bottom.x, c1y + bottom.y, c2x + bottom.x, c2y + bottom.y, top.x, top.y)
                Curve (bottom.x - c2x, c2y + bottom.y, bottom.x - c1x, c1y + bottom.y, bottom.x, bottom.y)
                fill |> toRGBFill
                border |> fst |> toRGBStroke
                border |> snd |> Width
                CloseFillStroke
            ]
        
        let candle (bottom:Point) (top:Point) (fill:System.Drawing.Color) (border:System.Drawing.Color * float) =
            let height = top.y - bottom.y |> float
            let width = 0.1 * height
            let flameHeight = height * 0.2
            let candleHeight = height * 0.79
            [
                // flame
                candleFlame {top with y = (float(top.y) - flameHeight) |> int} top System.Drawing.Color.Yellow (System.Drawing.Color.Goldenrod, 0.2)
                // wick
                [
                    Width 1.
                    Move (bottom.x, float(bottom.y) + candleHeight |> int)
                    LineTo (bottom.x, float(top.y) - (flameHeight * 0.96) |> int)
                    System.Drawing.Color.Black |> toRGBStroke
                    Stroke
                    LineTo (bottom.x, float(top.y) - (flameHeight * 0.9) |> int)
                    System.Drawing.Color.Orange |> toRGBStroke
                    Stroke
                ]
                // candle
                rectange {bottom with x = float(bottom.x) - 0.4 * width |> int} {x = float(top.x) + 0.4 * width |> int; y = float(bottom.y) + candleHeight |> int} fill border
            ] |> List.concat
