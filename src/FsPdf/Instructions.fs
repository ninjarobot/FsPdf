namespace FsPdf

type Instructions =
    // m
    | Move of x:float * y:float
    // l
    | LineTo of x:float * y:float
    // c - curve from some start point - the two control points and the end point
    | Curve of c1x:float * c1y:float * c2x:float * c2y:float * endx:float * endy:float
    | Stroke
    // h - close the current path.
    | ClosePath
    // F - fill the current path (non-zero winding rule)
    | Fill
    // B - fill and stroke the current path (non-zero winding rule)
    | FillStroke
    // b - close, fill, and stroke the current path (non-zero winding rule)
    | CloseFillStroke
    | Width of pixels:float
    | GrayStroke of lightness:float
    | GrayFill of lightness:float
    | RGBStroke of red:float * green:float * blue:float
    | RGBFill of red:float * green:float * blue:float
    | Translate of dx:float * dy:float
    | Scale of scalex:float * scaley:float
    | Rotate of radians:float
    | PushGraphicsState
    | PopGraphicsState
    | BeginText
    | EndText
    | CharSpacing of charSpace:int
    | WordSpacing of wordSpace:int
    | HorizontalScaling of scale:float
    | Leading of leading:int
    | NextLineTranslate of x:int * y:int
    | NextLine
    | FontSize of fontKey:string * size:float
    | TextMatrix of scalex:float * scaley:float * translateX:float * translateY:float
    | ShowText of text:string

module Instructions =
    let escapeString (s:string) =
        s.Replace(@"\", @"\\").Replace("(", "\(").Replace(")", "\)")
        
    let toRGBStroke (color:System.Drawing.Color) =
        RGBStroke (
            (color.R |> float) / 255.,
            (color.G |> float) / 255.,
            (color.B |> float) / 255.
        )

    let toRGBFill (color:System.Drawing.Color) =
        RGBFill (
            (color.R |> float) / 255.,
            (color.G |> float) / 255.,
            (color.B |> float) / 255.
        )
        
    let instruction = function
        | Stroke -> "S"
        | Width p -> System.String.Format ("{0} w", p)
        | Move (x, y) -> System.String.Format ("{0} {1} m", x, y)
        | LineTo (x, y) -> System.String.Format ("{0} {1} l", x, y)
        | Curve (c1x, c1y, c2x, c2y, endx, endy) -> System.String.Format ("{0} {1} {2} {3} {4} {5} c", c1x, c1y, c2x, c2y, endx, endy)
        | ClosePath -> "h"
        | Fill -> "F"
        | FillStroke -> "B"
        | CloseFillStroke -> "b"
        | GrayStroke (lightness) -> System.String.Format ("{0} G", lightness)
        | GrayFill (lightness) -> System.String.Format ("{0} g", lightness)
        | RGBStroke (r, g, b) -> System.String.Format ("{0} {1} {2} RG", r, g, b)
        | RGBFill (r, g, b) -> System.String.Format ("{0} {1} {2} rg", r, g, b)
        | Translate (dx, dy) -> System.String.Format ("1 0 0 1 {0} {1} cm", dx, dy)
        | Scale (scalex, scaley) -> System.String.Format ("{0} 0 0 {1} 0 0 cm", scalex, scaley)
        | Rotate (radians) ->
            let sinx = System.Math.Sin radians
            let cosx = System.Math.Cos radians
            System.String.Format ("{0} {1} {2} {3} 0 0 cm", cosx, sinx, -1. * sinx, cosx )
        | PushGraphicsState -> "q"
        | PopGraphicsState -> "Q"
        | BeginText -> "BT"
        | EndText -> "ET"
        | CharSpacing (charSpacing) -> System.String.Format ("{0} Tc", charSpacing)
        | WordSpacing (wordSpacing) -> System.String.Format ("{0} Tw", wordSpacing)
        | HorizontalScaling (scale) -> System.String.Format ("{0} Tz", scale / 100.)
        | Leading (leading) -> System.String.Format ("{0} TL", leading)
        | NextLineTranslate (x, y) -> System.String.Format ("{0} {1} Td", x, y)
        | NextLine -> "T*"
        | FontSize (fontKey, size) -> System.String.Format ("/{0} {1} Tf", fontKey, size)
        | TextMatrix (scaleX, scaleY, translateX, translateY) -> System.String.Format ("{0} 0 0 {1} {2} {3} Tm")
        | ShowText (text) -> System.String.Format ("({0}) Tj", (text |> escapeString))

