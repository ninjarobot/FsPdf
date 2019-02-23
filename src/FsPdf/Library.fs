namespace FsPdf

module Pdf =
    
    type PdfObject =
        | PName of string
        | PInteger of int
        | PString of string
        | PReference of Object:int * Generation:int
        | PArray of PdfObject list
        | PDictionary of Map<string, PdfObject>
        | PStream of Map<string, PdfObject> * byte array
        | PIndObj of Object:int * Generation:int * PdfObject
    
    type XRef =
        {
            Object:int
            Generation:int
            InUse : bool
            Offset: int64
        }
    
    type Instructions =
        // m
        | Move of x:int * y:int
        // l
        | LineTo of x:int * y:int
        // c - curve from some start point - the two control points and the end point
        | Curve of c1x:int * c1y:int * c2x:int * c2y:int * endx:int * endy:int
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
        | Translate of dx:int * dy:int
        | Scale of scalex:int * scaley:int
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
        | TextMatrix of scalex:int * scaley:int * translateX:int * translateY:int
        | ShowText of text:string
        
    
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
    
    type Font = {
        Name : string // name of font
        Size : float // size in points
    }
    
    /// Working with Adobe Font Metrics Files
    module Afm =

        type CharMetric = {
            Code : int
            /// width in 1/1000 of point size
            Width : float
            Name : string
        }
        
        type FontMetric = {
            FontName: string
            CharMetrics : System.Collections.Generic.IDictionary<int, CharMetric>
        }
        
        let parseFontMetric (line:string) =
            
            let defaultCharMetrics =
                {
                    Code = 0
                    Width = 0.
                    Name = ""
                }
            
            let segment (cm:CharMetric) (s:string) =
                let s = s.Trim ()
                if s.StartsWith "C" then
                    { cm with Code = s.Substring 2 |> int }
                elif s.StartsWith "WX" then
                    { cm with Width = s.Substring 3 |> float }
                elif s.StartsWith "N" then
                    { cm with Name = s.Substring 2 }
                else
                    cm
            
            line.Split(';') |> Seq.fold segment defaultCharMetrics
        
        let loadFontMetrics (path:string) =
            if System.IO.File.Exists path then
                System.IO.File.ReadAllLines path |> Seq.ofArray
                |> Seq.skipWhile (fun line -> not (line.StartsWith "StartCharMetrics"))
                |> Seq.skip 1
                |> Seq.takeWhile (fun line -> not (line.StartsWith "EndCharMetrics"))
                |> Seq.map parseFontMetric
            else
                Seq.empty
        
        let embeddedFontMetrics =
            let assm = System.Reflection.Assembly.GetExecutingAssembly ()
            let resources = assm.GetManifestResourceNames();
            resources
            |> Array.filter (fun name -> name.EndsWith (".afm"))
            |> Array.map (fun name ->
                use stream = assm.GetManifestResourceStream (name)
                let charMetrics =
                    seq {
                        use reader = new System.IO.StreamReader (stream)
                        while not (reader.EndOfStream) do
                            yield reader.ReadLine ()
                    }
                    |> Seq.skipWhile (fun line -> not (line.StartsWith "StartCharMetrics"))
                    |> Seq.skip 1
                    |> Seq.takeWhile (fun line -> not (line.StartsWith "EndCharMetrics"))
                    |> Seq.map parseFontMetric
                {
                    FontName = name.Split('.').[1] // get name from embedded resource file
                    CharMetrics = charMetrics |> Seq.map (fun cm -> cm.Code, cm) |> dict
                }
            )
            |> Array.map (fun fm -> fm.FontName, fm) |> dict
        
        let charWidth (charMetrics:System.Collections.Generic.IDictionary<int, CharMetric>) (f:Font) (charCode:char) =
            charCode |> int |> charMetrics.TryGetValue |> function
            | true, charMetric -> f.Size * charMetric.Width / 1000.
            | _ -> 0.6 * f.Size // we don't have the metric, so this is a workable estimate
        
        /// Measures a string of text based on the font metrics provided.
        let measureString (fontMetrics:System.Collections.Generic.IDictionary<string, FontMetric>) (f:Font) (s:string) =
            let charMetrics =
                fontMetrics.TryGetValue f.Name |> function
                | true, fontMetric -> fontMetric.CharMetrics
                | false, _ -> [] |> dict // we have no char metrics for this font
            s |> Seq.fold (fun total c -> total + (charWidth charMetrics f c)) 0.
        
        /// Reads lines from a string reader, wrapping them when they reach the specified maximum width.
        let wrapString (fontMetrics:System.Collections.Generic.IDictionary<string, FontMetric>) (f:Font) (maxwidth:float) (reader:System.IO.StringReader) =
            let charMetrics =
                fontMetrics.TryGetValue f.Name |> function
                | true, fontMetric -> fontMetric.CharMetrics
                | false, _ -> [] |> dict // we have no char metrics for this font
            seq {
                let rec readMore (sb:System.Text.StringBuilder) (totalWidth:float) =
                    let c = reader.Peek ()
                    if c < 0 then // End of reader, return whatever is left.
                        sb.ToString ()
                    else
                        let width = c |> char |> charWidth charMetrics f
                        let newTotalWidth = totalWidth + width
                        if newTotalWidth <= maxwidth then // keep reading
                            reader.Read () |> char |> sb.Append |> ignore
                            readMore sb newTotalWidth
                        else // hit the maxwidth, return the string
                            sb.ToString ()
                while reader.Peek () >= 0 do
                    yield readMore (System.Text.StringBuilder()) 0.
            }
    
    module Instructions =
        let escapeString (s:string) =
            s.Replace(@"\", @"\\").Replace("(", "\(").Replace(")", "\)")
            
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
        
    module PdfObject =

        let rec writeSource (writer:System.IO.StreamWriter) (xrefs:ResizeArray<XRef>) = function
            | PName n -> writer.Write (System.String.Format ("/{0}", n))
            | PInteger i -> writer.Write i
            | PString s -> writer.Write (System.String.Format ("({0})", s))
            | PReference (o, g) -> writer.Write (System.String.Format ("{0} {1} R", o, g))
            | PArray arr ->
                writer.Write "["
                arr |> List.iter (fun item ->
                    writer.Write (" ")
                    item |> writeSource writer xrefs
                    writer.Write (" ")
                )
                writer.Write "]"
            | PDictionary d ->
                writer.Write "<<"
                writer.Write System.Environment.NewLine
                writer.Write System.Environment.NewLine
                d |> Map.iter (fun name pdfObj ->
                    writer.Write (System.String.Format("/{0} ", name))
                    pdfObj |> writeSource writer xrefs
                    writer.Write System.Environment.NewLine
                )
                writer.Write ">>"
            | PIndObj (i, g, pdfObject) ->
                writer.Flush()
                let newXref =
                    {
                         Object = i
                         Generation = g
                         InUse = true
                         Offset = writer.BaseStream.Position
                    }
                writer.Write (System.String.Format("{0} {1} obj", i, g))
                writer.Write System.Environment.NewLine
                pdfObject |> writeSource writer xrefs
                writer.Write "endobj"
                writer.Write System.Environment.NewLine
                xrefs.Add newXref
            | PStream (d, bytes) ->
                writer.WriteLine ("<< /Length {0} >>", bytes.Length)
                writer.Write "stream"
                writer.Write System.Environment.NewLine
                writer.Flush ()
                writer.BaseStream.Write (bytes, 0, bytes.Length)
                writer.BaseStream.Flush ()
                writer.Write System.Environment.NewLine
                writer.Write ("endstream")
                writer.Write System.Environment.NewLine
        
        let writePreamble (writer:System.IO.StreamWriter) =
            writer.Write ("%PDF-1.7")
            writer.Write System.Environment.NewLine
            writer.Write ("%\u15B4\u266F\u2502\u227B\u2C92\u2764") // ᖴ♯│≻Ⲓ❤
            writer.Write System.Environment.NewLine
            writer.Write System.Environment.NewLine
            
        let writeXrefSection (writer:System.IO.StreamWriter) (xrefs:ResizeArray<XRef>) =
            writer.Write "xref"
            writer.Write System.Environment.NewLine
            writer.Write (System.String.Format ("{0} {1}", 0, xrefs.Count + 1))
            writer.Write System.Environment.NewLine
            writer.Write ("0000000000")
            writer.Write (" ")
            writer.Write ("65535")
            writer.Write (" ")
            writer.Write "f"
            writer.Write System.Environment.NewLine
            for xref in xrefs do
                writer.Write (xref.Offset.ToString("0000000000"))
                writer.Write (" ")
                writer.Write (xref.Generation.ToString("00000"))
                writer.Write (" ")
                if xref.InUse then
                    writer.Write "n"
                else
                    writer.Write "f"
                writer.Write System.Environment.NewLine
            writer.Write System.Environment.NewLine
        
        let writeTrailer (writer:System.IO.StreamWriter) (xrefStart:int64) =
            writer.Write "trailer"
            let xrefs = new ResizeArray<XRef> ()
            writer.Write System.Environment.NewLine
            [
                "Root", PReference (1, 0)
                "Size", PInteger 5
            ] |> Map.ofList |> PDictionary |> writeSource writer xrefs
            writer.Write "startxref"
            writer.Write System.Environment.NewLine
            writer.Write (xrefStart |> string)
            writer.Write System.Environment.NewLine
            writer.Write "%%EOF"
            writer.Write System.Environment.NewLine
