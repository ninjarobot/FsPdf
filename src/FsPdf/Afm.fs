namespace FsPdf

/// Working with Adobe Font Metrics Files
module Afm =

    type Font = {
        Name : string // name of font
        Size : float // size in points
    }
    
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
                match c with
                | -1 -> sb.ToString() // End of reader, return whatever is left.
                | _ -> 
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
