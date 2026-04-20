module Tests

open System
open System.Drawing
open Xunit
open FsPdf
open FsPdf.Afm
open FsPdf.Layout
open FsPdf.Document

let singlePageFromContent (content:byte array) =
    [
        PIndObj (1, 0,
            [
                "Type", PName "Catalog"
                "Pages", PReference (2, 0)
            ] |> Map.ofList |> PDictionary
        )
        PIndObj (2, 0,
            [
                "Type", PName "Pages"
                "Kids", PArray [ PReference (3, 0) ]
                "Count", PInteger 1
                "MediaBox", PArray [PInteger 0; PInteger 0; PInteger 612; PInteger 792]
            ] |> Map.ofList |> PDictionary
        )
        PIndObj (3, 0,
            [
                "Type", PName "Page"
                "Parent", PReference (2, 0)
                "Resources", [
                    "Font", [
                        "F1", [
                            "Type", PName "Font"
                            "Subtype", PName "Type1"
                            "BaseFont", PName "Times-Roman"
                        ] |> Map.ofList |> PDictionary
                        "F2", [
                            "Type", PName "Font"
                            "Subtype", PName "Type1"
                            "BaseFont", PName "Helvetica"
                        ] |> Map.ofList |> PDictionary
                    ] |> Map.ofList |> PDictionary
                ] |> Map.ofList |> PDictionary
                "Contents", PReference (4, 0)
            ] |> Map.ofList |> PDictionary
        )
        PIndObj (4, 0,
            PStream (
                [
                    "Length", PInteger content.Length
                ] |> Map.ofList ,
                content
            )
        )
    ]

let wrappedTextContent = 
    let text = "F# is a mature, open source, cross-platform, functional-first programming language. It empowers users and organizations to tackle complex computing problems with simple, maintainable and robust code."
    let font = { Name="Times-Roman"; Size=18. }
    use reader = new System.IO.StringReader (text)
    let lines =
        wrapString embeddedFontMetrics font (612. - (72. * 2.)) reader
        |> Seq.map (fun line -> [ShowText line; NextLine])
        |> Seq.concat |> List.ofSeq
    [
        BeginText
        Leading (20)
        FontSize ("F1", 18.)
        NextLineTranslate (50, 600)
    ] @
    lines @
    [
        NextLineTranslate (30, -20)
        ShowText "--fsharp.org"
        EndText
    ]

[<Fact>]
let ``Wrapped Text PDF`` () =
    let pdfName =
        sprintf "%s.pdf" (System.Reflection.MethodBase.GetCurrentMethod().Name)
    System.IO.File.Delete (pdfName)
    use stream = System.IO.File.OpenWrite (pdfName)
    wrappedTextContent
    |> List.map Instructions.instruction |> String.concat " "
    |> System.Text.Encoding.UTF8.GetBytes
    |> singlePageFromContent
    |> PdfObject.writePdf stream

let someShapesContent =
    [
        Move (200.,350.)
        LineTo (500., 750.)
        LineTo (600., 300.)
        LineTo (175., 220.)
        ClosePath
        RGBFill (0.3, 0.0, 0.4)
        RGBStroke (0.7, 0.0, 0.7)
        Width 5.0
        FillStroke
        PushGraphicsState
        Move (10., 20.)
        Curve (5., 10., 60., 50., 75., 100.)
        LineTo (75., 50.)
        LineTo (10., 50.)
        RGBFill (0., 0.9, 0.6)
        RGBStroke (0., 0., 0.)
        Width 2.0
        CloseFillStroke
        PopGraphicsState
        Move (250., 250.)
        LineTo (300., 250.)
        LineTo (300., 300.)
        LineTo (250., 300.)
        Width 2.0
        System.Drawing.Color.Goldenrod |> Instructions.toRGBStroke
        CloseFillStroke
    ] @ (Shapes.rectange {x=400.; y=200.} {x=600.; y=275.} System.Drawing.Color.SteelBlue (System.Drawing.Color.Black, 2.))
      //@ (Shapes.Holiday.candleFlame {x=100; y=500} {x=100; y=550} System.Drawing.Color.Yellow (System.Drawing.Color.Orange, 2.))
      @ (Shapes.Holiday.candle {x=100.; y=300.} {x=100.; y=550.} System.Drawing.Color.Pink (System.Drawing.Color.DeepPink, 2.))
      @ (Shapes.circle {x=125.; y=650.} 50. System.Drawing.Color.Firebrick (System.Drawing.Color.Black, 2.))

[<Fact>]
let ``Shape testing PDF`` () =
    let pdfName =
        sprintf "%s.pdf" (System.Reflection.MethodBase.GetCurrentMethod().Name)
    System.IO.File.Delete (pdfName)
    use stream = System.IO.File.OpenWrite (pdfName)
    someShapesContent
    |> List.map Instructions.instruction |> String.concat " "
    |> System.Text.Encoding.UTF8.GetBytes |> singlePageFromContent
    |> PdfObject.writePdf stream

let spiralRectanges =
    let ins =[ 1..174 ] |> List.pairwise |> List.mapi (fun idx (color1, color2) ->
            [
                yield! (Shapes.rectange {x=idx + 10 |> float; y=idx + 10 |> float} {x=idx + 75 |> float; y=idx + 30 |> float} (enum<System.Drawing.KnownColor> color1 |> System.Drawing.Color.FromKnownColor) (enum<System.Drawing.KnownColor> color2 |> System.Drawing.Color.FromKnownColor, 2.))
                yield Translate (3., 4.)
                yield Rotate (0.1)
            ]
        )
    [Translate (350., 450.)] :: ins |> List.concat

[<Fact>]
let ``Spiral rectangles PDF`` () =
    let pdfName =
        sprintf "%s.pdf" (System.Reflection.MethodBase.GetCurrentMethod().Name)
    System.IO.File.Delete (pdfName)
    use stream = System.IO.File.OpenWrite (pdfName)
    spiralRectanges
    |> List.map Instructions.instruction |> String.concat " "
    |> System.Text.Encoding.UTF8.GetBytes |> singlePageFromContent
    |> PdfObject.writePdf stream

let triangle =
    Shapes.triangle { x=200.; y=600. } { x=300.; y=700. } { x=400.; y=600. } System.Drawing.Color.SteelBlue (System.Drawing.Color.Orange, 2.)

[<Fact>]
let ``Triangle PDF`` () =
    let pdfName =
        sprintf "%s.pdf" (System.Reflection.MethodBase.GetCurrentMethod().Name)
    System.IO.File.Delete (pdfName)
    use stream = System.IO.File.OpenWrite (pdfName)
    triangle
    |> List.map Instructions.instruction |> String.concat " "
    |> System.Text.Encoding.UTF8.GetBytes |> singlePageFromContent
    |> PdfObject.writePdf stream

let colorfulTextContent = 
    let text = ""
    let font = { Name="Times-Roman"; Size=18. }
    use reader = new System.IO.StringReader (text)
    let lines =
        wrapString embeddedFontMetrics font (612. - (72. * 2.)) reader
        |> Seq.map (fun line -> [ShowText line; NextLine])
        |> Seq.concat |> List.ofSeq
    [
        BeginText
        Leading (20)
        FontSize ("F1", 30.)
        NextLineTranslate (50, 600)
        System.Drawing.Color.BlueViolet |> Instructions.toRGBFill
        ShowText "hello"
        NextLineTranslate (30, -20)
        System.Drawing.Color.Lime |> Instructions.toRGBFill
        ShowText "world"
        NextLineTranslate (0, -40)
        System.Drawing.Color.Red |> Instructions.toRGBFill
        FontSize ("F2", 16.)
        ShowText "Using different colors and sizes of text."
        EndText
    ]

[<Fact>]
let ``Colorful Text PDF`` () =
    let pdfName =
        sprintf "%s.pdf" (System.Reflection.MethodBase.GetCurrentMethod().Name)
    System.IO.File.Delete (pdfName)
    use stream = System.IO.File.OpenWrite (pdfName)
    colorfulTextContent
    |> List.map Instructions.instruction |> String.concat " "
    |> System.Text.Encoding.UTF8.GetBytes |> singlePageFromContent
    |> PdfObject.writePdf stream

[<Fact>]
let ``Measure a string`` () =
    let testString = "This is a test of string measurement."
    let testFont = { Name="Helvetica"; Size=8. }
    let fontMetrics = embeddedFontMetrics
    let width = measureString fontMetrics testFont testString |> float32
    (* How I came up with 128.9375, I rendered it with UIKit.  Seems close.
        import UIKit
        
        var str = "This is a test of string measurement."
        let font = UIFont (name: "Helvetica", size: 8)
        let atts = [NSAttributedString.Key.font: font]
        let attStr = NSAttributedString(string: str, attributes: atts as [NSAttributedString.Key : Any])
        attStr.size().width
    
    *)
    Assert.Equal (Math.Truncate (float 128.9375 * 100.), Math.Truncate(float width * 100.))

/// Path to a TrueType font available in the test environment.
let liberationSansPath = "/usr/share/fonts/truetype/liberation/LiberationSans-Regular.ttf"

[<Fact>]
let ``TrueType metrics from file`` () =
    if not (System.IO.File.Exists liberationSansPath) then
        ()   // skip gracefully when font is not installed
    else
        let data = System.IO.File.ReadAllBytes liberationSansPath
        let metrics = FsPdf.TrueType.read data
        // PostScript name should be non-empty
        Assert.False (System.String.IsNullOrWhiteSpace metrics.PostScriptName)
        // Widths array must have 256 entries
        Assert.Equal (256, metrics.CharWidths.Length)
        // Space character (32) should have a non-zero width
        Assert.True (metrics.CharWidths.[32] > 0)
        // Ascent should be positive, descent non-positive
        Assert.True (metrics.Ascent > 0)
        Assert.True (metrics.Descent <= 0)

[<Fact>]
let ``Embedded font PDF`` () =
    if not (System.IO.File.Exists liberationSansPath) then
        ()   // skip gracefully when font is not installed
    else
        let pdfName =
            sprintf "%s.pdf" (System.Reflection.MethodBase.GetCurrentMethod().Name)
        System.IO.File.Delete pdfName
        use stream = System.IO.File.OpenWrite pdfName
        let embeddedFont = EmbeddedFont.fromFile liberationSansPath
        let pdf =
            {
                Catalog =
                    {
                        PageLayout = SinglePage
                        DefaultMedia = Media.Letter
                        Pages =
                            [
                                {
                                    Resources =
                                        Map.empty
                                        |> Map.add "F1" (EmbeddedFontResource embeddedFont)
                                    Contents =
                                        [
                                            BeginText
                                            FontSize ("F1", 24.)
                                            NextLineTranslate (72, 700)
                                            ShowText "Hello from an embedded TrueType font!"
                                            NextLineTranslate (0, -36)
                                            FontSize ("F1", 12.)
                                            ShowText "FsPdf supports embedding custom fonts in PDF files."
                                            EndText
                                        ]
                                    MediaSize = Some Letter
                                }
                            ]
                    }
                Info = None
            }
        PdfObject.writePdf stream (pdf |> PdfFile.build)
        stream.Close ()
        Assert.True (System.IO.File.Exists pdfName)
        // PDF must contain the required header
        let fileBytes = System.IO.File.ReadAllBytes pdfName
        let text = System.Text.Encoding.ASCII.GetString (fileBytes : byte array)
        Assert.Contains ("%PDF-1.7", text)

[<Fact>]
let ``Build 5 page PDF`` () =
    let pdfName =
        sprintf "%s.pdf" (System.Reflection.MethodBase.GetCurrentMethod().Name)
    System.IO.File.Delete (pdfName)
    use stream = System.IO.File.OpenWrite (pdfName)
    let pdf =
        {
            Catalog =
                {
                    PageLayout = SinglePage
                    DefaultMedia = Media.Letter
                    Pages =
                        [
                            {
                                Resources =
                                    Map.empty
                                    |> Map.add "F1" (FontResource (Type1, "Helvetica"))
                                    |> Map.add "F2" (FontResource (Type1, "Courier"))
                                Contents = colorfulTextContent
                                MediaSize = Some (Letter)
                            }
                            {
                                Resources =
                                    Map.empty
                                    |> Map.add "F1" (FontResource (Type1, "Times-Roman"))
                                Contents = wrappedTextContent
                                MediaSize = Some (Letter)
                            }
                            {
                                Resources =
                                    Map.empty
                                    |> Map.add "F1" (FontResource (Type1, "Times-Roman"))
                                Contents = someShapesContent
                                MediaSize = Some (Letter)
                            }
                            {
                                Resources =
                                    Map.empty
                                Contents = spiralRectanges
                                MediaSize = Some (Letter)
                            }
                            {
                                Resources =
                                    Map.empty
                                Contents = triangle
                                MediaSize = Some (Letter)
                            }
                        ]
                }
            Info = None
        }
    PdfObject.writePdf stream (pdf |> PdfFile.build)
    stream.Close ()
    ()

[<Fact>]
let ``Paginate text into multiple pages`` () =
    let pdfName =
        sprintf "%s.pdf" (System.Reflection.MethodBase.GetCurrentMethod().Name)
    System.IO.File.Delete pdfName
    use stream = System.IO.File.OpenWrite pdfName
    let text =
        "F# is a mature, open source, cross-platform, functional-first programming language. \
         It empowers users and organizations to tackle complex computing problems with simple, \
         maintainable and robust code."
    let font = { Name = "Times-Roman"; Size = 12. }
    let paragraph t =
        TextParagraph { Text = t; Font = font; FontKey = "F1"; Leading = 15. }
    // 50 paragraphs is far more than fits on a single Letter page.
    let blocks = [ for _ in 1..50 -> paragraph text ]
    let doc = Document.toDocument embeddedFontMetrics PageSettings.letter blocks
    Assert.True (doc.Catalog.Pages.Length > 1)
    doc |> PdfFile.build |> PdfObject.writePdf stream

[<Fact>]
let ``Explicit page break starts new page`` () =
    let font = { Name = "Times-Roman"; Size = 12. }
    let para t =
        TextParagraph { Text = t; Font = font; FontKey = "F1"; Leading = 15. }
    let blocks =
        [
            para "This is chapter one."
            PageBreak
            para "This is chapter two."
        ]
    let doc = Document.toDocument embeddedFontMetrics PageSettings.letter blocks
    Assert.Equal (2, doc.Catalog.Pages.Length)

[<Fact>]
let ``Short content fits on a single page`` () =
    let font = { Name = "Times-Roman"; Size = 12. }
    let blocks =
        [ TextParagraph { Text = "Hello world."; Font = font; FontKey = "F1"; Leading = 15. } ]
    let doc = Document.toDocument embeddedFontMetrics PageSettings.letter blocks
    Assert.Equal (1, doc.Catalog.Pages.Length)

[<Fact>]
let ``Paginated document writes valid PDF`` () =
    let pdfName =
        sprintf "%s.pdf" (System.Reflection.MethodBase.GetCurrentMethod().Name)
    System.IO.File.Delete pdfName
    use stream = System.IO.File.OpenWrite pdfName
    let font = { Name = "Helvetica"; Size = 11. }
    let chapter title body =
        [
            TextParagraph { Text = title; Font = { font with Size = 18. }; FontKey = "F1"; Leading = 22. }
            TextParagraph { Text = body;  Font = font;                     FontKey = "F1"; Leading = 14. }
        ]
    let blocks =
        [
            yield! chapter "Chapter 1" "The quick brown fox jumps over the lazy dog. \
                The quick brown fox jumps over the lazy dog. \
                The quick brown fox jumps over the lazy dog."
            PageBreak
            yield! chapter "Chapter 2" "Pack my box with five dozen liquor jugs. \
                Pack my box with five dozen liquor jugs. \
                Pack my box with five dozen liquor jugs."
        ]
    let doc = Document.toDocument embeddedFontMetrics PageSettings.letter blocks
    Assert.Equal (2, doc.Catalog.Pages.Length)
    doc |> PdfFile.build |> PdfObject.writePdf stream
