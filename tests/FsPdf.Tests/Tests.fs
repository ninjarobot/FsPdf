module Tests

open System
open System.Drawing
open System.Drawing
open Xunit
open FsPdf
open FsPdf.Afm
open FsPdf.Layout

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

/// Mapping of font keys to fonts.
type FontMap () =
    let fontKeys = System.Collections.Generic.Dictionary<Font, string> ()
    
    /// Adds a unique font resource ID.
    member this.Set (font:Font) =
        match fontKeys.TryGetValue font with
        | true, fontKey -> fontKey
        | false, _ ->
            let fontKey = String.Format ("F{0}", fontKeys.Count)
            fontKeys.[font] <- fontKey
            fontKey
    
    /// Gets a read-only copy of the current FontMap.
    member this.GetReadOnlyMap () =
        fontKeys |> Seq.map (fun kvp -> kvp.Value, kvp.Key) |> dict

module Paragraph =
    type Part =
        | Content of string
        | FontSize of int
        | FontName of string
    let content (text:string) = Content text
    let fontSize (size:int) = FontSize size
    let fontName (name:string) = FontName name
    
    type Paragraph =
        {
            Text: string
            Font: Font
        }
    
    let create (parts:Part list) =
        parts |> List.fold
            (fun paragraph part ->
                match part with
                | Content text -> { paragraph with Text = text }
                | FontSize size -> { paragraph with Font = { paragraph.Font with Size = float size } }
                | FontName name -> { paragraph with Font = { paragraph.Font with Name = name } }
            )
            { Text=""; Font = { Name="Times-Roman"; Size=12. } }
    
    let text (content: string) =
        { Text=content; Font = { Name="Times-Roman"; Size=12. } }
    
    let render (fontMap:FontMap) (media:Media) (paragraph:Paragraph) =
        let fontKey = fontMap.Set paragraph.Font
        let text = paragraph.Text
        use reader = new System.IO.StringReader (text)
        let lines =
            wrapString embeddedFontMetrics paragraph.Font (float(media.Dimensions.Width - (Media.Dpi * 2))) reader
            |> Seq.map (fun line -> [ShowText line; NextLine])
            |> Seq.concat
        seq {
            yield BeginText
            yield Leading (20)
            yield Instructions.FontSize (fontKey, paragraph.Font.Size)
            yield! lines
            yield EndText
        }
module Page =
    type Part =
        | Size of Media
        | Resources of Resource list
        | Content of Paragraph.Paragraph list
    let content (paragraphs:Paragraph.Paragraph list) = Content paragraphs
    let size (media:Media) = Size media
    let resources (r:Resource list) = Resources r
        
    type Page =
        {
            Size: Media
            Resources: Resource list
            Content: Paragraph.Paragraph list
        }
    let create (parts:Part list) =
        parts |> List.fold
            (fun page part ->
                match part with
                | Size media -> { page with Page.Size = media }
                | Resources resources -> { page with Resources = resources }
                | Content paragraphs -> { page with Content = paragraphs }
            )
            { Size=Media.Letter; Resources=[]; Content=[] }

    let render (page:Page) =
        let spaceBetweenParagraphs = 30
        let fontMap = FontMap ()
        let renderedParagraphs = page.Content |> Seq.map (Paragraph.render fontMap page.Size)
        seq {
            for paragraph in page.Content |> Seq.map (Paragraph.render fontMap page.Size) do
                yield! paragraph
                yield NextLineTranslate (spaceBetweenParagraphs, 0)
        }

module Document =
    type Part =
        | Pages of Page.Page list
    
    type Document =
        {
            Pages: Page.Page list
        }
    let pages (pages:Page.Page list) = Pages pages
    let create (parts:Part list) =
        parts |> List.fold
            (fun doc part ->
                match part with
                | Pages pages -> { doc with Document.Pages = pages }
            )
            { Pages=[] }

/// Builds a paragraph record.
[<Fact>]
let ``Build Paragraph`` () =
    let p = 
        Paragraph.create [
            Paragraph.content "Hello world"
            Paragraph.fontSize 24
        ]
    Assert.Equal(24., p.Font.Size)
    Assert.Equal("Times New Roman", p.Font.Name)
    Assert.Equal("Hello world", p.Text)

/// Builds a page record containing some paragraphs
[<Fact>]
let ``Build Page`` () =
    let page =
        Page.create [
            Page.size Media.A4
            Page.resources [ ]
            Page.content [
                Paragraph.create [
                    Paragraph.content "Hello world"
                    Paragraph.fontSize 24
                ]
                Paragraph.text "This is some text underneath the header"
            ]
        ]
    Assert.Equal(2, page.Content.Length)
    
/// High level language more representative of how people work with PDF
/// documents.
let ``Higher level language example`` () =
    // Concept from Zaid Ajaj
    Document.create [
        Document.pages [
            Page.create [
                Page.size Media.A4
                Page.resources [ ]
                Page.content [
                    Paragraph.create [
                        Paragraph.content "Hello world"
                        Paragraph.fontSize 24
                    ]
                    Paragraph.text "This is some text underneath the header"
                ]
            ]
        ]
    ]
