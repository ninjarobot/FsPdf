module Tests

open System
open Xunit
open FsPdf.Pdf

[<Fact>]
let ``Simple PDF`` () =
    let content = "F# is a mature, open source, cross-platform, functional-first programming language. It empowers users and organizations to tackle complex computing problems with simple, maintainable and robust code."
    let font = { Name="Times-Roman"; Size=18. }
    use reader = new System.IO.StringReader (content)
    let lines =
        Afm.wrapString Afm.embeddedFontMetrics font 550. reader
        |> Seq.map (fun line -> [ShowText line; NextLine])
        |> Seq.concat |> List.ofSeq
    let content =
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
        ] |> List.map Instructions.instruction |>  String.concat " "
        |> System.Text.Encoding.UTF8.GetBytes
    let content2 =
        [
            Move (200,350)
            LineTo (500, 750)
            LineTo (600, 300)
            LineTo (175, 220)
            ClosePath
            RGBFill (0.3, 0.0, 0.4)
            RGBStroke (0.7, 0.0, 0.7)
            Width 5.0
            FillStroke
            PushGraphicsState
            Move (10, 20)
            Curve (5, 10, 60, 50, 75, 100)
            LineTo (75, 50)
            LineTo (10, 50)
            RGBFill (0., 0.9, 0.6)
            RGBStroke (0., 0., 0.)
            Width 2.0
            CloseFillStroke
            PopGraphicsState
            Move (250, 250)
            LineTo (300, 250)
            LineTo (300, 300)
            LineTo (250, 300)
            Width 2.0
            System.Drawing.Color.Goldenrod |> toRGBStroke
            CloseFillStroke
        ] @ (Shapes.rectange {x=400; y=200} {x=600; y=275} System.Drawing.Color.SteelBlue (System.Drawing.Color.Black, 2.))
        |> List.map Instructions.instruction |> String.concat " " |> System.Text.Encoding.UTF8.GetBytes
    let content3 =
        let ins =[ 1..174 ] |> List.pairwise |> List.mapi (fun idx (color1, color2) ->
                [
                    yield! (Shapes.rectange {x=idx + 10; y=idx + 10} {x=idx + 75; y=idx + 30} (enum<System.Drawing.KnownColor> color1 |> System.Drawing.Color.FromKnownColor) (enum<System.Drawing.KnownColor> color2 |> System.Drawing.Color.FromKnownColor, 2.))
                    yield Translate (3, 4)
                    yield Rotate (0.1)
                ]
            )
        [Translate (350, 450)] :: ins |> List.concat |> List.map Instructions.instruction |> String.concat " " |> System.Text.Encoding.UTF8.GetBytes
    let pdf =
        [
            PIndObj (1, 0,
                [
                    "Type", PString ("Catalog")
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
                    "Type", PString ("Page")
                    "Parent", PReference (2, 0)
                    "Resources", [
                        "Font", [
                            "F1", [
                                "Type", PName "Font"
                                "Subtype", PName "Type1"
                                "BaseFont", PName "Times-Roman"
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
    System.IO.File.Delete ("/tmp/test.pdf")
    use stream = System.IO.File.OpenWrite ("/tmp/test.pdf")
    use writer = new System.IO.StreamWriter (stream)
    PdfObject.writePreamble (writer)
    let xrefs = ResizeArray<XRef>()
    pdf |> List.iter (fun p -> p |> PdfObject.writeSource writer xrefs; writer.WriteLine ())
    writer.Flush ()
    let startxref = writer.BaseStream.Position
    PdfObject.writeXrefSection writer xrefs
    PdfObject.writeTrailer writer startxref
    Assert.True(true)

[<Fact>]
let ``Measure a string`` () =
    let testString = "This is a test of string measurement."
    let testFont = { Name="Helvetica"; Size=8. }
    let fontMetrics = Afm.embeddedFontMetrics
    let width = Afm.measureString fontMetrics testFont testString |> float32
    (* How I came up with 128.9375, I rendered it with UIKit.  Seems close.
        import UIKit
        
        var str = "This is a test of string measurement."
        let font = UIFont (name: "Helvetica", size: 8)
        let atts = [NSAttributedString.Key.font: font]
        let attStr = NSAttributedString(string: str, attributes: atts as [NSAttributedString.Key : Any])
        attStr.size().width
    
    *)
    Assert.Equal (Math.Truncate (float 128.9375 * 100.), Math.Truncate(float width * 100.))
