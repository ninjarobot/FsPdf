module Tests

open System
open System.IO
open Xunit
open FsPdf.Pdf

[<Fact>]
let ``Simple PDF`` () =
    let content =
        [
            "BT"
            "/F1 18 Tf"
            "20 500 Td"
            "(Hello World) Tj"
            "ED"
        ] |> String.concat "\n"
        |> System.Text.Encoding.UTF8.GetBytes
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
    use stream = System.IO.File.OpenWrite ("/tmp/test.pdf")
    use writer = new System.IO.BinaryWriter (stream)
    PdfObject.writePreamble (writer)
    let xrefs = ResizeArray<XRef>()
    pdf |> List.iter (PdfObject.writeSource writer xrefs)
    let startxref = writer.BaseStream.Position
    PdfObject.writeXrefSection writer xrefs
    PdfObject.writeTrailer writer startxref
    Assert.True(true)
