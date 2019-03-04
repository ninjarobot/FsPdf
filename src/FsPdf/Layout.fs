namespace FsPdf

module Layout =
    type FontSubtype =
        | Type1
        | TrueType
        | Type3
        
    type Resource =
        | FontResource of FontSubtype * Name:string
    
    module Resource =
        let pdfObject = function
            | FontResource (subtype, name) ->
                let subtypeName =
                    match subtype with
                        | Type1 -> PName "Type1"
                        | TrueType -> PName "TrueType"
                        | Type3 -> PName "Type3"
                Map.empty
                |> Map.add "Type" (PName "Font")
                |> Map.add "Subtype" subtypeName
                |> Map.add "BaseFont" (PName name)
                |> PDictionary
        
        let resourceDictionary (resources:Map<string, Resource>) =
            // only have fonts now, will need to extend with different types of resources.
            let fonts = System.Collections.Generic.Dictionary<string, PdfObject>()
            for resource in resources do
                match resource.Value with
                | FontResource _ -> fonts.[resource.Key] <- resource.Value |> pdfObject
            let fontDict = fonts |> Seq.map (|KeyValue|) |> Map.ofSeq |> PDictionary
            Map.empty
            |> Map.add "Font" fontDict
            |> PDictionary

    
    type Media =
        | Letter
        | Legal
        | Ledger
        | Tabloid
        | Executive
        | A0
        | A1
        | A2
        | A3
        | A4
        | A5
        | A6
        | A7
        | A8
        | Custom of Width:int * Height:int
        with
            /// Builds the MediaBox array for a page.
            member mb.MediaBox =
                let width, height =
                    match mb with
                    | Letter -> 612, 792
                    | Legal -> 612, 1008
                    | Ledger -> 792, 1224
                    | Tabloid -> 1224, 792
                    | Executive -> 522, 756
                    | A0 -> 2384, 3370
                    | A1 -> 1684, 2384
                    | A2 -> 1190, 1684
                    | A3 -> 842, 1190
                    | A4 -> 595, 842
                    | A5 -> 420, 595
                    | A6 -> 298, 420
                    | A7 -> 210, 298
                    | A8 -> 148, 210
                    | Custom (width, height) -> width, height
                PArray [PInteger 0; PInteger 0; PInteger width; PInteger height]
    
    type Page =
        {
            Resources: Map<string, Resource>
            Contents : FsPdf.Instructions list
            MediaSize: Media option
        }
    
    /// Not sure we need to represent them at this level.
    /// Docs say a page tree should hold no more than 25-50 pages,
    /// and this is just a way to break them up so less have to be
    /// loaded into memory at once.  Probably can save this for
    /// writing the PDF.
    type PageTree =
        {
            Children: Child list
        }
    and Child =
        | PageChild of Page
        | PageTreeChild of PageTree
    
    type PageLayout =
        /// One page at a time (default)
        | SinglePage
        /// Pages in a column
        | OneColumn
        /// Two columns, odd-numbered pages on left
        | TwoColumnLeft
        /// Two columns, odd-numbered pages on right
        | TwoColumnRight
        /// Two pages at a time, odd-numbered pages on left
        | TwoPageLeft
        /// Two pages at a time, odd-numbered pages on right
        | TwoPageRight
    
    type DocInfo =
        {
            Title : string option
            Subject : string option
            Keywords: string list
            Author : string option
            Creator : string option
            Producer : string option
        }
    module DocInfo =
        let PdfObject docInfo =
            [
                if docInfo.Title.IsSome then
                    yield "Title", PString(docInfo.Title.Value)
                if docInfo.Subject.IsSome then
                    yield "Subject", PString(docInfo.Subject.Value)
                if docInfo.Keywords.Length > 0 then
                    yield "Keywords", PString(docInfo.Keywords |> String.concat "; ")
                if docInfo.Author.IsSome then
                    yield "Author", PString(docInfo.Author.Value)
                if docInfo.Creator.IsSome then
                    yield "Creator", PString(docInfo.Creator.Value)
                if docInfo.Producer.IsSome then
                    yield "Producer", PString(docInfo.Producer.Value)
            ] |> Map.ofSeq |> PDictionary
    
    type Catalog =
        {
            Pages : Page list
            PageLayout : PageLayout
            DefaultMedia : Media
        }
    
    module Catalog =
        let PdfObject catalog =
            [
                "Type", PString ("Catalog")
                "Pages", PReference (2, 0) // Catalog will be 1, root page will be 2.
            ] |> Map.ofList |> PDictionary
            
        let fromPage pages = { Pages = pages; DefaultMedia = Letter; PageLayout = SinglePage }
    
    type PdfFile =
        {
            Catalog : Catalog
            Info : DocInfo option
            // TODO: Outline here or groups of pages?
        }
    
    module PdfFile =
        /// Builds a list of PdfObjects ready to write to a PDF file.
        let build (pdfFile:PdfFile) =
            [
                yield PIndObj (1, 0,
                    [
                        "Type", PString ("Catalog")
                        "Pages", PReference (2, 0)
                    ] |> Map.ofList |> PDictionary
                )
                // for simplicity right now, just one deep page tree - TODO: split up to 25-50 page groups
                yield PIndObj (2, 0,
                    [
                        "Type", PName "Pages"
                        "Kids", PArray [
                            PReference (3, 0)
                            PReference (5, 0)
                            PReference (7, 0)
                            PReference (9, 0)
                        ]
                        "Count", PInteger pdfFile.Catalog.Pages.Length
                        "MediaBox", pdfFile.Catalog.DefaultMedia.MediaBox
                    ] |> Map.ofList |> PDictionary
                )
                for idx, page in (pdfFile.Catalog.Pages |> List.indexed) do
                    let pageIdx = 3 + (idx * 2)
                    let contentIdx = 4 + (idx * 2)
                    let contentBytes =
                        page.Contents
                        |> List.map Instructions.instruction
                        |> String.concat " "
                        |> System.Text.Encoding.ASCII.GetBytes
                    yield PIndObj (pageIdx, 0,
                        [
                            "Type", PString ("Page")
                            "Parent", PReference (2, 0)
                            "Resources", page.Resources |> Resource.resourceDictionary
                            "Contents", PReference (contentIdx, 0)
                        ] |> Map.ofList |> PDictionary
                    )
                    yield PIndObj (contentIdx, 0,
                        PStream (
                            [
                                "Length", PInteger contentBytes.Length
                            ] |> Map.ofList ,
                            contentBytes
                        )
                    )
            ]
