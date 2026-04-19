namespace FsPdf

module Layout =
    type FontSubtype =
        | Type1
        | TrueType
        | Type3
        
    /// A TrueType or OpenType (TrueType-outline) font to be embedded in the PDF.
    type EmbeddedFont = {
        /// Raw bytes of the font file (.ttf or TrueType-outline .otf).
        FontData : byte array
        /// Metrics parsed from the font file.
        Metrics  : TrueType.FontMetrics
    }

    module EmbeddedFont =
        /// Load an embedded font from a file path.
        let fromFile (path: string) =
            let data = System.IO.File.ReadAllBytes path
            { FontData = data; Metrics = TrueType.read data }

        /// Load an embedded font from a byte array.
        let fromBytes (data: byte array) =
            { FontData = data; Metrics = TrueType.read data }

        /// Load an embedded font from a stream.
        let fromStream (stream: System.IO.Stream) =
            use ms = new System.IO.MemoryStream()
            stream.CopyTo ms
            fromBytes (ms.ToArray())

    type Resource =
        | FontResource of FontSubtype * Name:string
        /// A font whose binary data is embedded inside the PDF file.
        | EmbeddedFontResource of EmbeddedFont
    
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
            | EmbeddedFontResource _ ->
                // Embedded fonts are written as indirect objects by PdfFile.build;
                // pages reference them via PReference. This path should not be called
                // for embedded fonts in a well-formed document.
                PDictionary Map.empty

        /// Builds a resource dictionary for page-level resources.
        /// For embedded fonts, caller should pass a resolved map of key→object number.
        let resourceDictionary (embeddedFontRefs: Map<string, int>) (resources: Map<string, Resource>) =
            let fonts = System.Collections.Generic.Dictionary<string, PdfObject>()
            for resource in resources do
                match resource.Value with
                | FontResource _ ->
                    fonts.[resource.Key] <- resource.Value |> pdfObject
                | EmbeddedFontResource _ ->
                    match embeddedFontRefs |> Map.tryFind resource.Key with
                    | Some objNum -> fonts.[resource.Key] <- PReference (objNum, 0)
                    | None        -> fonts.[resource.Key] <- resource.Value |> pdfObject
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
                "Type", PName "Catalog"
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
        ///
        /// Object layout:
        ///   1             – Catalog
        ///   2             – Pages (root page tree node)
        ///   3 + i*3       – Font dictionary for embedded font i
        ///   4 + i*3       – FontDescriptor for embedded font i
        ///   5 + i*3       – FontFile2 stream for embedded font i
        ///   fontBase+p*2  – Page object for page p
        ///   fontBase+p*2+1– Content stream for page p
        let build (pdfFile: PdfFile) =

            // Collect unique embedded fonts (by resource key) across all pages.
            let embeddedFonts =
                pdfFile.Catalog.Pages
                |> List.collect (fun page ->
                    page.Resources
                    |> Map.toList
                    |> List.choose (fun (key, res) ->
                        match res with
                        | EmbeddedFontResource ef -> Some (key, ef)
                        | _ -> None
                    )
                )
                |> List.distinctBy fst

            let numEmbeddedFonts = embeddedFonts.Length

            // First object number used for page pairs.
            let pageObjectBase = 3 + numEmbeddedFonts * 3

            // Map from resource key → Font-dict object number (for PReference in pages).
            let fontDictObjNums =
                embeddedFonts
                |> List.mapi (fun i (key, _) -> key, 3 + i * 3)
                |> Map.ofList

            [
                yield PIndObj (1, 0,
                    [
                        "Type", PName "Catalog"
                        "Pages", PReference (2, 0)
                    ] |> Map.ofList |> PDictionary
                )

                // Root page tree — page object numbers start at pageObjectBase.
                yield PIndObj (2, 0,
                    [
                        "Type", PName "Pages"
                        "Kids", PArray
                            [ for p in 0 .. pdfFile.Catalog.Pages.Length - 1 do
                                yield PReference (pageObjectBase + p * 2, 0) ]
                        "Count", PInteger pdfFile.Catalog.Pages.Length
                        "MediaBox", pdfFile.Catalog.DefaultMedia.MediaBox
                    ] |> Map.ofList |> PDictionary
                )

                // Embedded-font objects (Font dict + FontDescriptor + FontFile2 stream).
                for i, (_key, ef) in embeddedFonts |> List.indexed do
                    let fontDictObj  = 3 + i * 3
                    let fontDescObj  = 4 + i * 3
                    let fontFileObj  = 5 + i * 3
                    let m = ef.Metrics

                    // Font dictionary
                    yield PIndObj (fontDictObj, 0,
                        [
                            "Type",           PName "Font"
                            "Subtype",        PName "TrueType"
                            "BaseFont",       PName m.PostScriptName
                            "Encoding",       PName "WinAnsiEncoding"
                            "FirstChar",      PInteger 32
                            "LastChar",       PInteger 255
                            "Widths",         PArray [ for c in 32 .. 255 -> PInteger m.CharWidths.[c] ]
                            "FontDescriptor", PReference (fontDescObj, 0)
                        ] |> Map.ofList |> PDictionary
                    )

                    // FontDescriptor
                    yield PIndObj (fontDescObj, 0,
                        [
                            "Type",        PName "FontDescriptor"
                            "FontName",    PName m.PostScriptName
                            "Flags",       PInteger m.Flags
                            "FontBBox",    PArray [ PInteger m.XMin; PInteger m.YMin; PInteger m.XMax; PInteger m.YMax ]
                            "ItalicAngle", PInteger (int m.ItalicAngle)
                            "Ascent",      PInteger m.Ascent
                            "Descent",     PInteger m.Descent
                            "CapHeight",   PInteger m.CapHeight
                            "StemV",       PInteger m.StemV
                            "FontFile2",   PReference (fontFileObj, 0)
                        ] |> Map.ofList |> PDictionary
                    )

                    // FontFile2 stream — raw TrueType font bytes
                    yield PIndObj (fontFileObj, 0,
                        PStream (
                            [
                                "Length",  PInteger ef.FontData.Length
                                "Length1", PInteger ef.FontData.Length
                            ] |> Map.ofList,
                            ef.FontData
                        )
                    )

                // Page pairs
                for idx, page in pdfFile.Catalog.Pages |> List.indexed do
                    let pageIdx    = pageObjectBase + idx * 2
                    let contentIdx = pageObjectBase + idx * 2 + 1
                    let contentBytes =
                        page.Contents
                        |> List.map Instructions.instruction
                        |> String.concat " "
                        |> System.Text.Encoding.ASCII.GetBytes
                    yield PIndObj (pageIdx, 0,
                        [
                            "Type",      PName "Page"
                            "Parent",    PReference (2, 0)
                            "Resources", page.Resources |> Resource.resourceDictionary fontDictObjNums
                            "Contents",  PReference (contentIdx, 0)
                        ] |> Map.ofList |> PDictionary
                    )
                    yield PIndObj (contentIdx, 0,
                        PStream (
                            [ "Length", PInteger contentBytes.Length ] |> Map.ofList,
                            contentBytes
                        )
                    )
            ]
