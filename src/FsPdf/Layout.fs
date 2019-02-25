namespace FsPdf

module Layout =
    type FontSubtype =
        | Type1
        | TrueType
        | Type3
        
    type Resource =
        | FontResource of Key:string * FontSubtype * Name:string
    
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
        with
            /// Builds the MediaBox dictionary pair for a page.
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
                "MediaBox", PArray [PInteger 0; PInteger 0; PInteger width; PInteger height]
    
    type Page =
        {
            Resources: Map<string, Resource list>
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
        }
    
    module Catalog =
        let PdfObject catalog =
            [
                "Type", PString ("Catalog")
                "Pages", PReference (2, 0) // Catalog will be 1, root page will be 2.
            ] |> Map.ofList |> PDictionary
            
        let fromPage pages = { Pages = pages; PageLayout = SinglePage }
    
    type PdfFile =
        {
            Catalog : Catalog
            Info : DocInfo option
        }
    
    // What is looks like...
    let pdf =
        {
            Catalog =
                {
                    PageLayout = SinglePage
                    Pages =
                        [
                            {
                                Resources =
                                    Map.empty
                                    |> Map.add "Font" [FontResource ("F1", Type1, "Times-Roman")]
                                Contents = []
                                MediaSize = Some (Letter)
                            }
                        ]
                }
            Info =
                {
                    Title = Some "Sample PDF"
                    Subject = None
                    Keywords = []
                    Author = Some "Dave Curylo"
                    Creator = None
                    Producer = Some "FsPdf"
                } |> Some
        }
