namespace FsPdf

/// Higher-level document abstraction for building multi-page documents without
/// manually managing page breaks or text wrapping.
module Document =

    open Afm
    open Layout

    /// Settings for the page layout including margins.
    type PageSettings =
        {
            /// The media size of the pages.
            Media: Media
            /// Top margin in points (1 inch = 72 points).
            TopMargin: float
            /// Bottom margin in points.
            BottomMargin: float
            /// Left margin in points.
            LeftMargin: float
            /// Right margin in points.
            RightMargin: float
        }

    module PageSettings =
        /// Default settings for US Letter paper with 1-inch (72-point) margins.
        let letter =
            {
                Media = Letter
                TopMargin = 72.
                BottomMargin = 72.
                LeftMargin = 72.
                RightMargin = 72.
            }

        /// Default settings for A4 paper with 1-inch (72-point) margins.
        let a4 =
            {
                Media = A4
                TopMargin = 72.
                BottomMargin = 72.
                LeftMargin = 72.
                RightMargin = 72.
            }

    /// A text paragraph with font and layout settings.
    type Paragraph =
        {
            /// The text content of the paragraph.
            Text: string
            /// The font to render the text with.
            Font: Font
            /// The font resource key used in the page's resource dictionary (e.g. "F1").
            FontKey: string
            /// Line spacing in points. Should be at least as large as the font size.
            Leading: float
        }

    /// A block of content in a document.
    type ContentBlock =
        /// A paragraph of text that is wrapped to fit the page width and split across pages as needed.
        | TextParagraph of Paragraph
        /// Forces a page break; content after this block starts on a new page.
        | PageBreak

    let private mediaDimensions (media: Media) =
        match media with
        | Letter -> 612., 792.
        | Legal -> 612., 1008.
        | Ledger -> 792., 1224.
        | Tabloid -> 1224., 792.
        | Executive -> 522., 756.
        | A0 -> 2384., 3370.
        | A1 -> 1684., 2384.
        | A2 -> 1190., 1684.
        | A3 -> 842., 1190.
        | A4 -> 595., 842.
        | A5 -> 420., 595.
        | A6 -> 298., 420.
        | A7 -> 210., 298.
        | A8 -> 148., 210.
        | Custom (w, h) -> float w, float h

    /// <summary>
    /// Paginates a list of content blocks into pages that fit within the given page settings.
    /// Text paragraphs are automatically wrapped to fit the page width and split across pages
    /// when the content exceeds the available vertical space. A leading-sized gap is added
    /// between consecutive paragraphs.
    /// </summary>
    /// <param name="fontMetrics">Font metrics used to measure text for line wrapping.</param>
    /// <param name="settings">Page settings including media size and margins.</param>
    /// <param name="blocks">The content blocks to lay out.</param>
    /// <returns>A list of pages ready to be included in a Catalog.</returns>
    let paginate
        (fontMetrics: System.Collections.Generic.IDictionary<string, FontMetric>)
        (settings: PageSettings)
        (blocks: ContentBlock list)
        : Page list =
        let pageWidth, pageHeight = mediaDimensions settings.Media
        let textWidth = pageWidth - settings.LeftMargin - settings.RightMargin
        // Y coordinate of the first text baseline on a fresh page.
        let pageStartY = pageHeight - settings.TopMargin

        let pages = ResizeArray<Page> ()
        let currentInstructions = ResizeArray<Instructions> ()
        let mutable currentResources = Map.empty<string, Resource>
        let mutable currentY = pageStartY

        /// Saves the current page (if it has content) and resets page state.
        let finishPage () =
            if currentInstructions.Count > 0 then
                pages.Add
                    {
                        Resources = currentResources
                        Contents = currentInstructions |> List.ofSeq
                        MediaSize = Some settings.Media
                    }
                currentInstructions.Clear ()
                currentResources <- Map.empty
            currentY <- pageStartY

        for block in blocks do
            match block with
            | PageBreak -> finishPage ()
            | TextParagraph para ->
                // Register the font resource for this paragraph on the current page.
                currentResources <-
                    currentResources |> Map.add para.FontKey (FontResource (Type1, para.Font.Name))
                use reader = new System.IO.StringReader (para.Text)
                let allLines = wrapString fontMetrics para.Font textWidth reader |> List.ofSeq
                let mutable remaining = allLines
                while not remaining.IsEmpty do
                    // How many lines fit in the remaining vertical space?
                    let available = int ((currentY - settings.BottomMargin) / para.Leading)
                    if available <= 0 then
                        // No room for even one line; start a new page and re-register the font.
                        finishPage ()
                        currentResources <-
                            currentResources |> Map.add para.FontKey (FontResource (Type1, para.Font.Name))
                    else
                        let fitting = remaining |> List.truncate available
                        remaining <- remaining |> List.skip fitting.Length
                        // Emit a self-contained text block positioned at (leftMargin, currentY).
                        // After BeginText the text origin is (0,0), so NextLineTranslate is
                        // effectively an absolute position.
                        currentInstructions.Add BeginText
                        currentInstructions.Add (FontSize (para.FontKey, para.Font.Size))
                        currentInstructions.Add (Leading (int para.Leading))
                        currentInstructions.Add (NextLineTranslate (int settings.LeftMargin, int currentY))
                        for line in fitting do
                            currentInstructions.Add (ShowText line)
                            currentInstructions.Add NextLine
                        currentInstructions.Add EndText
                        currentY <- currentY - float fitting.Length * para.Leading
                // Add one leading's worth of vertical space between paragraphs.
                currentY <- currentY - para.Leading
                if currentY <= settings.BottomMargin then
                    finishPage ()

        // Flush any remaining content as the final page.
        finishPage ()
        List.ofSeq pages

    /// <summary>
    /// Converts a list of content blocks into a PdfFile using the given page settings.
    /// This is the primary entry point for building a multi-page document from a
    /// sequence of paragraphs and optional page breaks.
    /// </summary>
    /// <param name="fontMetrics">Font metrics used to measure text for line wrapping.</param>
    /// <param name="settings">Page settings including media size and margins.</param>
    /// <param name="blocks">The content blocks to lay out.</param>
    /// <returns>A PdfFile with automatically paginated content.</returns>
    let toDocument
        (fontMetrics: System.Collections.Generic.IDictionary<string, FontMetric>)
        (settings: PageSettings)
        (blocks: ContentBlock list)
        : PdfFile =
        {
            Catalog =
                {
                    PageLayout = SinglePage
                    DefaultMedia = settings.Media
                    Pages = paginate fontMetrics settings blocks
                }
            Info = None
        }
