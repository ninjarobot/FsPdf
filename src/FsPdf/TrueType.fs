namespace FsPdf

/// Minimal TrueType / OpenType font metrics reader for PDF font embedding.
/// Supports fonts with TrueType outlines (glyf table); CFF-based OpenType
/// fonts require FontFile3 and are not currently handled here.
module TrueType =

    // -----------------------------------------------------------------------
    // Big-endian binary helpers
    // -----------------------------------------------------------------------

    let private readUInt16BE (data: byte array) offset =
        (int data.[offset] <<< 8) ||| int data.[offset + 1]

    let private readInt16BE (data: byte array) offset =
        let v = readUInt16BE data offset
        if v > 0x7FFF then v - 0x10000 else v

    let private readUInt32BE (data: byte array) offset =
        (int data.[offset] <<< 24) |||
        (int data.[offset + 1] <<< 16) |||
        (int data.[offset + 2] <<< 8) |||
        int data.[offset + 3]

    let private readTag (data: byte array) offset =
        System.String [| char data.[offset]; char data.[offset + 1]; char data.[offset + 2]; char data.[offset + 3] |]

    // -----------------------------------------------------------------------
    // Table directory
    // -----------------------------------------------------------------------

    /// Returns (tableOffset, tableLength) for the named table, or None.
    let private findTable (data: byte array) (tag: string) =
        let numTables = readUInt16BE data 4
        seq { 0 .. numTables - 1 }
        |> Seq.tryFind (fun i -> readTag data (12 + i * 16) = tag)
        |> Option.map (fun i ->
            let base_ = 12 + i * 16
            readUInt32BE data (base_ + 8), readUInt32BE data (base_ + 12)
        )

    // -----------------------------------------------------------------------
    // Public type
    // -----------------------------------------------------------------------

    type FontMetrics = {
        /// PostScript font name (from the name table, nameID 6).
        PostScriptName: string
        /// Design units per em.
        UnitsPerEm: int
        /// Font bounding box in design units.
        XMin: int
        YMin: int
        XMax: int
        YMax: int
        /// Typographic ascender in design units.
        Ascent: int
        /// Typographic descender in design units (usually negative).
        Descent: int
        /// Capital-height in design units.
        CapHeight: int
        /// Italic angle in degrees (counter-clockwise from vertical).
        ItalicAngle: float
        /// PDF FontDescriptor Flags bitfield.
        Flags: int
        /// Dominant vertical stem width (used for hinting hints).
        StemV: int
        /// Advance widths for char codes 0–255 in 1/1000 of a point.
        CharWidths: int array
    }

    let private defaultMetrics = {
        PostScriptName = "UnknownFont"
        UnitsPerEm = 1000
        XMin = -200; YMin = -200; XMax = 1000; YMax = 1000
        Ascent = 800; Descent = -200; CapHeight = 700
        ItalicAngle = 0.0; Flags = 32; StemV = 80
        CharWidths = Array.create 256 600
    }

    // -----------------------------------------------------------------------
    // Table parsers
    // -----------------------------------------------------------------------

    /// head table — returns (unitsPerEm, xMin, yMin, xMax, yMax).
    let private parseHead (data: byte array) =
        match findTable data "head" with
        | None -> 1000, -200, -200, 1000, 1000
        | Some (off, _) ->
            readUInt16BE data (off + 18),
            readInt16BE  data (off + 36),
            readInt16BE  data (off + 38),
            readInt16BE  data (off + 40),
            readInt16BE  data (off + 42)

    /// hhea table — returns (ascent, descent, numberOfHMetrics).
    let private parseHhea (data: byte array) =
        match findTable data "hhea" with
        | None -> 800, -200, 0
        | Some (off, _) ->
            readInt16BE  data (off + 4),
            readInt16BE  data (off + 6),
            readUInt16BE data (off + 34)

    /// OS/2 table — returns (capHeight, flags, weight).
    let private parseOs2 (data: byte array) ascent =
        match findTable data "OS/2" with
        | None -> (ascent * 7 / 10), 32, 400
        | Some (off, length) ->
            let version = readUInt16BE data off
            let weight  = readUInt16BE data (off + 4)
            let capH =
                if version >= 2 && length >= 90 then readInt16BE data (off + 88)
                else ascent * 7 / 10
            let selection = readUInt16BE data (off + 62)
            let isItalic  = selection &&& 0x01 <> 0
            let flags = 32 ||| (if isItalic then 64 else 0) // NonSymbolic [+ Italic]
            capH, flags, weight

    /// post table — returns italic angle as a float.
    let private parsePost (data: byte array) =
        match findTable data "post" with
        | None -> 0.0
        | Some (off, _) ->
            let intPart  = readInt16BE  data (off + 4)
            let fracPart = readUInt16BE data (off + 6)
            float intPart + float fracPart / 65536.0

    /// name table — returns the PostScript name (nameID 6) if found.
    let private parseName (data: byte array) =
        match findTable data "name" with
        | None -> None
        | Some (off, _) ->
            let count       = readUInt16BE data (off + 2)
            let strBase     = off + readUInt16BE data (off + 4)
            seq { 0 .. count - 1 }
            |> Seq.tryPick (fun i ->
                let recOff     = off + 6 + i * 12
                let platformID = readUInt16BE data  recOff
                let nameID     = readUInt16BE data (recOff + 6)
                let length     = readUInt16BE data (recOff + 8)
                let strOff     = strBase + readUInt16BE data (recOff + 10)
                if nameID = 6 then
                    if platformID = 3 then
                        // Windows Unicode UTF-16 BE
                        let chars = Array.init (length / 2) (fun j ->
                            char (readUInt16BE data (strOff + j * 2)))
                        Some (System.String chars)
                    elif platformID = 1 then
                        Some (System.Text.Encoding.GetEncoding("iso-8859-1").GetString(data, strOff, length))
                    else None
                else None
            )

    /// cmap table — returns a char-code → glyph-ID mapping for codes 0–255.
    let private parseCmap (data: byte array) =
        let empty () = System.Collections.Generic.Dictionary<int, int>() :> System.Collections.Generic.IDictionary<int, int>
        match findTable data "cmap" with
        | None -> empty ()
        | Some (off, _) ->
            let numSubtables = readUInt16BE data (off + 2)
            // Prefer Windows Unicode BMP (3,1) then Unicode (0,3/4)
            let subtableOffset =
                seq { 0 .. numSubtables - 1 }
                |> Seq.tryPick (fun i ->
                    let recOff     = off + 4 + i * 8
                    let platformID = readUInt16BE data  recOff
                    let encodingID = readUInt16BE data (recOff + 2)
                    let subOff     = off + readUInt32BE data (recOff + 4)
                    if (platformID = 3 && encodingID = 1) ||
                       (platformID = 0 && (encodingID = 3 || encodingID = 4)) then
                        Some subOff
                    else None
                )
            match subtableOffset with
            | None -> empty ()
            | Some subOff ->
                let format = readUInt16BE data subOff
                if format <> 4 then empty ()
                else
                    let segCount         = readUInt16BE data (subOff + 6) / 2
                    let endCountOff      = subOff + 14
                    let startCountOff    = endCountOff + segCount * 2 + 2
                    let idDeltaOff       = startCountOff + segCount * 2
                    let idRangeOffOff    = idDeltaOff + segCount * 2
                    let map = System.Collections.Generic.Dictionary<int, int>()
                    for seg in 0 .. segCount - 2 do   // last segment is 0xFFFF terminator
                        let endCode   = readUInt16BE data (endCountOff   + seg * 2)
                        let startCode = readUInt16BE data (startCountOff + seg * 2)
                        let idDelta   = readInt16BE  data (idDeltaOff    + seg * 2)
                        let iro       = readUInt16BE data (idRangeOffOff + seg * 2)
                        for c in startCode .. min endCode 255 do
                            if iro = 0 then
                                map.[c] <- (c + idDelta) &&& 0xFFFF
                            else
                                let glyphPtrOff = idRangeOffOff + seg * 2 + iro + (c - startCode) * 2
                                let glyphId = readUInt16BE data glyphPtrOff
                                if glyphId <> 0 then
                                    map.[c] <- (glyphId + idDelta) &&& 0xFFFF
                    map :> _

    /// hmtx + maxp — returns an array of advance widths indexed by glyph ID.
    let private parseGlyphWidths (data: byte array) numberOfHMetrics =
        match findTable data "hmtx", findTable data "maxp" with
        | Some (hmtxOff, _), Some (maxpOff, _) ->
            let numGlyphs = readUInt16BE data (maxpOff + 4)
            let widths = Array.zeroCreate numGlyphs
            for i in 0 .. numberOfHMetrics - 1 do
                widths.[i] <- readUInt16BE data (hmtxOff + i * 4)
            let lastWidth =
                if numberOfHMetrics > 0 then widths.[numberOfHMetrics - 1] else 0
            for i in numberOfHMetrics .. numGlyphs - 1 do
                widths.[i] <- lastWidth
            widths
        | _ -> Array.empty

    // -----------------------------------------------------------------------
    // Public API
    // -----------------------------------------------------------------------

    /// Reads TrueType font metrics from the raw font bytes.
    /// Gracefully returns defaults if tables are missing or parsing fails.
    let read (data: byte array) : FontMetrics =
        try
            let upm, xMin, yMin, xMax, yMax = parseHead data
            let ascent, descent, numberOfHMetrics = parseHhea data
            let capHeight, flags, weight = parseOs2 data ascent
            let italicAngle = parsePost data
            let psName = parseName data |> Option.defaultValue "UnknownFont"
            let charToGlyph = parseCmap data
            let glyphWidths = parseGlyphWidths data numberOfHMetrics

            // StemV estimate from weight class
            let stemV =
                if weight >= 700 then 140
                elif weight >= 500 then 100
                else 80

            // Build per-char-code widths in 1/1000 of a design unit.
            // Codes 0–31 are control characters not used in WinAnsiEncoding, so leave them as 0.
            let charWidths = Array.create 256 0
            for c in 32 .. 255 do
                let glyphId =
                    match charToGlyph.TryGetValue c with
                    | true, g -> g
                    | _       -> 0
                let rawWidth =
                    if glyphId > 0 && glyphId < glyphWidths.Length then
                        glyphWidths.[glyphId]
                    elif glyphWidths.Length > 0 then
                        glyphWidths.[0]   // .notdef width
                    else 600
                charWidths.[c] <- rawWidth * 1000 / upm

            {
                PostScriptName = psName
                UnitsPerEm     = upm
                XMin           = xMin * 1000 / upm
                YMin           = yMin * 1000 / upm
                XMax           = xMax * 1000 / upm
                YMax           = yMax * 1000 / upm
                Ascent         = ascent   * 1000 / upm
                Descent        = descent  * 1000 / upm
                CapHeight      = capHeight * 1000 / upm
                ItalicAngle    = italicAngle
                Flags          = flags
                StemV          = stemV
                CharWidths     = charWidths
            }
        with ex ->
            // Return safe defaults so callers can still produce a PDF even if the font
            // file is malformed or an unsupported table layout is encountered.
            // The PostScriptName is set to indicate a parsing failure for diagnostics.
            { defaultMetrics with PostScriptName = sprintf "UnknownFont_%s" (ex.GetType().Name) }
