namespace FsPdf

type PdfObject =
    | PName of string
    | PInteger of int
    | PString of string
    | PReference of Object:int * Generation:int
    | PArray of PdfObject list
    | PDictionary of Map<string, PdfObject>
    | PStream of Map<string, PdfObject> * byte array
    | PIndObj of Object:int * Generation:int * PdfObject

type XRef =
    {
        Object:int
        Generation:int
        InUse : bool
        Offset: int64
    }

module PdfObject =

    let rec writeSource (writer:System.IO.StreamWriter) (xrefs:ResizeArray<XRef>) = function
        | PName n -> writer.Write (System.String.Format ("/{0}", n))
        | PInteger i -> writer.Write i
        | PString s -> writer.Write (System.String.Format ("({0})", s))
        | PReference (o, g) -> writer.Write (System.String.Format ("{0} {1} R", o, g))
        | PArray arr ->
            writer.Write "["
            arr |> List.iter (fun item ->
                writer.Write (" ")
                item |> writeSource writer xrefs
                writer.Write (" ")
            )
            writer.Write "]"
        | PDictionary d ->
            writer.Write "<<"
            writer.Write System.Environment.NewLine
            writer.Write System.Environment.NewLine
            d |> Map.iter (fun name pdfObj ->
                writer.Write (System.String.Format("/{0} ", name))
                pdfObj |> writeSource writer xrefs
                writer.Write System.Environment.NewLine
            )
            writer.Write ">>"
        | PIndObj (i, g, pdfObject) ->
            writer.Flush()
            let newXref =
                {
                     Object = i
                     Generation = g
                     InUse = true
                     Offset = writer.BaseStream.Position
                }
            writer.Write (System.String.Format("{0} {1} obj", i, g))
            writer.Write System.Environment.NewLine
            pdfObject |> writeSource writer xrefs
            writer.Write "endobj"
            writer.Write System.Environment.NewLine
            xrefs.Add newXref
        | PStream (d, bytes) ->
            writer.WriteLine ("<< /Length {0} >>", bytes.Length)
            writer.Write "stream"
            writer.Write System.Environment.NewLine
            writer.Flush ()
            writer.BaseStream.Write (bytes, 0, bytes.Length)
            writer.BaseStream.Flush ()
            writer.Write System.Environment.NewLine
            writer.Write ("endstream")
            writer.Write System.Environment.NewLine
    
    let writePreamble (writer:System.IO.StreamWriter) =
        writer.Write ("%PDF-1.7")
        writer.Write System.Environment.NewLine
        writer.Write ("%\u15B4\u266F\u2502\u227B\u2C92\u2764") // ᖴ♯│≻Ⲓ❤
        writer.Write System.Environment.NewLine
        writer.Write System.Environment.NewLine
        
    let writeXrefSection (writer:System.IO.StreamWriter) (xrefs:ResizeArray<XRef>) =
        writer.Write "xref"
        writer.Write System.Environment.NewLine
        writer.Write (System.String.Format ("{0} {1}", 0, xrefs.Count + 1))
        writer.Write System.Environment.NewLine
        writer.Write ("0000000000")
        writer.Write (" ")
        writer.Write ("65535")
        writer.Write (" ")
        writer.Write "f"
        writer.Write System.Environment.NewLine
        for xref in xrefs do
            writer.Write (xref.Offset.ToString("0000000000"))
            writer.Write (" ")
            writer.Write (xref.Generation.ToString("00000"))
            writer.Write (" ")
            if xref.InUse then
                writer.Write "n"
            else
                writer.Write "f"
            writer.Write System.Environment.NewLine
        writer.Write System.Environment.NewLine
    
    let writeTrailer (writer:System.IO.StreamWriter) (xrefStart:int64) (numXRefs:int) =
        writer.Write "trailer"
        let xrefs = new ResizeArray<XRef> ()
        writer.Write System.Environment.NewLine
        [
            "Root", PReference (1, 0)
            "Size", PInteger numXRefs
        ] |> Map.ofList |> PDictionary |> writeSource writer xrefs
        writer.Write "startxref"
        writer.Write System.Environment.NewLine
        writer.Write (xrefStart |> string)
        writer.Write System.Environment.NewLine
        writer.Write "%%EOF"
        writer.Write System.Environment.NewLine
