module Cthom06.Wrecker.Http

open System.IO
open System.Text

type Header = Map<string, string list>

type Body =
    | ByteBody of byte[]
    | StreamBody of Stream
    | EmptyBody
    with
        static member fromString : string -> Body =
            Encoding.UTF8.GetBytes >> ByteBody

type Request = {
    Method : string
    Url : System.Uri
    Version : string
    Header : Header
    Body : Body
    }

type Response = {
    Version : string
    Code : int
    Reason : string
    Header : Header
    Body : Body
    }

let canonHeader (name : string) =
    System.String.Join ("-", name.Split ('-') |> Seq.map (fun s ->
        if s.Length = 0 then
            s
        else
            (s.Substring (0, 1)).ToUpper () + s.Substring 1))
