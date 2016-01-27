module Cthom06.Wrecker.Util

open Cthom06.Wrecker.Http
open Cthom06.Wrecker.Routing
open System.IO
open System.IO.Compression

let private defaultMimeTypes =
    [ ".html", "text/html; charset=utf-8"
      ".css", "text/css; charset=utf-8"
      ".js", "application/x-javascript"
      ".txt", "text/plain; charset=utf-8"
      ".gif", "image/gif"
      ".jpg", "image/jpeg"
      ".png", "image/png" ]
    |> Map.ofList

type StaticHandlerConfig = {
    Prefix : string
    Root : string
    MimeTypes : Map<string, string>
    NotFound : Request -> Response
    }
    with
        static member Default =
            { Prefix = "/"
              Root = "/var/www"
              MimeTypes = defaultMimeTypes
              NotFound = (fun _ -> None) |> or404 }

let private getStaticFileHeaders (stat : FileInfo) =
    let fileType =
        match Map.tryFind (Path.GetExtension stat.FullName) defaultMimeTypes with
        | Some mime -> mime
        | None -> "application/octet-stream"
    [ "Content-Type", [fileType]
      "Content-Length", [stat.Length.ToString ()]
      "Last-Modified", [stat.LastWriteTimeUtc.ToString "R"] ]
    |> Map.ofList

let private fileResp stat (req : Request) =
    let headers = getStaticFileHeaders stat
    let body =
        match req.Method with
        | "HEAD" -> ByteBody [||]
        | "GET" -> StreamBody (stat.OpenRead ())
        | _ -> raise <| new System.InvalidOperationException ("Non head/get request passed to static handler")
    { Code = 200
      Reason = "OK"
      Version = req.Version
      Header = headers
      Body = body }

let private unmodResp (req : Request) =
    { Code = 304
      Reason = "Not Modified"
      Version = req.Version
      Header = Map.empty
      Body = EmptyBody }

let rec private doFile (req : Request) path =
    try
        let stat = new FileInfo (path)
        if stat.Exists then
            match Map.tryFind "If-Modified-Since" req.Header with
            | Some (d::_) ->
                let dr = ref System.DateTime.MinValue
                if System.DateTime.TryParse (d, dr) && !dr <= stat.LastAccessTimeUtc then
                    Some (unmodResp req)
                else
                    Some (fileResp stat req)
                        
            | _ ->
                Some (fileResp stat req)
        else
            tryIndex req path
    with
    | :? FileNotFoundException -> None

and private tryIndex req path =
    let stat = new DirectoryInfo (path)
    if stat.Exists then
        Path.Combine (path, "index.html")
        |> doFile req
    else
        None

let handleStatic (config : StaticHandlerConfig) (req : Request) =
    let f req = 
        Path.Combine (config.Root, req.Url.AbsolutePath.Substring config.Prefix.Length)
        |> doFile req
        |> Option.orDefault (config.NotFound req)
    [ config.Prefix, 
        [ "GET", f
          "HEAD", f ]
        |> plexMethod >> Option.orDefault (config.NotFound req) ]
    |> plexPathPrefix >> Option.orDefault (config.NotFound req)
    <| req
