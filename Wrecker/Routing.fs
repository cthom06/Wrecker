module Cthom06.Wrecker.Routing

open Cthom06.Wrecker.Http
open System.Text.RegularExpressions

let try500 f (req : Request) =
    try
        f req
    with
    | e ->
        let body = "500 - An error occurred"
        { Code = 500
          Reason = "Server Error"
          Version = req.Version
          Header = Map.empty
          Body = Body.fromString body }

let or404 f (req : Request) =
    f req
    |> Option.orDefault
        { Code = 404
          Reason = "Not Found"
          Version = req.Version
          Header = Map.empty
          Body = Body.fromString "404 - Resource not found" }

let plexMethod handlers =
    let m = Map.ofSeq handlers
    fun (req : Request) ->
        Map.tryFind req.Method m
        |> Option.map (fun f -> f req)

let plexHost handlers =
    let m = Map.ofSeq handlers
    fun (req : Request) ->
        Map.tryFind req.Url.Host m
        |> Option.map (fun f -> f req)

let plexPath handlers =
    let m = Map.ofSeq handlers
    fun (req : Request) ->
        Map.tryFind req.Url.AbsolutePath m
        |> Option.map (fun f -> f req)

let plexPathPrefix handlers (req : Request) =
    List.tryFind (fst >> req.Url.AbsolutePath.StartsWith) handlers
    |> Option.map (fun (_,f) -> f req)

let plexPathRegex (handlers : (Regex * _) list) (req : Request) =
    handlers
    |> Seq.map (fun (pattern,f) -> (pattern.Match req.Url.AbsolutePath), f)
    |> Seq.tryFind (fun (mtch, f) -> mtch.Success)
    |> Option.map (fun (mtch,f) -> f mtch req)