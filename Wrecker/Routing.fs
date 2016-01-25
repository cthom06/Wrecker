module Cthom06.Wrecker.Routing

open Cthom06.Wrecker.Http
open System.Text.RegularExpressions

let orDef def opt =
    match opt with
    | Some v -> v
    | None -> def

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
    |> orDef
        { Code = 404
          Reason = "Not Found"
          Version = req.Version
          Header = Map.empty
          Body = Body.fromString "404 - Resource not found" }

let onlyGet f (req : Request) =
    if req.Method = "GET" then
        Some (f req)
    else
        None

let onlyPost f (req : Request) =
    if req.Method = "POST" then
        Some (f req)
    else
        None

let plexMethod handlers (req : Request) =
    Map.tryFind req.Method handlers
    |> Option.map (fun f -> f req)

let plexHost handlers (req : Request) =
    Map.tryFind req.Url.Host handlers
    |> Option.map (fun f -> f req)

let plexPathPrefix handlers (req : Request) =
    List.tryFind (fst >> req.Url.AbsolutePath.StartsWith) handlers
    |> Option.map (fun (_,f) -> f req)

let plexPathRegex (handlers : (Regex * _) list) (req : Request) =
    handlers
    |> List.tryFind (fun (pattern,_) -> (pattern.Match req.Url.AbsolutePath).Success)
    |> Option.map (fun (path,f) -> f req)