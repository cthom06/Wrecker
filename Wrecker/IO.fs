module Cthom06.Wrecker.IO

open System.IO
open System.Text
open System.Net
open System.Net.Sockets
open Cthom06.Wrecker.Http

type private ReadState = {
    Buffer : byte[]
    Offset : int
    }

let private abind (f : _ -> Async<_ option>) v =
    async {
        let! v = v
        match v with
        | None -> return None
        | Some v -> return! f v
    }
module private Async =
    let value v = async { return v }
    let bind f v = async {
        let! v = v
        return! f v }

let private read (state : ReadState) n maxSize (s : Stream) =
    async {
        if state.Offset >= maxSize then
            return None
        else
            let n = min n (maxSize - state.Offset)
            let newLen = n + state.Offset
            let buff = ref state.Buffer
            if (!buff).Length < newLen then
                System.Array.Resize (buff, min ((!buff).Length * 2) maxSize)
            try
                let! n = s.ReadAsync (!buff, state.Offset, n) |> Async.AwaitTask
                if n = 0 then
                    return None
                else
                    return Some (n, {Buffer = !buff; Offset = state.Offset + n})
            with
            | _ -> return None
    }

let rec private readN (state : ReadState) n maxSize s =
    async {
        if state.Offset < n then
            return!
                read state (n - state.Offset) maxSize s
                |> abind (fun (_, state) -> readN state n maxSize s)
        else
            return Some state
    }
    
let private trimFront (state : ReadState) (n : int) =
    if n < state.Offset then
        let remSize = state.Offset - n
        System.Array.ConstrainedCopy (state.Buffer, n, state.Buffer, 0, remSize)
        { state with Offset = remSize }
    else
        {state with Offset = 0}
    
let private readLine (state : ReadState) maxSize (s : Stream) =
    let rec inner i (state : ReadState) =
        if state.Offset > i then
            let mutable i = i
            let mutable found = false
            while i < state.Offset && not found do
                if state.Buffer.[i] = byte '\n' then
                    found <- i > 0 && state.Buffer.[i - 1] = byte '\r'
                    i <- i + 1
                else
                    i <- i + 1
            if found then
                let line : byte[] = Array.zeroCreate (i - 2)
                System.Array.ConstrainedCopy (state.Buffer, 0, line, 0, i - 2)
                Some (line, trimFront state i) |> Async.value
            else
                inner i state
        else
            read state 1024 maxSize s
            |> abind (inner i << snd)
    inner 0 state


let private parseReqLine baseUri (line : byte[]) =
    // drop \r\n
    // 4 to detect errors but not allocate 10000 arrays for bad input
    let parts = (Encoding.ASCII.GetString (line, 0, line.Length)).Split ([|' '|], 4)
    if parts.Length <> 3 then
        None
    else
        try
            Some (new System.Uri (baseUri, parts.[1]))
        with
        | _ -> None
        |> Option.map (fun url ->
            parts.[0], url, parts.[2])

let private parseHeader (line : byte[]) =
    let parts = (Encoding.ASCII.GetString (line, 0, line.Length)).Split ([|':'|], 2)
    if parts.Length <> 2 then
        None
    else
        Some (canonHeader parts.[0], parts.[1].TrimStart ())

let private readHeaders state maxSize s =
    let rec inner m state maxSize =
        readLine state maxSize s
        |> abind (fun (line, state) ->
            if line.Length = 0 then
                Async.value (Some (m, state, maxSize - 2))
            else
                parseHeader line
                |> Async.value
                |> abind (fun (name, value) ->
                    let m =
                        match Map.tryFind name m with
                        | None -> Map.add name [value] m
                        | Some l -> Map.add name (value::l) m
                    inner m state (maxSize - line.Length)))
    inner Map.empty state maxSize

let private readChunkEncoded state maxSize s =
    let rec inner (mem : MemoryStream) state maxSize =
        readLine state maxSize s
        |> abind (fun (line,state) ->
            try
                let size = int (Encoding.ASCII.GetString line)
                let maxSize = maxSize - line.Length - 2
                if size = 0 then
                    Some (mem.ToArray (), state) |> Async.value
                else
                    readN state (size + 2) maxSize s
                    |> abind (fun state ->
                        mem.AsyncWrite(state.Buffer, 0, size)
                        |> Async.bind (fun () ->
                            inner mem (trimFront state (size + 2)) (maxSize - size - 2)))
            with
            | _ -> None |> Async.value)
    inner (new MemoryStream ()) state maxSize
        
let private readBody headers state maxSize s : Async<(byte[]*ReadState) option> =
        match Map.tryFind "Content-Length" headers with
        | Some (x::_) ->
            try
                let size = int x
                readN state size maxSize s
                |> abind (fun state ->
                    let buff : byte[] = Array.zeroCreate size
                    System.Array.ConstrainedCopy (state.Buffer, 0, buff, 0, size)
                    Some (buff, trimFront state size)
                    |> Async.value)
            with
            | _ -> Async.value None
        | _ ->
            match Map.tryFind "Transfer-Encoding" headers with
            | Some ("chunked"::_) ->
                readChunkEncoded state maxSize s
            | _ -> Some ([||], state) |> Async.value

let private fixUrl (url : System.Uri) (headers : Map<string,string list>) : System.Uri option =
    match Map.tryFind "Host" headers with
    | Some (x::_) ->
        try
            let host = System.Uri x
            Some (new System.Uri (host, url))
        with
        | _ -> None
    | _ -> Some url

let private readRequest state baseUri maxSize s =
    readLine state maxSize s
    |> abind (fun (line, state) ->
        let maxSize = maxSize - line.Length
        parseReqLine baseUri line
        |> Async.value
        |> abind (fun (meth, url, version) ->
            readHeaders state maxSize s
            |> abind (fun (headers, state, maxSize) ->
                readBody headers state maxSize s
                |> abind (fun (buff, state) ->
                    fixUrl url headers
                    |> Option.bind (fun newUrl ->
                        Some (state, { Method = meth
                                       Url = newUrl
                                       Version = version
                                       Header = headers
                                       Body = ByteBody buff }))
                    |> Async.value))))
    
let private writeAll (s : Stream) buff = s.AsyncWrite (buff, 0, buff.Length)
let private writeCR s = writeAll s [| byte '\r'; byte '\n' |]
    
let private stampHeader (resp : Response) =
    resp.Header
    |> Map.add "Date" [System.DateTime.UtcNow.ToString "R"]
    |> Map.add "Server" ["Wrecker"]
    |> fun m ->
        match (Map.tryFind "Content-Length" m), resp.Body with
        | Some _, _ -> false, m
        | None, EmptyBody -> false, m
        | None, _ -> true, Map.add "Transfer-Encoding" ["chunked"] m

let private writeChunk s buff offset count =
    sprintf "%X\r\n" count
    |> Encoding.ASCII.GetBytes
    |> writeAll s
    |> Async.bind (fun () -> s.AsyncWrite (buff, offset, count))
    |> Async.bind (fun () -> writeCR s)

let private writeByteChunk s buff =
    writeCR s
    |> Async.bind (fun () -> writeChunk s buff 0 buff.Length)
    |> Async.bind (fun () -> writeChunk s [||] 0 0)

let private writeStreamChunk (s : Stream) (src : Stream) =
    let buff : byte[] = Array.zeroCreate 4096
    let rec inner () =
        async {
            let! n = src.AsyncRead (buff, 0, 4096)
            return!
                writeChunk s buff 0 n
                |> Async.bind (fun () ->
                    if n = 0 then
                        Async.value ()
                    else
                        inner ())
        }
    writeCR s |> Async.bind inner

let private writeBody s body shouldChunk =
    match shouldChunk, body with
    | true, ByteBody buff -> writeByteChunk s buff
    | true, StreamBody body -> writeStreamChunk s body
    | false, ByteBody buff ->
        writeCR s
        |> Async.bind (fun () -> writeAll s buff)
        |> Async.bind (fun () -> writeCR s)
    | false, StreamBody body ->
        writeCR s
        |> Async.bind (fun () -> body.CopyToAsync s |> Async.AwaitTask)
        |> Async.bind (fun () -> writeCR s)
    | _, EmptyBody -> writeCR s

let private writeHead s = 
    Map.fold (fun sofar name values ->
        List.fold (fun sofar value ->
            sofar
            |> Async.bind (fun () ->
                sprintf "%s: %s\r\n" name value
                |> Encoding.ASCII.GetBytes
                |> writeAll s)) sofar values) (Async.value ())

let private writeResponse s (resp : Response) =
    sprintf "%s %d %s\r\n" resp.Version resp.Code resp.Reason
    |> Encoding.ASCII.GetBytes
    |> writeAll s
    |> Async.bind (fun () ->
        let shouldChunk, newHead = stampHeader resp
        writeHead s newHead
        |> Async.bind (fun () -> Async.value shouldChunk))
    |> Async.bind (writeBody s resp.Body)

type Server = {
    Host : string
    Port : int
    Router : Request -> Response
    DefaultUrl : System.Uri
    MaxRequest : int
    }

let defaultServer =
    let hostName = Dns.GetHostName ()
    { Host = hostName
      Port = 80
      Router = fun _ ->
        { Version="HTTP/1.1"
          Code = 404
          Reason = "Not Found"
          Header = Map.empty
          Body = Body.fromString "Not Found" }
      DefaultUrl = new System.Uri("http://" + hostName)
      MaxRequest = 64 * 1024 }

let runConn (server : Server) (conn : Socket) =
    let st = new NetworkStream (conn)
    let state = { Offset = 0; Buffer = Array.zeroCreate 4096 }
    let rec inner state = 
        async {
            let! req = readRequest state server.DefaultUrl server.MaxRequest st
            match req with
            | None ->
                printfn "None"
                conn.Close ()
            | Some (state, req) ->
                printfn "%A" req
                let resp = server.Router req
                printfn "%A" resp
                do! writeResponse st resp
                do! st.FlushAsync () |> Async.AwaitTask
                return! inner state
        }
    inner state

let runServer (server : Server) =
    let socks = 
        server.Host
        |> Dns.GetHostAddresses
        |> Array.map (fun addr ->
            let sock = new Socket(SocketType.Stream, ProtocolType.Tcp)
            sock.Bind (new IPEndPoint (addr, server.Port))
            sock.Listen (10) // idk
            sock)
    while true do
        let sockList = new System.Collections.Generic.List<Socket> (socks)
        Socket.Select (sockList, null, null, -1)
        sockList
        |> Seq.iter (fun t -> t.Accept () |> runConn server |> Async.Start)
