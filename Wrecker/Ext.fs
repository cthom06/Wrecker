namespace Cthom06.Wrecker

module private Option =
    let orDefault v o =
        match o with
        | None -> v
        | Some x -> x