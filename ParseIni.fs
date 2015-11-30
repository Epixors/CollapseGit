
#light

namespace Collapse
open System

module ParseIni = 

    let (|RegexMatch|_|) regex str =
       let m = System.Text.RegularExpressions.Regex(regex).Match(str)
       if m.Success
       then Some (List.tail [ for x in m.Groups -> x.Value ])
       else None

    let loadConfig fn = 
        System.IO.File.ReadAllLines(fn)
        |> Array.fold
            (fun acc l ->
                match l with 
                | RegexMatch "^\[(\S+)\]$" [section] -> (section, []) :: acc            
                | RegexMatch "^(\S+)[\s]*=[\s]*(.*)$" [prop; value] -> (fst acc.Head, (prop, value)::(snd acc.Head))::(acc.Tail)
                | _ -> acc
            )
            List.empty
        |> Map.ofList