open System
open System.IO
open System.Collections.Generic

let star1 (input: int seq)=
    Seq.sum input

[
    [+1; +1; +1]
    [+1; +1; -2]
    [-1; -2; -3]
]
|> List.map star1
|> printfn "%A"

File.ReadAllLines("day1.input")
|> Seq.map Int32.Parse
|> star1
|> printfn "%d"
// 406

module Seq =
    let repeatForever (source: 'a array) =
        Seq.initInfinite (fun i -> source.[i%source.Length])

let star2' input =
    let rec loop (input: int array) idx lastFreq (freqs: HashSet<int>)  =
        let nextFreq = lastFreq + input.[idx]
        if freqs.Add(nextFreq) then
            if freqs.Count % 1000 = 0 then
                printfn "%d" freqs.Count
            loop input ((idx + 1) % input.Length) nextFreq freqs
        else
            nextFreq

    let set = HashSet<int>()
    set.Add(0) |> ignore
    loop input 0 0 set

let star2 (input: int array) =
    let rec loop (freqs: HashSet<int>) idx lastFreq =
        let nextFreq = lastFreq + input.[idx]
        if freqs.Add(nextFreq) then
            loop freqs ((idx + 1) % input.Length) nextFreq
        else
            nextFreq
    loop (HashSet<int>([0])) 0 0

[
    [|1; -1|]
    [|3; 3; 4; -2; -4|]
    [|-6; +3; +8; +5; -6|]
    [|+7; +7; -2; -7; -4|]
]
|> List.map star2
|> printfn "%A"

File.ReadAllLines("day1.input")
|> Array.map Int32.Parse
|> star2
|> printfn "%d"
// 312