// FastPrintf - a fast F# printf replacement
// Copyright (C) 2011-2012, by Arseny Kapoulkine (arseny.kapoulkine@gmail.com)
// Report bugs and download new versions at http://hg.zeuxcg.org/fastprintf
// This library is distributed under the MIT License.
module Benchmark

open System
open System.Diagnostics

module CorePrintf = Microsoft.FSharp.Core.Printf

let time1 N =
    let timer = Stopwatch.StartNew()
    let mutable result = 0
    for i in 0..N do
        result <- result + (CorePrintf.sprintf (Printf.StringFormat<int -> string -> string>("foo %d %s " + i.ToString())) i "hey").Length
    result, timer.ElapsedMilliseconds

let time2 N =
    let timer = Stopwatch.StartNew()
    let mutable result = 0
    for i in 0..N do
        result <- result + (FastPrintf.sprintf (Printf.StringFormat<int -> string -> string>("foo %d %s " + i.ToString())) i "hey").Length
    result, timer.ElapsedMilliseconds

let time3 N =
    let timer = Stopwatch.StartNew()
    let mutable result = 0
    for i in 0..N do
        result <- result + (FastPrintf.sprintf "foo %d %s" i "hey").Length
    result, timer.ElapsedMilliseconds

let time4 N =
    let timer = Stopwatch.StartNew()
    let mutable result = 0
    let print = FastPrintf.sprintf "foo %d %s"
    for i in 0..N do
        result <- result + (print i "hey").Length
    result, timer.ElapsedMilliseconds

let time5 N =
    let timer = Stopwatch.StartNew()
    let mutable result = 0
    for i in 0..N do
        result <- result + (String.Format("foo {0} {1}", i, "hey")).Length
    result, timer.ElapsedMilliseconds

let time6 N =
    let timer = Stopwatch.StartNew()
    let mutable result = 0
    for i in 0..N do
        result <- result + String.Concat("foo ", i.ToString(), " ", "hey").Length
    result, timer.ElapsedMilliseconds

let N1 = 10000
let N2 = 1000000

printfn "different pattern strings (no caching):"
printfn "core sprintf:  %A (%d iter)" (time1 N1) N1
printfn "fast sprintf:  %A (%d iter)" (time2 N1) N1

printfn "same pattern string:"
printfn "fast sprintf : %A (%d iter)" (time3 N2) N2
printfn "fast sprintf!: %A (%d iter)" (time4 N2) N2
printfn "string.format: %A (%d iter)" (time5 N2) N2
printfn "manual concat: %A (%d iter)" (time6 N2) N2
