let time1 N =
    let timer = System.Diagnostics.Stopwatch.StartNew()
    let mutable result = 0
    for i in 0..N do
        result <- result + (sprintf "foo %d %s" i "hey").Length
    result, timer.ElapsedMilliseconds

let time2 N =
    let timer = System.Diagnostics.Stopwatch.StartNew()
    let mutable result = 0
    for i in 0..N do
        result <- result + (FastPrintf.sprintf "foo %d %s" i "hey").Length
    result, timer.ElapsedMilliseconds

let time3 N =
    let timer = System.Diagnostics.Stopwatch.StartNew()
    let mutable result = 0
    for i in 0..N do
        result <- result + (FastPrintf.sprintfc "foo %d %s" i "hey").Length
    result, timer.ElapsedMilliseconds

let time4 N =
    let timer = System.Diagnostics.Stopwatch.StartNew()
    let mutable result = 0
    for i in 0..N do
        result <- result + (System.String.Format("foo {0} {1}", i, "hey")).Length
    result, timer.ElapsedMilliseconds

let N1 = 10000
let N2 = 1000000

printfn "core sprintf:  %A (%d iter)" (time1 N1) N1
printfn "fast sprintf:  %A (%d iter)" (time2 N1) N1
printfn "fast sprintfc: %A (%d iter)" (time3 N2) N2
printfn "string.format: %A (%d iter)" (time4 N2) N2
