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

printfn "sprintf: %A" (time1 N1)
printfn "fast sprintf: %A" (time2 N1)
printfn "fast sprintfc: %A" (time3 N2)
printfn "string.format: %A" (time4 N2)
