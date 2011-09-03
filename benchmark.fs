let timec = 10000

let time1 () =
    let timer = System.Diagnostics.Stopwatch.StartNew()
    let mutable result = 0
    for i in 0..timec do
        result <- result + (sprintf "foo %d %s" i "hey").Length
    result, timer.ElapsedMilliseconds

let time2 () =
    let timer = System.Diagnostics.Stopwatch.StartNew()
    let mutable result = 0
    for i in 0..timec do
        result <- result + (FastPrintf.sprintf "foo %d %s" i "hey").Length
    result, timer.ElapsedMilliseconds

let time3 () =
    let timer = System.Diagnostics.Stopwatch.StartNew()
    let mutable result = 0
    for i in 0..timec do
        result <- result + (FastPrintf.sprintfc "foo %d %s" i "hey").Length
    result, timer.ElapsedMilliseconds

let time4 () =
    let timer = System.Diagnostics.Stopwatch.StartNew()
    let mutable result = 0
    for i in 0..timec do
        result <- result + (System.String.Format("foo {0} {1}", i, "hey")).Length
    result, timer.ElapsedMilliseconds

printfn "sprintf: %A" (time1())
printfn "fast sprintf: %A" (time2())
printfn "fast sprintfc: %A" (time3())
printfn "string.format: %A" (time4())
