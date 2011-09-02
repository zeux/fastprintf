(*
printfn "%A" (parseFormatString "%c %b %s %d %i %u %x %X %o %e %E %f %F %g %G %M %O %A %a %t %%")
printfn "%A" (parseFormatString "%06d %-4d %+d % d %6.4f %-06.4f %.g")
printfn "%A" (parseFormatString "")
*)

printfn "%s" (FastPrintf.sprintf "foo %d bar %s baz" 1 "hey")
printfn "%s" (FastPrintf.sprintf "[%d,%d%d] %s %d %s%s" 1 2 0 "hey" 5 "a" "b")
printfn "%s" (FastPrintf.sprintf "%d %i %x %X %o" 123 456 254 254 123)
printfn "%s" (FastPrintf.sprintf "%f %F %g %G %e %E" -12.4 -12.4 -12.4 -12.4 -12.4 -12.4)
printfn "%s" (FastPrintf.sprintf "%d %u %x %o" -123y -123y -123y -123y)

let curry = (FastPrintf.sprintf "foo %d %s" 4)
printfn "%s" (curry "hey!")
printfn "%s" (curry "yo!")

printfn "%s" (FastPrintf.sprintf "%O %O %O %O" 1 "fo" true ())
printfn "%s" (FastPrintf.sprintf "%b %c" true 'X')

type Point = {x: float; y: float}
printfn "%s" (FastPrintf.sprintf "%A" {x = 1.0; y = 4.0})

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
