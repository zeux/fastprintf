(*
printfn "%A" (parseFormatString "%b %s %d %i %u %x %X %o %e %E %f %F %g %G %M %O %A %a %t %%")
printfn "%A" (parseFormatString "%06d %-4d %+d % d %6.4f %-06.4f %.g")
printfn "%A" (parseFormatString "")
*)

printfn "%s" (FastPrintf.sprintf "foo %d bar %s baz" 1 "hey")
printfn "%s" (FastPrintf.sprintf "[%d,%d%d] %s %d %s%s" 1 2 0 "hey" 5 "a" "b")
