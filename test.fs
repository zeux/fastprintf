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
