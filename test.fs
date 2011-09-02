open FastPrintf

printfn "%A" (parseFormatString "%b %s %d %i %u %x %X %o %e %E %f %F %g %G %M %O %A %a %t %%")
printfn "%A" (parseFormatString "%06d %-4d %+d % d %6.4f %-06.4f %.g")
printfn "%A" (parseFormatString "")
