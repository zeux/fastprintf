module UnitTests

open System
open System.Globalization

let testCounter = ref 0

let (=!) x y =
    if x <> y then failwithf "Test failed: %A <> %A" x y
    else incr testCounter

let t0 l r = l =! r
let t1 l r a0 = l a0 =! r a0
let t2 l r a0 a1 = l a0 a1 =! r a0 a1
let t3 l r a0 a1 a2 = l a0 a1 a2 =! r a0 a1 a2
let t4 l r a0 a1 a2 a3 = l a0 a1 a2 a3 =! r a0 a1 a2 a3
let t5 l r a0 a1 a2 a3 a4 = l a0 a1 a2 a3 a4 =! r a0 a1 a2 a3 a4
let t6 l r a0 a1 a2 a3 a4 a5 = l a0 a1 a2 a3 a4 a5 =! r a0 a1 a2 a3 a4 a5
let t7 l r a0 a1 a2 a3 a4 a5 a6 = l a0 a1 a2 a3 a4 a5 a6 =! r a0 a1 a2 a3 a4 a5 a6
let t8 l r a0 a1 a2 a3 a4 a5 a6 a7 = l a0 a1 a2 a3 a4 a5 a6 a7 =! r a0 a1 a2 a3 a4 a5 a6 a7
let t9 l r a0 a1 a2 a3 a4 a5 a6 a7 a8 = l a0 a1 a2 a3 a4 a5 a6 a7 a8 =! r a0 a1 a2 a3 a4 a5 a6 a7 a8

let tf0 (fmt: PrintfFormat<_, _, _, _>) = t0 (sprintf fmt) (FastPrintf.sprintf fmt)
let tf1 (fmt: PrintfFormat<_, _, _, _>) = t1 (sprintf fmt) (FastPrintf.sprintf fmt)
let tf2 (fmt: PrintfFormat<_, _, _, _>) = t2 (sprintf fmt) (FastPrintf.sprintf fmt)
let tf3 (fmt: PrintfFormat<_, _, _, _>) = t3 (sprintf fmt) (FastPrintf.sprintf fmt)
let tf4 (fmt: PrintfFormat<_, _, _, _>) = t4 (sprintf fmt) (FastPrintf.sprintf fmt)
let tf5 (fmt: PrintfFormat<_, _, _, _>) = t5 (sprintf fmt) (FastPrintf.sprintf fmt)
let tf6 (fmt: PrintfFormat<_, _, _, _>) = t6 (sprintf fmt) (FastPrintf.sprintf fmt)
let tf7 (fmt: PrintfFormat<_, _, _, _>) = t7 (sprintf fmt) (FastPrintf.sprintf fmt)
let tf8 (fmt: PrintfFormat<_, _, _, _>) = t8 (sprintf fmt) (FastPrintf.sprintf fmt)
let tf9 (fmt: PrintfFormat<_, _, _, _>) = t9 (sprintf fmt) (FastPrintf.sprintf fmt)

let tf6l (fmt: PrintfFormat<_, _, _, _>) l = for a in l do t6 (sprintf fmt) (FastPrintf.sprintf fmt) a a a a a a

// literal strings
let testLiteral () =
    tf0 ""
    tf0 "foo"
    tf0 "a b c d           e"
    tf0 "\u1238"

// literal % (with some dummy flags as well)
let testLiteralPercent () =
    tf0 "%%"
    tf0 "foo%%"
    tf0 "%%foo"
    tf0 "%%foo%%bar%%"
    tf0 "% -034%"
    tf1 "%%%d%%" 1
    tf2 "%%%d%%%d%%" 1 2

// integers: no flags, all types
let testIntegerBasic () =
    tf6l "%d %i %o %u %x %X" [0y; 42y; 127y; -128y]
    tf6l "%d %i %o %u %x %X" [0uy; 42uy; 127uy; 255uy]
    tf6l "%d %i %o %u %x %X" [0s; 42s; 32767s; -32768s]
    tf6l "%d %i %o %u %x %X" [0us; 42us; 32767us; 65535us]
    tf6l "%d %i %o %u %x %X" [0; 42; 2147483647; -2147483648]
    tf6l "%d %i %o %u %x %X" [0u; 42u; 2147483647u; 4294967295u]
    tf6l "%d %i %o %u %x %X" [0L; 42L; 9223372036854775807L; -9223372036854775808L]
    tf6l "%d %i %o %u %x %X" [0UL; 42UL; 9223372036854775807UL; 18446744073709551615UL]
    tf6l "%d %i %o %u %x %X" [0n; 42n; 2147483647n; -2147483648n]
    tf6l "%d %i %o %u %x %X" [0un; 42un; 2147483647un; 4294967295un]

// floats: no flags, all types
let testFloatBasic () =
    tf6l "%f %F %e %E %g %G" [0.f; 0.1f; 10.001f; -34.73f; 1000000000.343f; infinityf; -infinityf; nanf]
    tf6l "%f %F %e %E %g %G" [0.0; 0.1; 10.001; -34.73; 1000000000.343; infinity; -infinity; nan]
    tf6l "%f %F %e %E %g %G" [0.0m; 0.1m; 10.001m; -34.73m; 1000000000.343m]

// fixed types (i.e. no variation), no flags
let testFixedBasic () =
    tf5 "%M %M %M %M %M" 0.0m 0.1m 10.001m -34.73m 1000000000.343m
    tf2 "%b %b" true false
    tf2 "%c %c" ' ' 'x'
    tf3 "%s %s %s" "" "booo" null

// make a mess out of the current culture to make sure the culture-related behavior is the same as that of core printf
let numberFormat = NumberFormatInfo()
numberFormat.NaNSymbol <- "no way"
numberFormat.NegativeInfinitySymbol <- "very small"
numberFormat.NegativeSign <- "minus"
numberFormat.NumberDecimalDigits <- 1
numberFormat.NumberDecimalSeparator <- "`"
numberFormat.NumberGroupSeparator <- ","
numberFormat.NumberGroupSizes <- [|4; 1|]
numberFormat.NumberNegativePattern <- 3
numberFormat.PositiveInfinitySymbol <- "very large"
numberFormat.PositiveSign <- "plus"

System.Threading.Thread.CurrentThread.CurrentCulture <- CultureInfo("", NumberFormat = numberFormat)

// run tests
testLiteral ()
// testLiteralPercent ()
testIntegerBasic ()
testFloatBasic ()
testFixedBasic ()

printfn "%d tests passed" !testCounter
