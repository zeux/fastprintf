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

// signed integers: all flags & combinations (0-+ ) with padding
let testIntegerFlagsWithPaddingSigned () =
    tf6l "%020d %020i %020o %020u %020x %020X" [0; 200; -200; 2147483647; -2147483648]
    tf6l "%0-20d %0-20i %0-20o %0-20u %0-20x %0-20X" [0; 200; -200; 2147483647; -2147483648]
    tf6l "%0-+20d %0-+20i %0-+20o %0-+20u %0-+20x %0-+20X" [0; 200; -200; 2147483647; -2147483648]
    tf6l "%0- 20d %0- 20i %0- 20o %0- 20u %0- 20x %0- 20X" [0; 200; -200; 2147483647; -2147483648]
    tf6l "%0+20d %0+20i %0+20o %0+20u %0+20x %0+20X" [0; 200; -200; 2147483647; -2147483648]
    tf6l "%0 20d %0 20i %0 20o %0 20u %0 20x %0 20X" [0; 200; -200; 2147483647; -2147483648]
    tf6l "%20d %20i %20o %20u %20x %20X" [0; 200; -200; 2147483647; -2147483648]
    tf6l "%-20d %-20i %-20o %-20u %-20x %-20X" [0; 200; -200; 2147483647; -2147483648]
    tf6l "%-+20d %-+20i %-+20o %-+20u %-+20x %-+20X" [0; 200; -200; 2147483647; -2147483648]
    tf6l "%- 20d %- 20i %- 20o %- 20u %- 20x %- 20X" [0; 200; -200; 2147483647; -2147483648]
    tf6l "%+20d %+20i %+20o %+20u %+20x %+20X" [0; 200; -200; 2147483647; -2147483648]
    tf6l "% 20d % 20i % 20o % 20u % 20x % 20X" [0; 200; -200; 2147483647; -2147483648]

// unsigned integers: all flags & combinations (0-+ ) with padding
let testIntegerFlagsWithPaddingUnsigned () =
    tf6l "%020d %020i %020o %020u %020x %020X" [0u; 200u; 2147483647u; 4294967295u]
    tf6l "%0-20d %0-20i %0-20o %0-20u %0-20x %0-20X" [0u; 200u; 2147483647u; 4294967295u]
    tf6l "%0-+20d %0-+20i %0-+20o %0-+20u %0-+20x %0-+20X" [0u; 200u; 2147483647u; 4294967295u]
    tf6l "%0- 20d %0- 20i %0- 20o %0- 20u %0- 20x %0- 20X" [0u; 200u; 2147483647u; 4294967295u]
    tf6l "%0+20d %0+20i %0+20o %0+20u %0+20x %0+20X" [0u; 200u; 2147483647u; 4294967295u]
    tf6l "%0 20d %0 20i %0 20o %0 20u %0 20x %0 20X" [0u; 200u; 2147483647u; 4294967295u]
    tf6l "%20d %20i %20o %20u %20x %20X" [0u; 200u; 2147483647u; 4294967295u]
    tf6l "%-20d %-20i %-20o %-20u %-20x %-20X" [0u; 200u; 2147483647u; 4294967295u]
    tf6l "%-+20d %-+20i %-+20o %-+20u %-+20x %-+20X" [0u; 200u; 2147483647u; 4294967295u]
    tf6l "%- 20d %- 20i %- 20o %- 20u %- 20x %- 20X" [0u; 200u; 2147483647u; 4294967295u]
    tf6l "%+20d %+20i %+20o %+20u %+20x %+20X" [0u; 200u; 2147483647u; 4294967295u]
    tf6l "% 20d % 20i % 20o % 20u % 20x % 20X" [0u; 200u; 2147483647u; 4294967295u]

// signed integers: all flags & combinations (0-+ ) without padding
let testIntegerFlagsWithoutPaddingSigned () =
    tf6l "%0d %0i %0o %0u %0x %0X" [0; 200; -200; 2147483647; -2147483648]
    tf6l "%0-d %0-i %0-o %0-u %0-x %0-X" [0; 200; -200; 2147483647; -2147483648]
    tf6l "%0-+d %0-+i %0-+o %0-+u %0-+x %0-+X" [0; 200; -200; 2147483647; -2147483648]
    tf6l "%0- d %0- i %0- o %0- u %0- x %0- X" [0; 200; -200; 2147483647; -2147483648]
    tf6l "%0+d %0+i %0+o %0+u %0+x %0+X" [0; 200; -200; 2147483647; -2147483648]
    tf6l "%0 d %0 i %0 o %0 u %0 x %0 X" [0; 200; -200; 2147483647; -2147483648]
    tf6l "%d %i %o %u %x %X" [0; 200; -200; 2147483647; -2147483648]
    tf6l "%-d %-i %-o %-u %-x %-X" [0; 200; -200; 2147483647; -2147483648]
    tf6l "%-+d %-+i %-+o %-+u %-+x %-+X" [0; 200; -200; 2147483647; -2147483648]
    tf6l "%- d %- i %- o %- u %- x %- X" [0; 200; -200; 2147483647; -2147483648]
    tf6l "%+d %+i %+o %+u %+x %+X" [0; 200; -200; 2147483647; -2147483648]
    tf6l "% d % i % o % u % x % X" [0; 200; -200; 2147483647; -2147483648]

// unsigned integers: all flags & combinations (0-+ ) without padding
let testIntegerFlagsWithoutPaddingUnsigned () =
    tf6l "%0d %0i %0o %0u %0x %0X" [0u; 200u; 2147483647u; 4294967295u]
    tf6l "%0-d %0-i %0-o %0-u %0-x %0-X" [0u; 200u; 2147483647u; 4294967295u]
    tf6l "%0-+d %0-+i %0-+o %0-+u %0-+x %0-+X" [0u; 200u; 2147483647u; 4294967295u]
    tf6l "%0- d %0- i %0- o %0- u %0- x %0- X" [0u; 200u; 2147483647u; 4294967295u]
    tf6l "%0+d %0+i %0+o %0+u %0+x %0+X" [0u; 200u; 2147483647u; 4294967295u]
    tf6l "%0 d %0 i %0 o %0 u %0 x %0 X" [0u; 200u; 2147483647u; 4294967295u]
    tf6l "%d %i %o %u %x %X" [0u; 200u; 2147483647u; 4294967295u]
    tf6l "%-d %-i %-o %-u %-x %-X" [0u; 200u; 2147483647u; 4294967295u]
    tf6l "%-+d %-+i %-+o %-+u %-+x %-+X" [0u; 200u; 2147483647u; 4294967295u]
    tf6l "%- d %- i %- o %- u %- x %- X" [0u; 200u; 2147483647u; 4294967295u]
    tf6l "%+d %+i %+o %+u %+x %+X" [0u; 200u; 2147483647u; 4294967295u]
    tf6l "% d % i % o % u % x % X" [0u; 200u; 2147483647u; 4294967295u]

// integers: all flags, all types
let testIntegerComplex () =
    tf6l "%10d %-9i %10o %-9u %10x %-9X" [0y; 42y; 127y; -128y]
    tf6l "%010d %+7i %010o %+7u %010x %+7X" [0uy; 42uy; 127uy; 255uy]
    tf6l "%0-8d %0+9i %0-8o %0+9u %0-8x %0+9X" [0s; 42s; 32767s; -32768s]
    tf6l "%-+7d %+-7i %-+7o %+-7u %-+7x %+-7X" [0us; 42us; 32767us; 65535us]
    tf6l "%0 d % 0i %0 o % 0u %0 x % 0X" [0; 42; 2147483647; -2147483648]
    tf6l "%- d % -i %- o % -u %- x % -X" [0u; 42u; 2147483647u; 4294967295u]
    tf6l "%123d %0i %123o %0u %123x %0X" [0L; 42L; 9223372036854775807L; -9223372036854775808L]
    tf6l "%-d %0i %-o %0u %-x %0X" [0UL; 42UL; 9223372036854775807UL; 18446744073709551615UL]
    tf6l "%+d % i %+o % u %+x % X" [0n; 42n; 2147483647n; -2147483648n]
    tf6l "%-10d %20i %-10o %20u %-10x %20X" [0un; 42un; 2147483647un; 4294967295un]

// floats: no flags, all types
let testFloatBasic () =
    tf6l "%f %F %e %E %g %G" [0.f; 0.1f; 10.001f; -34.73f; 1000000000.343f; infinityf; -infinityf; nanf]
    tf6l "%f %F %e %E %g %G" [0.0; 0.1; 10.001; -34.73; 1000000000.343; infinity; -infinity; nan]
    tf6l "%f %F %e %E %g %G" [0.0m; 0.1m; 10.001m; -34.73m; 1000000000.343m]

// single floats: all flags & combinations (0-+ ) with padding
let testFloatFlagsWithPaddingSingle () =
    tf6l "%020.4f %020.4F %020.4e %020.4E %020.4g %020.4G" [0.0f; 0.1f; 10.001f; -34.73f; 1000000000.343f; infinityf; -infinityf; nanf]
    tf6l "%0-20.4f %0-20.4F %0-20.4e %0-20.4E %0-20.4g %0-20.4G" [0.0f; 0.1f; 10.001f; -34.73f; 1000000000.343f; infinityf; -infinityf; nanf]
    tf6l "%0-+20.4f %0-+20.4F %0-+20.4e %0-+20.4E %0-+20.4g %0-+20.4G" [0.0f; 0.1f; 10.001f; -34.73f; 1000000000.343f; infinityf; -infinityf; nanf]
    tf6l "%0- 20.4f %0- 20.4F %0- 20.4e %0- 20.4E %0- 20.4g %0- 20.4G" [0.0f; 0.1f; 10.001f; -34.73f; 1000000000.343f; infinityf; -infinityf; nanf]
    tf6l "%0+20.4f %0+20.4F %0+20.4e %0+20.4E %0+20.4g %0+20.4G" [0.0f; 0.1f; 10.001f; -34.73f; 1000000000.343f; infinityf; -infinityf; nanf]
    tf6l "%0 20.4f %0 20.4F %0 20.4e %0 20.4E %0 20.4g %0 20.4G" [0.0f; 0.1f; 10.001f; -34.73f; 1000000000.343f; infinityf; -infinityf; nanf]
    tf6l "%20.4f %20.4F %20.4e %20.4E %20.4g %20.4G" [0.0f; 0.1f; 10.001f; -34.73f; 1000000000.343f; infinityf; -infinityf; nanf]
    tf6l "%-20.4f %-20.4F %-20.4e %-20.4E %-20.4g %-20.4G" [0.0f; 0.1f; 10.001f; -34.73f; 1000000000.343f; infinityf; -infinityf; nanf]
    tf6l "%-+20.4f %-+20.4F %-+20.4e %-+20.4E %-+20.4g %-+20.4G" [0.0f; 0.1f; 10.001f; -34.73f; 1000000000.343f; infinityf; -infinityf; nanf]
    tf6l "%- 20.4f %- 20.4F %- 20.4e %- 20.4E %- 20.4g %- 20.4G" [0.0f; 0.1f; 10.001f; -34.73f; 1000000000.343f; infinityf; -infinityf; nanf]
    tf6l "%+20.4f %+20.4F %+20.4e %+20.4E %+20.4g %+20.4G" [0.0f; 0.1f; 10.001f; -34.73f; 1000000000.343f; infinityf; -infinityf; nanf]
    tf6l "% 20.4f % 20.4F % 20.4e % 20.4E % 20.4g % 20.4G" [0.0f; 0.1f; 10.001f; -34.73f; 1000000000.343f; infinityf; -infinityf; nanf]

// single floats: all flags & combinations (0-+ ) without padding
let testFloatFlagsWithoutPaddingSingle () =
    tf6l "%0.4f %0.4F %0.4e %0.4E %0.4g %0.4G" [0.0f; 0.1f; 10.001f; -34.73f; 1000000000.343f; infinityf; -infinityf; nanf]
    tf6l "%0-.4f %0-.4F %0-.4e %0-.4E %0-.4g %0-.4G" [0.0f; 0.1f; 10.001f; -34.73f; 1000000000.343f; infinityf; -infinityf; nanf]
    tf6l "%0-+.4f %0-+.4F %0-+.4e %0-+.4E %0-+.4g %0-+.4G" [0.0f; 0.1f; 10.001f; -34.73f; 1000000000.343f; infinityf; -infinityf; nanf]
    tf6l "%0- .4f %0- .4F %0- .4e %0- .4E %0- .4g %0- .4G" [0.0f; 0.1f; 10.001f; -34.73f; 1000000000.343f; infinityf; -infinityf; nanf]
    tf6l "%0+.4f %0+.4F %0+.4e %0+.4E %0+.4g %0+.4G" [0.0f; 0.1f; 10.001f; -34.73f; 1000000000.343f; infinityf; -infinityf; nanf]
    tf6l "%0 .4f %0 .4F %0 .4e %0 .4E %0 .4g %0 .4G" [0.0f; 0.1f; 10.001f; -34.73f; 1000000000.343f; infinityf; -infinityf; nanf]
    tf6l "%.4f %.4F %.4e %.4E %.4g %.4G" [0.0f; 0.1f; 10.001f; -34.73f; 1000000000.343f; infinityf; -infinityf; nanf]
    tf6l "%-.4f %-.4F %-.4e %-.4E %-.4g %-.4G" [0.0f; 0.1f; 10.001f; -34.73f; 1000000000.343f; infinityf; -infinityf; nanf]
    tf6l "%-+.4f %-+.4F %-+.4e %-+.4E %-+.4g %-+.4G" [0.0f; 0.1f; 10.001f; -34.73f; 1000000000.343f; infinityf; -infinityf; nanf]
    tf6l "%- .4f %- .4F %- .4e %- .4E %- .4g %- .4G" [0.0f; 0.1f; 10.001f; -34.73f; 1000000000.343f; infinityf; -infinityf; nanf]
    tf6l "%+.4f %+.4F %+.4e %+.4E %+.4g %+.4G" [0.0f; 0.1f; 10.001f; -34.73f; 1000000000.343f; infinityf; -infinityf; nanf]
    tf6l "% .4f % .4F % .4e % .4E % .4g % .4G" [0.0f; 0.1f; 10.001f; -34.73f; 1000000000.343f; infinityf; -infinityf; nanf]

// double floats: all flags & combinations (0-+ ) with padding
let testFloatFlagsWithPaddingDouble () =
    tf6l "%020.4f %020.4F %020.4e %020.4E %020.4g %020.4G" [0.0; 0.1; 10.001; -34.73; 1000000000.343; infinity; -infinity; nan]
    tf6l "%0-20.4f %0-20.4F %0-20.4e %0-20.4E %0-20.4g %0-20.4G" [0.0; 0.1; 10.001; -34.73; 1000000000.343; infinity; -infinity; nan]
    tf6l "%0-+20.4f %0-+20.4F %0-+20.4e %0-+20.4E %0-+20.4g %0-+20.4G" [0.0; 0.1; 10.001; -34.73; 1000000000.343; infinity; -infinity; nan]
    tf6l "%0- 20.4f %0- 20.4F %0- 20.4e %0- 20.4E %0- 20.4g %0- 20.4G" [0.0; 0.1; 10.001; -34.73; 1000000000.343; infinity; -infinity; nan]
    tf6l "%0+20.4f %0+20.4F %0+20.4e %0+20.4E %0+20.4g %0+20.4G" [0.0; 0.1; 10.001; -34.73; 1000000000.343; infinity; -infinity; nan]
    tf6l "%0 20.4f %0 20.4F %0 20.4e %0 20.4E %0 20.4g %0 20.4G" [0.0; 0.1; 10.001; -34.73; 1000000000.343; infinity; -infinity; nan]
    tf6l "%20.4f %20.4F %20.4e %20.4E %20.4g %20.4G" [0.0; 0.1; 10.001; -34.73; 1000000000.343; infinity; -infinity; nan]
    tf6l "%-20.4f %-20.4F %-20.4e %-20.4E %-20.4g %-20.4G" [0.0; 0.1; 10.001; -34.73; 1000000000.343; infinity; -infinity; nan]
    tf6l "%-+20.4f %-+20.4F %-+20.4e %-+20.4E %-+20.4g %-+20.4G" [0.0; 0.1; 10.001; -34.73; 1000000000.343; infinity; -infinity; nan]
    tf6l "%- 20.4f %- 20.4F %- 20.4e %- 20.4E %- 20.4g %- 20.4G" [0.0; 0.1; 10.001; -34.73; 1000000000.343; infinity; -infinity; nan]
    tf6l "%+20.4f %+20.4F %+20.4e %+20.4E %+20.4g %+20.4G" [0.0; 0.1; 10.001; -34.73; 1000000000.343; infinity; -infinity; nan]
    tf6l "% 20.4f % 20.4F % 20.4e % 20.4E % 20.4g % 20.4G" [0.0; 0.1; 10.001; -34.73; 1000000000.343; infinity; -infinity; nan]

// double floats: all flags & combinations (0-+ ) without padding
let testFloatFlagsWithoutPaddingDouble () =
    tf6l "%0.4f %0.4F %0.4e %0.4E %0.4g %0.4G" [0.0; 0.1; 10.001; -34.73; 1000000000.343; infinity; -infinity; nan]
    tf6l "%0-.4f %0-.4F %0-.4e %0-.4E %0-.4g %0-.4G" [0.0; 0.1; 10.001; -34.73; 1000000000.343; infinity; -infinity; nan]
    tf6l "%0-+.4f %0-+.4F %0-+.4e %0-+.4E %0-+.4g %0-+.4G" [0.0; 0.1; 10.001; -34.73; 1000000000.343; infinity; -infinity; nan]
    tf6l "%0- .4f %0- .4F %0- .4e %0- .4E %0- .4g %0- .4G" [0.0; 0.1; 10.001; -34.73; 1000000000.343; infinity; -infinity; nan]
    tf6l "%0+.4f %0+.4F %0+.4e %0+.4E %0+.4g %0+.4G" [0.0; 0.1; 10.001; -34.73; 1000000000.343; infinity; -infinity; nan]
    tf6l "%0 .4f %0 .4F %0 .4e %0 .4E %0 .4g %0 .4G" [0.0; 0.1; 10.001; -34.73; 1000000000.343; infinity; -infinity; nan]
    tf6l "%.4f %.4F %.4e %.4E %.4g %.4G" [0.0; 0.1; 10.001; -34.73; 1000000000.343; infinity; -infinity; nan]
    tf6l "%-.4f %-.4F %-.4e %-.4E %-.4g %-.4G" [0.0; 0.1; 10.001; -34.73; 1000000000.343; infinity; -infinity; nan]
    tf6l "%-+.4f %-+.4F %-+.4e %-+.4E %-+.4g %-+.4G" [0.0; 0.1; 10.001; -34.73; 1000000000.343; infinity; -infinity; nan]
    tf6l "%- .4f %- .4F %- .4e %- .4E %- .4g %- .4G" [0.0; 0.1; 10.001; -34.73; 1000000000.343; infinity; -infinity; nan]
    tf6l "%+.4f %+.4F %+.4e %+.4E %+.4g %+.4G" [0.0; 0.1; 10.001; -34.73; 1000000000.343; infinity; -infinity; nan]
    tf6l "% .4f % .4F % .4e % .4E % .4g % .4G" [0.0; 0.1; 10.001; -34.73; 1000000000.343; infinity; -infinity; nan]

// decimal floats: all flags & combinations (0-+ ) with padding
let testFloatFlagsWithPaddingDecimal () =
    tf6l "%020.4f %020.4F %020.4e %020.4E %020.4g %020.4G" [0.0m; 0.1m; 10.001m; -34.73m; 1000000000.343m]
    tf6l "%0-20.4f %0-20.4F %0-20.4e %0-20.4E %0-20.4g %0-20.4G" [0.0m; 0.1m; 10.001m; -34.73m; 1000000000.343m]
    tf6l "%0-+20.4f %0-+20.4F %0-+20.4e %0-+20.4E %0-+20.4g %0-+20.4G" [0.0m; 0.1m; 10.001m; -34.73m; 1000000000.343m]
    tf6l "%0- 20.4f %0- 20.4F %0- 20.4e %0- 20.4E %0- 20.4g %0- 20.4G" [0.0m; 0.1m; 10.001m; -34.73m; 1000000000.343m]
    tf6l "%0+20.4f %0+20.4F %0+20.4e %0+20.4E %0+20.4g %0+20.4G" [0.0m; 0.1m; 10.001m; -34.73m; 1000000000.343m]
    tf6l "%0 20.4f %0 20.4F %0 20.4e %0 20.4E %0 20.4g %0 20.4G" [0.0m; 0.1m; 10.001m; -34.73m; 1000000000.343m]
    tf6l "%20.4f %20.4F %20.4e %20.4E %20.4g %20.4G" [0.0m; 0.1m; 10.001m; -34.73m; 1000000000.343m]
    tf6l "%-20.4f %-20.4F %-20.4e %-20.4E %-20.4g %-20.4G" [0.0m; 0.1m; 10.001m; -34.73m; 1000000000.343m]
    tf6l "%-+20.4f %-+20.4F %-+20.4e %-+20.4E %-+20.4g %-+20.4G" [0.0m; 0.1m; 10.001m; -34.73m; 1000000000.343m]
    tf6l "%- 20.4f %- 20.4F %- 20.4e %- 20.4E %- 20.4g %- 20.4G" [0.0m; 0.1m; 10.001m; -34.73m; 1000000000.343m]
    tf6l "%+20.4f %+20.4F %+20.4e %+20.4E %+20.4g %+20.4G" [0.0m; 0.1m; 10.001m; -34.73m; 1000000000.343m]
    tf6l "% 20.4f % 20.4F % 20.4e % 20.4E % 20.4g % 20.4G" [0.0m; 0.1m; 10.001m; -34.73m; 1000000000.343m]

// decimal floats: all flags & combinations (0-+ ) without padding
let testFloatFlagsWithoutPaddingDecimal () =
    tf6l "%0.4f %0.4F %0.4e %0.4E %0.4g %0.4G" [0.0m; 0.1m; 10.001m; -34.73m; 1000000000.343m]
    tf6l "%0-.4f %0-.4F %0-.4e %0-.4E %0-.4g %0-.4G" [0.0m; 0.1m; 10.001m; -34.73m; 1000000000.343m]
    tf6l "%0-+.4f %0-+.4F %0-+.4e %0-+.4E %0-+.4g %0-+.4G" [0.0m; 0.1m; 10.001m; -34.73m; 1000000000.343m]
    tf6l "%0- .4f %0- .4F %0- .4e %0- .4E %0- .4g %0- .4G" [0.0m; 0.1m; 10.001m; -34.73m; 1000000000.343m]
    tf6l "%0+.4f %0+.4F %0+.4e %0+.4E %0+.4g %0+.4G" [0.0m; 0.1m; 10.001m; -34.73m; 1000000000.343m]
    tf6l "%0 .4f %0 .4F %0 .4e %0 .4E %0 .4g %0 .4G" [0.0m; 0.1m; 10.001m; -34.73m; 1000000000.343m]
    tf6l "%.4f %.4F %.4e %.4E %.4g %.4G" [0.0m; 0.1m; 10.001m; -34.73m; 1000000000.343m]
    tf6l "%-.4f %-.4F %-.4e %-.4E %-.4g %-.4G" [0.0m; 0.1m; 10.001m; -34.73m; 1000000000.343m]
    tf6l "%-+.4f %-+.4F %-+.4e %-+.4E %-+.4g %-+.4G" [0.0m; 0.1m; 10.001m; -34.73m; 1000000000.343m]
    tf6l "%- .4f %- .4F %- .4e %- .4E %- .4g %- .4G" [0.0m; 0.1m; 10.001m; -34.73m; 1000000000.343m]
    tf6l "%+.4f %+.4F %+.4e %+.4E %+.4g %+.4G" [0.0m; 0.1m; 10.001m; -34.73m; 1000000000.343m]
    tf6l "% .4f % .4F % .4e % .4E % .4g % .4G" [0.0m; 0.1m; 10.001m; -34.73m; 1000000000.343m]

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
testIntegerFlagsWithPaddingSigned ()
testIntegerFlagsWithPaddingUnsigned ()
testIntegerFlagsWithoutPaddingSigned ()
testIntegerFlagsWithoutPaddingUnsigned ()
testIntegerComplex ()
testFloatBasic ()
testFloatFlagsWithPaddingSingle ()
testFloatFlagsWithoutPaddingSingle ()
testFloatFlagsWithPaddingDouble ()
testFloatFlagsWithoutPaddingDouble ()
testFloatFlagsWithPaddingDecimal ()
testFloatFlagsWithoutPaddingDecimal ()
testFixedBasic ()

printfn "%d tests passed" !testCounter
