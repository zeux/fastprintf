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

// decimals: all flags & combinations (0-+ ) with/without padding
let testDecimalFlags () =
    tf6l "%020.4M %0-20.4M %0-+20.4M %0- 20.4M %0+20.4M %0 20.4M" [0.0m; 0.1m; 10.001m; -34.73m; 1000000000.343m]
    tf6l "%20.4M %-20.4M %-+20.4M %- 20.4M %+20.4M % 20.4M" [0.0m; 0.1m; 10.001m; -34.73m; 1000000000.343m]
    tf6l "%0.4M %0-.4M %0-+.4M %0- .4M %0+.4M %0 .4M" [0.0m; 0.1m; 10.001m; -34.73m; 1000000000.343m]
    tf6l "%.4M %-.4M %-+.4M %- .4M %+.4M % .4M" [0.0m; 0.1m; 10.001m; -34.73m; 1000000000.343m]

// misc fixed types (i.e. no variation), no flags
let testFixedBasic () =
    tf2 "%b %b" true false
    tf2 "%c %c" ' ' 'x'
    tf3 "%s %s %s" "" "booo" null

// misc fixed types (i.e. no variation), all flags (only - is supported, thank god) with/without padding
let testFixedFlags () =
    tf4 "%10b %10c %10s %10s" true 'x' "booo" null
    tf4 "%-10b %-10c %-10s %-10s" true 'x' "booo" null
    tf4 "%1b %1c %1s %1s" true 'x' "booo" null
    tf4 "%-1b %-1c %-1s %-1s" true 'x' "booo" null
    tf4 "%b %c %s %s" true 'x' "booo" null
    tf4 "%-b %-c %-s %-s" true 'x' "booo" null

// helper class type for ToString() testing
type X() =
    override this.ToString() =
        "custom X " + this.GetHashCode().ToString()

// helper value type for ToString() testing
[<Struct>]
type Y =
    override this.ToString() =
        "custom Y struct"

// helper class type for ToString() exception testing
type Z() =
    override this.ToString () =
        failwith "oops"

// helper enum
type W =
| One = 1
| Two = 2

// helper record
type U = {x: int; y: float; z: string}

// helper private record
type private V = {x: int; y: float; z: string}

// generic type %O (ToString())
let testGenericOBasic () =
    tf8 "%O %O %O %O %O %O %O %O" 1y 1 1UL "foo" null true (X()) (Y())
    tf8 "%10O %10O %10O %10O %10O %10O %10O %10O" 1y 1 1UL "foo" null true (X()) (Y())
    tf8 "%-10O %-10O %-10O %-10O %-10O %-10O %-10O %-10O" 1y 1 1UL "foo" null true (X()) (Y())

// generic type %A
let testGenericABasic () =
    tf8 "%A %A %A %A %A %A %A %A" 1y 1 1UL "foo" null true (X()) (Y())
    tf8 "%10A %10A %10A %10A %10A %10A %10A %10A" 1y 1 1UL "foo" null true (X()) (Y())
    tf8 "%-10A %-10A %-10A %-10A %-10A %-10A %-10A %-10A" 1y 1 1UL "foo" null true (X()) (Y())

// generic type %A, various types
let testGenericATypes () =
    tf8 "%A %A %A %A %A %A %A %A" -1y 1uy -1s 1us -1 1u -1L 1UL
    tf8 "%A %A %A %A %A %A %A %A" 1n 1un 1.f 1.0 1.0m true "oo" 'x'
    tf6 "%A %A %A %A %A %A" (X()) (Y()) testGenericABasic W.One (ref 0) {new U with x = 5 and y = 6.0 and z = "woo"}
    tf8 "%A %A %A %A %A %A %A %A" 'x' '\t' '\b' '\r' '\n' '\000' '\012' '\031'
    tf6 "%A %A %A %A %A %A" null "" "x" "\"fo\"" "\r\n\b\t\000\012\031" "'"
    tf8 "%A %A %A %A %A %A %A %A" 0.f 1.f -1.f 1000.f 1000.1f infinityf -infinityf nanf
    tf8 "%A %A %A %A %A %A %A %A" 0.0 1.0 -1.0 1000.0 1000.1 infinity -infinity nan
    tf8 "%A %A %A %A %A %A %A %A" 2147483647.f 2147483648.f -2147483648.f -2147483649.f 2147483647.0 2147483648.0 -2147483648.0 -2147483649.0

// generic type %A, various flags
let testGenericAFlags () =
    let u = {new U with x = 5 and y = 6.0 and z = "woo"}
    let v = {new V with x = 5 and y = 6.0 and z = "woo"} // V is private
    tf5 "%A %A %A %A %A" u v 2.0 true "boo"
    tf5 "%+A %+A %+A %+A %+A" u v 2.0 true "boo"
    tf5 "%0A %0A %0A %0A %0A" u v 2.0 true "boo"
    tf5 "%+0A %+0A %+0A %+0A %+0A" u v 2.0 true "boo"
    tf5 "%4A %4A %4A %4A %4A" u v 2.0 true "boo"
    tf5 "%+4A %+4A %+4A %+4A %+4A" u v 2.0 true "boo"
    tf5 "%04A %04A %04A %04A %04A" u v 2.0 true "boo"
    tf5 "%+04A %+04A %+04A %+04A %+04A" u v 2.0 true "boo"
    tf5 "%.2A %.2A %.2A %.2A %.2A" u v 2.0 true "boo"
    tf5 "%+.2A %+.2A %+.2A %+.2A %+.2A" u v 2.0 true "boo"
    tf5 "%.0A %.0A %.0A %.0A %.0A" u v 2.0 true "boo"
    tf5 "%+.0A %+.0A %+.0A %+.0A %+.0A" u v 2.0 true "boo"

// generic type %A, ToString() throw exception
let testGenericAToStringExn () =
    tf1 "%A" (Z())

// currying (test for lack of visible internal state)
let testCurry () =
    let f = FastPrintf.sprintf "[%d %f %s %A]"
    let v1 = f 1
    let v2 = f 2
    let v11 = v1 1.0
    let v12 = v1 2.0
    let v21 = v2 1.0
    let v22 = v2 2.0
    let v111 = v11 "a"
    let v112 = v11 "b"
    let v121 = v12 "a"
    let v122 = v12 "b"
    let v211 = v21 "a"
    let v212 = v21 "b"
    let v221 = v22 "a"
    let v222 = v22 "b"

    v111 W.One =! "[1 1.000000 a One]"
    v112 W.One =! "[1 1.000000 b One]"
    v121 W.One =! "[1 2.000000 a One]"
    v122 W.One =! "[1 2.000000 b One]"
    v211 W.One =! "[2 1.000000 a One]"
    v212 W.One =! "[2 1.000000 b One]"
    v221 W.One =! "[2 2.000000 a One]"
    v222 W.One =! "[2 2.000000 b One]"

// generic formatting test for strings
let testGenericTString () =
    tf9 "<%d;%a,%c.%t[%s]%t=%a>" 1 (fun _ i -> string (i + 1)) 5 'x' (fun _ -> "hey!") "boo" (fun _ -> "bar") (fun _ s -> "[" + s + "]") "??"
    tf9 "<%d;%a,%c.%t[%s]%t=%a" 1 (fun _ i -> string (i + 1)) 5 'x' (fun _ -> "hey!") "boo" (fun _ -> "bar") (fun _ s -> "[" + s + "]") "??"
    tf7 "<%d;%a,%c.%t[%s]%t=" 1 (fun _ i -> string (i + 1)) 5 'x' (fun _ -> "hey!") "boo" (fun _ -> "bar")
    tf7 "<%d;%a,%c.%t[%s]%t" 1 (fun _ i -> string (i + 1)) 5 'x' (fun _ -> "hey!") "boo" (fun _ -> "bar")
    tf6 "<%d;%a,%c.%t[%s]" 1 (fun _ i -> string (i + 1)) 5 'x' (fun _ -> "hey!") "boo"
    tf6 "<%d;%a,%c.%t[%s" 1 (fun _ i -> string (i + 1)) 5 'x' (fun _ -> "hey!") "boo"
    tf5 "<%d;%a,%c.%t[" 1 (fun _ i -> string (i + 1)) 5 'x' (fun _ -> "hey!")
    tf5 "<%d;%a,%c.%t" 1 (fun _ i -> string (i + 1)) 5 'x' (fun _ -> "hey!")
    tf4 "<%d;%a,%c." 1 (fun _ i -> string (i + 1)) 5 'x'
    tf4 "<%d;%a,%c" 1 (fun _ i -> string (i + 1)) 5 'x'
    tf3 "<%d;%a," 1 (fun _ i -> string (i + 1)) 5
    tf3 "<%d;%a" 1 (fun _ i -> string (i + 1)) 5
    tf2 ";%a," (fun _ i -> string (i + 1)) 5
    tf2 ":%a" (fun _ i -> string (i + 1)) 5
    tf2 "%a," (fun _ i -> string (i + 1)) 5
    tf2 "%a" (fun _ i -> string (i + 1)) 5
    tf1 "<%d;" 1
    tf1 ";%t," (fun _ -> "boo")
    tf1 ":%t" (fun _ -> "boo")
    tf1 "%t," (fun _ -> "boo")
    tf1 "%t" (fun _ -> "boo")

// generic formatting test for string null values
let testGenericTStringNull () =
    tf1 "%t" (fun _ -> null)
    tf2 "%a" (fun _ _ -> null) null
    tf3 "%t %a" (fun _ -> null) (fun _ _ -> null) null
    tf5 "%a %t %a" (fun _ _ -> null) null (fun _ -> null) (fun _ _ -> null) null

// generic formatting currying test: the functions should only be called once the final argument is passed, the calls should be in order
let testGenericTCurry () =
    let count = ref 0
    let f = FastPrintf.sprintf "[%a %t]"
    let ff = f (fun _ a ->
        !count =! 0
        count := 1
        a)
    let fff = ff "!"

    !count =! 0

    let ffff = fff (fun _ ->
        !count =! 1
        count := 2
        "?")

    !count =! 2
    ffff =! "[! ?]"

// test for various argument count (w/out %a/%t)
let testArgumentCountBasic () =
    tf0 "a"
    tf1 "a%sb" "?"
    tf2 "a%sb%dc" "?" 1
    tf3 "a%sb%dc%cd" "?" 1 'x'
    tf4 "a%sb%dc%cd%be" "?" 1 'x' false
    tf5 "a%sb%dc%cd%be%Of" "?" 1 'x' false (ref 0)
    tf6 "a%sb%dc%cd%be%Of%Ag" "?" 1 'x' false (ref 0) (Some 4)
    tf7 "a%sb%dc%cd%be%Of%Ag%fh" "?" 1 'x' false (ref 0) (Some 4) 0.4
    tf8 "a%sb%dc%cd%be%Of%Ag%fh%xi" "?" 1 'x' false (ref 0) (Some 4) 0.4 255uy
    tf9 "a%sb%dc%cd%be%Of%Ag%fh%xi%Mj" "?" 1 'x' false (ref 0) (Some 4) 0.4 255uy 1.000m

// test for various argument count (with %a/%t)
let testArgumentCountGenericT () =
    tf1 "a%tb" (fun _ -> "x")
    tf2 "a%ab" (fun _ x -> x) "y"
    tf3 "a%ab%tc" (fun _ x -> x) "y" (fun _ -> "z")
    tf4 "a%ab%ac" (fun _ x -> x) "y" (fun _ x -> x + "z") "v"
    tf5 "a%ab%tx%ac" (fun _ x -> x) "y" (fun _ -> "!") (fun _ x -> x + "z") "v"
    tf5 "%t %t %t %t %t" (fun _ -> "a") (fun _ -> "b") (fun _ -> "c") (fun _ -> "d") (fun _ -> "e")

// all tests
let testAll () =
    testLiteral ()
    testLiteralPercent ()
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
    testDecimalFlags ()
    testFixedBasic ()
    testFixedFlags ()
    testGenericOBasic ()
    testGenericABasic ()
    testGenericATypes ()
    testGenericAFlags ()
    testGenericAToStringExn ()
    testCurry ()
    testGenericTString ()
    testGenericTStringNull ()
    testGenericTCurry ()
    testArgumentCountBasic ()
    testArgumentCountGenericT ()

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

testAll ()

printfn "%d tests passed" !testCounter
