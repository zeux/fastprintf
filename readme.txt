fastprintf, by Arseny Kapoulkine (arseny.kapoulkine@gmail.com)

This is the distribution of fastprintf package. It is a replacement for
printf function family, that is intended to maximize the performance,
while preserving the behavior of the original functions.

Installation process (using NuGet):

PM> Install-Package FastPrintf

Installation process (using source):

- Include fastprintf.fs in all your assemblies (as the first file in the list);
if your project has a 'core' assembly that is referenced by all other assemblies,
it's sufficient to include the file in just the core assembly.

Installation process (using prebuilt assembly):

- Include fastprintf.dll as a dependency for all assemblies in your project.
The distributed assembly is compiled using F# 3.0; you can compile it yourself:

> fsc fastprintf.fs /optimize /tailcalls /target:library

Configuration:

There are two configuration defines; use them to compile fastprintf for specific
environments:

FASTPRINTF_COMPAT_FS2 - enables F# 2.0 compatibility. This removes padding support
for %c specifier to be bug-compatible with F# 2.0 Core.

FASTPRINTF_COMPAT_FX3 - enables .NET 3.x compatibility. This replaces ConcurrentDictionary
reference with plain Dictionary with a lock.

How it works:

- The library replicates the structure of printf methods in FastPrintf module,
which is marked as AutoOpen; adding the assembly or the source file to the project
makes unqualified (printf) or partially qualified (Printf.printf) calls use
the custom versions in the library.
Fully-qualified names (Microsoft.FSharp.Core.Printf.printf) still refer to F# core
version.

- Library compiles the format specification and a set of types to a sequence of
functions that do the necessary formatting; the compilation result is cached by
format string and type list. The compilation process uses reflection introspection
and generics, but does not use dynamic code generation (Reflection.Emit).

- Library uses a fast thread-safe cache (ConcurrentDictionary backed by fast
thread-local cache to reduce cache hit time).

- All format specifiers except %A are relatively fast; %A implementation is specialized
for several known types (primitive types, strings, etc.), but falls back to F# core
implementation, which is slow. It is possible to improve the performance of %A in more
cases, but this has not been done yet.

License:

This library is distributed under the MIT License:

Copyright (c) 2011-2012 Arseny Kapoulkine

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the "Software"), to deal in the Software without
restriction, including without limitation the rights to use,
copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.
