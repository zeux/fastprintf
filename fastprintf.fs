module FastPrintf

open System
open System.Collections.Generic
open System.Globalization
open System.Reflection

open Microsoft.FSharp.Reflection

[<Flags>]
type FormatFlags =
| None = 0
| ZeroFill = 1
| LeftJustify = 2
| AddSignIfPositive = 4
| AddSpaceIfPositive = 8

type FormatElement =
    { flags: FormatFlags
      width: int
      precision: int
      typ: char
      postfix: string }

let inline hasFlag e f = (e &&& f) = f

let parseFormatFlag ch =
    match ch with
    | '0' -> FormatFlags.ZeroFill
    | '-' -> FormatFlags.LeftJustify
    | '+' -> FormatFlags.AddSignIfPositive
    | ' ' -> FormatFlags.AddSpaceIfPositive
    | _ -> FormatFlags.None

let rec parseFormatFlags (fmt: string) i acc =
    match parseFormatFlag fmt.[i] with
    | FormatFlags.None -> i, acc
    | x -> parseFormatFlags fmt (i + 1) (acc ||| x)

let rec parseNumber (fmt: string) i acc =
    match fmt.[i] with
    | ch when ch >= '0' && ch <= '9' -> parseNumber fmt (i + 1) (acc * 10 + int ch - int '0')
    | _ -> i, acc

let rec parseString (fmt: string) i =
    if i >= fmt.Length || fmt.[i] = '%' then i
    else parseString fmt (i + 1)

let parsePrecision (fmt: string) i def =
    if fmt.[i] = '.' then parseNumber fmt (i + 1) 0
    else i, def

let parseElement (fmt: string) i =
    let i, flags = parseFormatFlags fmt i FormatFlags.None
    let i, width = parseNumber fmt i 0
    let i, precision = parsePrecision fmt i -1
    i + 1, { new FormatElement with flags = flags and width = width and precision = precision and typ = fmt.[i] and postfix = "" }

let parseFormatString (fmt: string) =
    let rec loop i acc =
        if i >= fmt.Length then acc
        else
            assert (fmt.[i] = '%')
            let i, el = parseElement fmt (i + 1)
            let e = parseString fmt i
            loop e ({el with postfix = fmt.Substring(i, e - i)} :: acc)

    let prefix = parseString fmt 0
    fmt.Substring(0, prefix), loop prefix [] |> List.rev

type FormatPart =
    { func: obj // 'T -> string
      element: FormatElement
      next: FormatContext -> obj }

and FormatContext =
    { res: string
      arg: FormatPart list }

type Formatter<'T, 'Result>(ctx: FormatContext) =
    inherit FSharpFunc<'T, 'Result>()

    override this.Invoke (v: 'T) =
        match ctx.arg with
        | x :: xs ->
            let res = ctx.res + ((x.func :?> 'T -> string) v) + x.element.postfix
            unbox (x.next {new FormatContext with res = res and arg = xs})
        | _ -> failwith "Internal error"

type Factory =
    static member CreateFormatter<'T, 'Result> () =
        fun ctx -> box (Formatter<'T, 'Result>(ctx))

    static member CreateBoxString<'T> () =
        fun (o: 'T) -> if Object.ReferenceEquals(o, null) then "<null>" else o.ToString()

    static member CreateGenericString<'T> () =
        fun (o: 'T) -> Printf.sprintf "%A" o

let getFormatterFactory (typ: Type) =
    let arg, res = FSharpType.GetFunctionElements typ 
    typeof<Factory>.GetMethod("CreateFormatter").MakeGenericMethod([|arg; res|]).Invoke(null, [||]) :?> (FormatContext -> obj)

let getBoxStringFunction (typ: Type) =
    typeof<Factory>.GetMethod("CreateBoxString").MakeGenericMethod([|typ|]).Invoke(null, [||])

let getGenericStringFunction (e: FormatElement) (typ: Type) =
    typeof<Factory>.GetMethod("CreateGenericString").MakeGenericMethod([|typ|]).Invoke(null, [||])

let addPadding (e: FormatElement) (conv: 'a -> string) =
    if e.width = 0 then conv
    else
        let ch = if hasFlag e.flags FormatFlags.ZeroFill then '0' else ' '
        if hasFlag e.flags FormatFlags.LeftJustify then
            fun x -> (conv x).PadLeft(e.width, ch)
        else
            fun x -> (conv x).PadRight(e.width, ch)

let inline toStringInvariant (x: ^T) =
    (^T: (member ToString: IFormatProvider -> string) (x, CultureInfo.InvariantCulture))

let inline toStringFormatInvariant (x: ^T) format =
    (^T: (member ToString: string -> IFormatProvider -> string) (x, format, CultureInfo.InvariantCulture))

let inline toStringInteger (e: FormatElement) unsigned : ^T -> string =
    match e.typ with
    | 'd' | 'i' ->
        if hasFlag e.flags FormatFlags.AddSignIfPositive || hasFlag e.flags FormatFlags.AddSpaceIfPositive then
            let pad = if hasFlag e.flags FormatFlags.AddSignIfPositive then "+" else " "
            fun (x: 'T) -> let s = toStringInvariant x in if x >= Unchecked.defaultof<'T> then pad + s else s
        else
            fun (x: 'T) -> toStringInvariant x
    | 'u' -> fun (x: 'T) -> toStringInvariant (x |> unsigned)
    | 'x' -> fun (x: ^T) -> toStringFormatInvariant x "x"
    | 'X' -> fun (x: ^T) -> toStringFormatInvariant x "X"
    | 'o' -> fun (x: 'T) -> Convert.ToString(x |> unsigned |> int64, 8)
    | _ -> failwithf "Unrecognized integer type specifier '%c'" e.typ

let inline toStringFloat (e: FormatElement) : ^T -> string =
    let fmt = e.typ.ToString() + (if e.precision < 0 then "6" else (max (min e.precision 99) 0).ToString())
    
    fun (x: 'T) -> toStringFormatInvariant x fmt

let fin<'T> e (f: 'T -> string) =
    f |> addPadding e |> box

let toString (e: FormatElement) (typ: Type) =
    match e.typ with
    | 'b' -> (fun x -> if x then "true" else "false") |> box
    | 'c' -> (fun (x: char) -> x.ToString()) |> box
    | 'd' | 'i' | 'u' | 'x' | 'X' | 'o' ->
        match Type.GetTypeCode(typ) with
        | TypeCode.SByte -> toStringInteger e uint8 |> fin<int8> e
        | TypeCode.Byte -> toStringInteger e uint8 |> fin<uint8> e
        | TypeCode.Int16 -> toStringInteger e uint16 |> fin<int16> e
        | TypeCode.UInt16 -> toStringInteger e uint16 |> fin<uint16> e
        | TypeCode.Int32 -> toStringInteger e uint32 |> fin<int32> e
        | TypeCode.UInt32 -> toStringInteger e uint32 |> fin<uint32> e
        | TypeCode.Int64 -> toStringInteger e uint64 |> fin<int64> e
        | TypeCode.UInt64 -> toStringInteger e uint64 |> fin<uint64> e
        | _ -> failwithf "Unrecognized type %A" typ
    | 'e' | 'E' | 'f' | 'F' | 'g' | 'G' ->
        match Type.GetTypeCode(typ) with
        | TypeCode.Single -> toStringFloat e |> fin<float32> e
        | TypeCode.Double -> toStringFloat e |> fin<float> e
        | TypeCode.Decimal -> toStringFloat e |> fin<decimal> e
        | _ -> failwithf "Unrecognized type %A" typ
    | 'M' ->
        if typ = typeof<decimal> then (fun (x: decimal) -> toStringInvariant x) |> fin<decimal> e
        else failwithf "Unrecognized type %A" typ
    | 's' ->
        if typ = typeof<string> then (fun (x: string) -> x) |> fin<string> e
        else failwithf "Unrecognized type %A" typ
    | 'O' -> getBoxStringFunction typ
    | 'A' -> getGenericStringFunction e typ
    | _ -> failwithf "Unrecognized format type %c" e.typ

let rec getFormatParts (els: FormatElement list) (typ: Type) =
    match els with
    | [] ->
        if typ <> typeof<string> then failwithf "Residue %A" typ
        []
    | x :: xs ->
        let arg, res = FSharpType.GetFunctionElements typ 
        let str = toString x arg
        let next =
            if FSharpType.IsFunction res then
                getFormatterFactory res
            else
                if res <> typeof<string> then failwithf "Residue %A" res
                fun ctx -> ctx.res.ToString() |> box
        { new FormatPart with func = str and element = x and next = next } :: getFormatParts xs res

let sprintf (fmt: PrintfFormat<'a, _, _, string>) =
    let prefix, els = parseFormatString fmt.Value
    let parts = getFormatParts els typeof<'a>
    let ctx = { new FormatContext with res = prefix and arg = parts }
    if FSharpType.IsFunction typeof<'a> then
        let start = getFormatterFactory typeof<'a>
        unbox (start ctx): 'a
    else
        unbox prefix: 'a

let cache = Dictionary<string, string * FormatPart list * (FormatContext -> obj)>()

let sprintfc (fmt: PrintfFormat<'a, _, _, string>) =
    let prefix, parts, start =
        match cache.TryGetValue(fmt.Value) with
        | true, v -> v
        | _ ->
            let prefix, els = parseFormatString fmt.Value
            let parts = getFormatParts els typeof<'a>
            let start = getFormatterFactory typeof<'a>
            cache.Add(fmt.Value, (prefix, parts, start))
            prefix, parts, start

    let ctx = { new FormatContext with res = prefix and arg = parts }
    unbox (start ctx): 'a
