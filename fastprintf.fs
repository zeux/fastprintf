module FastPrintf

open System
open System.Text
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
    { res: StringBuilder
      arg: FormatPart list }

type Formatter<'T, 'Result>(ctx: FormatContext) =
    inherit FSharpFunc<'T, 'Result>()

    override this.Invoke (v: 'T) =
        match ctx.arg with
        | x :: xs ->
            ctx.res.Append((x.func :?> 'T -> string) v) |> ignore
            ctx.res.Append(x.element.postfix) |> ignore
            unbox (x.next {ctx with arg = xs})
        | _ -> failwith "Internal error"

type FormatterFactory =
    static member Create<'T, 'Result> () =
        fun ctx -> box (Formatter<'T, 'Result>(ctx))

let getFormatterFactory (typ: Type) =
    let arg, res = FSharpType.GetFunctionElements typ 
    typeof<FormatterFactory>.GetMethod("Create").MakeGenericMethod([|arg; res|]).Invoke(null, [||]) :?> (FormatContext -> obj)

let addPadding (e: FormatElement) (conv: 'a -> string) =
    if e.width = 0 then conv
    else
        let ch = if e.flags.HasFlag(FormatFlags.ZeroFill) then '0' else ' '
        if e.flags.HasFlag(FormatFlags.LeftJustify) then
            fun x -> (conv x).PadLeft(e.width, ch)
        else
            fun x -> (conv x).PadRight(e.width, ch)

let inline toStringInteger (e: FormatElement) unsigned : ^T -> string =
    match e.typ with
    | 'd' | 'i' ->
        if e.flags.HasFlag(FormatFlags.AddSignIfPositive) || e.flags.HasFlag(FormatFlags.AddSpaceIfPositive) then
            let pad = if e.flags.HasFlag(FormatFlags.AddSignIfPositive) then "+" else " "
            fun (x: 'T) -> let s = x.ToString() in if x >= Unchecked.defaultof<'T> then pad + s else s
        else
            fun (x: 'T) -> x.ToString()
    | 'u' -> fun (x: 'T) -> (x |> unsigned).ToString()
    | 'x' -> fun (x: ^T) -> (^T: (member ToString: string -> string) (x, "x"))
    | 'X' -> fun (x: ^T) -> (^T: (member ToString: string -> string) (x, "X"))
    | 'o' -> fun (x: 'T) -> Convert.ToString(x |> unsigned |> int64, 8)
    | _ -> failwithf "Unrecognized integer type specifier '%c'" e.typ

let inline toStringFloat (e: FormatElement) : ^T -> string =
    let fmt = e.typ.ToString() + (if e.precision < 0 then "6" else (max (min e.precision 99) 0).ToString())
    fun (x: 'T) -> (^T: (member ToString: string -> string) (x, fmt))

let fin<'T> e (f: 'T -> string) =
    f |> addPadding e |> box

let toString (e: FormatElement) (typ: Type) =
    if typ = typeof<int8> then toStringInteger e uint8 |> fin<int8> e
    else if typ = typeof<uint8> then toStringInteger e uint8 |> fin<uint8> e
    else if typ = typeof<int16> then toStringInteger e uint16 |> fin<int16> e
    else if typ = typeof<uint16> then toStringInteger e uint16 |> fin<uint16> e
    else if typ = typeof<int32> then toStringInteger e uint32 |> fin<int32> e
    else if typ = typeof<uint32> then toStringInteger e uint32 |> fin<uint32> e
    else if typ = typeof<int64> then toStringInteger e uint64 |> fin<int64> e
    else if typ = typeof<uint64> then toStringInteger e uint64 |> fin<uint64> e
    else if typ = typeof<nativeint> then toStringInteger e unativeint |> fin<nativeint> e
    // else if typ = typeof<unativeint> then toStringInteger e unativeint |> fin<unativeint> e
    else if typ = typeof<float32> then toStringFloat e |> fin<float32> e
    else if typ = typeof<float> then toStringFloat e |> fin<float> e
    else if typ = typeof<decimal> then toStringFloat e |> fin<decimal> e
    else if typ = typeof<string> then (fun (x: string) -> x) |> fin<string> e
    else failwithf "Unrecognized type %A" typ

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
    let ctx = { new FormatContext with res = StringBuilder().Append(prefix) and arg = parts }
    let start = getFormatterFactory typeof<'a>
    unbox (start ctx): 'a
