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

let rec getFunctionArguments (typ: Type) = 
    let arg, res = FSharpType.GetFunctionElements typ 
    arg :: (if FSharpType.IsFunction res then getFunctionArguments res else [res])

let getFormatterFactory (typ: Type) =
    let arg, res = FSharpType.GetFunctionElements typ 
    typeof<FormatterFactory>.GetMethod("Create").MakeGenericMethod([|arg; res|]).Invoke(null, [||]) :?> (FormatContext -> obj)

let rec getFormatParts (els: FormatElement list) (typ: Type) =
    match els with
    | [] ->
        if typ <> typeof<string> then failwithf "Residue %A" typ
        []
    | x :: xs ->
        let arg, res = FSharpType.GetFunctionElements typ 
        let str =
            if arg = typeof<int> then box (fun (x: int) -> x.ToString())
            else if arg = typeof<string> then box (fun (x: string) -> x)
            else failwithf "Unsupported type %A" arg
        let next =
            if FSharpType.IsFunction res then
                getFormatterFactory res
            else
                if res <> typeof<string> then failwithf "Residue %A" res
                fun ctx -> ctx.res.ToString() |> box
        { new FormatPart with func = str and element = x and next = next } :: getFormatParts xs res

let sprintf (fmt: PrintfFormat<'a, _, _, string>) =
    let prefix, els = parseFormatString fmt.Value
    let pel = { new FormatElement with flags = FormatFlags.None and width = 0 and precision = 0 and typ = '^' and postfix = prefix }
    let parts = getFormatParts els typeof<'a>
    let ctx = { new FormatContext with res = StringBuilder().Append(prefix) and arg = parts }
    let start = getFormatterFactory typeof<'a>
    unbox (start ctx)
