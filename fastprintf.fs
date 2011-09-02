module FastPrintf

open System

[<Flags>]
type FormatFlags =
| None = 0
| ZeroFill = 1
| LeftJustify = 2
| AddSignIfPositive = 4
| AddSpaceIfPositive = 8

type FormatElement =
| Literal of string
| Argument of FormatFlags * int * int * char

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

let parseArgument (fmt: string) i =
    let i, flags = parseFormatFlags fmt i FormatFlags.None
    let i, width = parseNumber fmt i 0
    let i, precision = parsePrecision fmt i -1
    i + 1, Argument (flags, width, precision, fmt.[i])

let parseFormatString (fmt: string) =
    let rec loop i acc =
        if i >= fmt.Length then acc
        else if fmt.[i] = '%' then
            let i, el = parseArgument fmt (i + 1)
            loop i (el :: acc)
        else
            let e = parseString fmt i
            loop e (Literal (fmt.Substring(i, e - i)) :: acc)
    loop 0 [] |> List.rev
