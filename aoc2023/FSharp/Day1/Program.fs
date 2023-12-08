open System
open System.IO

let lines = File.ReadAllLines(@"day1.txt")

let charToString ch = ch.ToString

let isDigit (ch: char) = Char.IsDigit ch

let tryParseInt (s: String) =
    try
        s
        |> int
    with :? FormatException ->
        0

let charToInt (a: char) (b: char) = (a.ToString() + b.ToString())
                                   |> tryParseInt

let stringToInt (a: string) (b: string) = (a + b)
                                          |> tryParseInt

let part_line (line: string) =
    let digits = line.ToCharArray()
                 |> Array.filter isDigit
                 |> List.ofArray

    let first = List.head digits
    let last = List.last digits
    charToInt first last


let filterBool struct(b: bool, i: int, ch:string) = b.Equals(true)

let exists (line:string) (pat: string) =
    match line.Contains pat with
    | true ->
        let idxleft = line.IndexOf(pat)
        let idxright = line.LastIndexOf(pat)
        [struct(true, idxleft, pat); struct(true, idxright, pat)]
    | false -> [struct(false, 0, pat)]

let patternMatch (line: string) (pattern: List<string>) =
    pattern
    |> List.map (exists line)


let stripBool struct(b: bool, i: int, ch: string) = (i, ch)

let mapToStringInt str =
    match str with
    | "one" -> "1"
    | "two" -> "2"
    | "three" -> "3"
    | "four" -> "4"
    | "five" -> "5"
    | "six" -> "6"
    | "seven" -> "7"
    | "eight" -> "8"
    | "nine"  -> "9"
    | x -> x


let  mapTupleToInt (i: int, ch: string) = mapToStringInt ch

let part2_line (line: string) =
    let pattern = ["one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine"; "1"; "2"; "3"; "4";"5";"6";"7";"8";"9"]
    let digits = patternMatch line pattern
                 |> List.collect id
                 |> List.filter filterBool
                 |> List.map stripBool


    let sorted = digits
                 |> List.sort

    let first = List.head sorted
                        |> mapTupleToInt

    let last = List.last sorted
               |> mapTupleToInt

    stringToInt first last



[<EntryPoint>]
let main args =
    printfn "Hello from F#"

    let input = Seq.toList lines

    // printfn $"%A{input}"

    let res = input
                |> List.map part_line
                |> List.sum

    printfn "Part 1: %A" res

    let res2 = input |> List.map part2_line |> List.sum



    printfn "Part 2: %A " res2
    0



