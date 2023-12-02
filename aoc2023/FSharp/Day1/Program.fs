open System.IO

// For more information see https://aka.ms/fsharp-console-apps
printfn "Hello from F#"

let lines = File.ReadAllLines(@"day1.txt")
let input = Seq.toList lines

printfn $"%A{input}"
