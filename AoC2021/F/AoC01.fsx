open System.IO

let readfile filename = 
    File.ReadAllLines(filename)
                |> Seq.toList
                |> List.map(fun x -> int x)


let rec depthchange current li count =
    match li with 
    | [] -> count
    | _ -> 
        if (List.head li) > current then
            depthchange (List.head li) (List.tail li) count + 1
        else 
            depthchange (List.head li) (List.tail li) count

let window xs =
    xs
        |> List.take 3
        |> List.sum :int

let rec slidingwindow  current xs count =
    match (List.length xs >= 3) with
        | true ->   if window xs > current then 
                        slidingwindow (window xs) (List.tail xs) (count + 1)
                    else 
                        slidingwindow (window xs) (List.tail xs) count
        | _ -> count
        
        

printfn "Hello world AoC"
let filename = "aoc01.txt"
let content = readfile filename 
let count = depthchange (List.head content) content 0 
let task2_count = slidingwindow (window content) content 0
printfn "Task 1: %d" count
printfn "Task 2: %d" task2_count
