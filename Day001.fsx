open System.Text.RegularExpressions

let both f g x = (f x, g x)
 
let foldl = Seq.fold

let isDigit c = System.Char.IsDigit c
let atoi (c:char) = int c - int '0'

let splitOn (c:char) (s:string) = s.Split c
let parseRegex regex map s =  Regex.Match(s,regex) |> fun m -> m.Groups
                              |> Seq.skip 1 // ignore first group
                              |> Seq.map (fun a -> a.Value) |> Array.ofSeq |> map
open System
open System.IO

let path = $@"{__SOURCE_DIRECTORY__}\Day001.txt"
let lines = File.ReadAllLines path;

let first (s:string) (i:string) = s.IndexOf(i)
let last (s:string) (i:string) = s.LastIndexOf(i)

let substitutes = [("one","1"); ("two","2"); ("three","3"); ("four","4"); ("five","5"); ("six","6"); ("seven","7"); ("eight","8"); ("nine","9")]

let calibrate = both (Seq.find isDigit) (Seq.findBack isDigit) >> fun (cx,cy) -> 10 * (atoi cx) + (atoi cy)

let part1 = Seq.map calibrate >> Seq.sum 

let replace index selBy (s:string) = List.map (fun (on,r) -> (index s on,r) ) substitutes
                                     |> List.filter (fun (x,_) -> x <> -1)
                                     |> function
                                        | [] -> s
                                        | xs -> xs |> selBy fst |> s.Insert


let part2 = Seq.map (replace first List.minBy >> replace last List.maxBy) >> part1

let Solve : (string seq -> int * int)  = both part1 part2

Solve lines