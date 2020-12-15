// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp


open System

let addOneToSecond (pair:int64*int64):int64*int64 = (fst pair,snd pair + 1L)
let swapPair (pair:int64*int64) : int64*int64 = (snd pair,fst pair)

type Cache (map:Map<int64,int64>) as self =
    member this.Map = map
    member this.add (num:int64) (turn:int64) =
//        printfn "Add: num=%d at turn=%d" num turn 
        let newMap = map.Add (num,turn) 
        Cache(newMap)
    member this.last (num:int64) (turn:int64) : int64 =
        if map.ContainsKey num then
//            printfn "Seen %d at round %d" num (map.[num])
            turn - map.[num]; 
        else
//            printfn "Never seen before: %d" num 
            0L

let rec readMore (numbers:List<int64>) (lastTime:Cache) (turn:int64) =
    if turn % 1000000L = 0L then
        printf "."
    let prev : int64 = numbers.Head  
    if turn = 30000000L then
        prev 
    else 
        let since : int64 = lastTime.last prev turn 
//        printfn "@[%d] %d was last seen %d rounds ago" turn prev since 
        let newCache : Cache = lastTime.add prev turn        
        readMore (since::numbers) newCache (turn+1L)         

let task1 (numbers:int64[]) =
    let list = numbers |> Seq.rev |> Seq.toList
    let lastTime : Map<int64,int64> =
        list
        |> Seq.rev 
        |> Seq.mapi (fun i v -> (v,i + 1 |> int64))
        |> Map.ofSeq 
    let initCache = Cache lastTime 
    readMore (0L::list) (Cache lastTime) (list.Length + 1 |> int64) 

[<EntryPoint>]
let main argv =
    let input2 = [|0L;3L;6L|]
    let input = [|11L;18L;0L;20L;1L;7L;16L|]
    let message = "from hello"
    let answer = task1 input
    printfn "Answer %d" answer 
    0 // return an integer exit code