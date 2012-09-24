#r @"..\Spidy\bin\Release\Spidy.dll"

open System
open Spidy.Types
open Spidy.Crawler

let printAgent = MailboxProcessor.Start(fun inbox ->
    let rec loop count =
        async {
            let! msg = inbox.Receive()
            printfn "%d: %s" count msg
            return! loop (count + 1) }
    loop 1)

let seeds = [Uri "http://twitter.com/"]

let httpDataFunc (httpData : HttpData) =
    async {
        let uri = httpData.RequestUri
        let status = httpData.StatusCode.ToString()
        let msg = sprintf "%s: %s" uri status
        printAgent.Post msg }

let completionFunc = async { printAgent.Post "Done." }

let config =
    {
        Seeds          = seeds
        Depth          = None
        Limit          = None
        AllowedHosts   = Some [seeds.[0].Host]
        RogueMode      = RogueMode.OFF
        HttpDataFunc   = httpDataFunc
        CompletionFunc = completionFunc
    }

let main = crawl config
let canceler = main |> Async.RunSynchronously
        
//canceler.Post Message.Done