namespace Spidy

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Net.Http
open System.Threading
open Http
open Links
open Robots
open Types
open Utilities

module Crawler =

    let private event = Event<System.EventArgs>()
    let CrawlingCompleted = event.Publish

    let private crawlUrl f rogueMode depth client requestData =
        async {
            let requestUri, sourceUri, permission = requestData
            let requestUri' = requestUri.ToString()
            try
                let! response = awaitHttpResponse client requestUri'
                let requestUri'' = response.RequestMessage.RequestUri.ToString()
                let wasRedirected = requestUri' <> requestUri''
                match wasRedirected with
                    | true  -> return [Uri requestUri'', sourceUri, permission]
                    | false ->
                        let httpContent = response.Content
                        let responseHeaders = response.Headers
                        let contentHeaders = httpContent.Headers
                        let headers = httpHeaders responseHeaders contentHeaders
                        let contentType = contentHeaders.ContentType.MediaType
                        let isHtml = htmlContentRegex.IsMatch contentType
                        let! content = readContent isHtml httpContent
                        let baseUriOption = baseUri content requestUri
                        let linkHeaderOption = linkCanonicalHeader headers baseUriOption depth
                        match linkHeaderOption with
                            | None ->
                                let canonicalUriOption = canonicalUri content baseUriOption depth
                                match canonicalUriOption with
                                    | None ->
                                        let directives = robotsDirectives headers content permission
                                        let statusCode = response.StatusCode
                                        let uri = Uri requestUri'
                                        let links = collectLinks uri isHtml depth baseUriOption content
                                        let httpData = constructHttpData requestUri' headers directives (Some contentType) content statusCode sourceUri links
                                        do! f httpData
                                        let following = directives.Following
                                        return
                                            match rogueMode, following with
                                                | OFF, DoFollow ->
                                                    links
                                                    |> List.partition (fun (_, follow) -> follow = DoFollow)
                                                    |> fst
                                                    |> List.map (fst >> (fun x -> x, requestUri', None))
                                                | OFF, NoFollow -> []
                                                | _ -> links |> List.map (fst >> (fun x -> x, requestUri', None))
                                    | Some x -> return [x, requestUri', permission]
                                | Some linkHeader -> return [linkHeader, requestUri', permission]
            with _ -> return []
        }

    [<Literal>]
    let private Gate = 5

    let private processMsg (hashset : HashSet<string>) limitOption (q : ConcurrentQueue<Uri * string * Permission option>) run (mailbox : MessageAgent) =
        let keepRunning =
            match limitOption with
            | None       -> run
            | Some limit -> hashset.Count < limit && run
        match keepRunning with
            | true ->
                let uriData = q.TryDequeue()
                match uriData with
                    | true, (uri, sourceUri, permission) ->
                        let wasntCrawled = hashset.Add <| uri.ToString()
                        match wasntCrawled with
                            | false -> mailbox.Post <| UriData None
                            | true  -> mailbox.Post <| UriData(Some (uri, sourceUri, permission))
                    | _ -> mailbox.Post <| UriData None
            | false -> mailbox.Post Stop

    let private spawnSupervisor hashset limit q completionFunc =
        let processMsg' = processMsg hashset limit q
        MailboxProcessor.Start(fun inbox ->
            let rec loop run cancel =
                async {
                    let! msg = inbox.Receive()
                    match msg with
                        | Mailbox mailbox -> 
                            processMsg' run mailbox
                            return! loop run cancel
                        | Stop   -> return! loop false cancel
                        | Cancel -> return! loop false true
                        | _      ->
                            match cancel with
                                | true  -> ()
                                | false -> Async.Start completionFunc
                            dispose inbox
                            event.Trigger EventArgs.Empty
                }
            loop true false)

    let private spawnRobotsAgent (q : ConcurrentQueue<Uri * string * Permission option>) =
        MessageAgent.Start(fun inbox ->
            let rec loop botsList =
                async {
                    let! msg = inbox.Receive()
                    match msg with
                        | UriData (Some (uri, sourceUri, permission)) ->
                            let botOption = botsList |> List.tryFind (fun x -> fst x = uri.Host)
                            match botOption with
                                | Some (host, bot) ->
                                    let permission' = isAllowed bot <| uri.ToString()
                                    q.Enqueue (uri, sourceUri, Some permission')
                                    return! loop botsList
                                | None ->
                                    let! bot = catchAllBot uri
                                    let permission' = isAllowed bot <| uri.ToString()
                                    q.Enqueue (uri, sourceUri, Some permission')
                                    let botsList' = (uri.Host, bot) :: botsList
                                    return! loop botsList'
                        | _ -> dispose inbox
                }
            loop [])

    let private isAllowedFunc allowedHosts =
        match allowedHosts with
            | None -> fun _ -> true
            | Some allowedHosts' ->
                match allowedHosts' with
                    | [] -> fun _ -> true
                    | allowedHosts'' ->
                        let hashSet = HashSet<string>()
                        allowedHosts'' |> List.iter (fun x -> hashSet.Add x |> ignore)
                        let isAllowed host = hashSet.Contains host = true
                        isAllowed

    let private spawnUrlCollector allowedHosts (supervisor : MessageAgent) (botsAgent : MessageAgent) =
        let isAllowedHost = isAllowedFunc allowedHosts
        MailboxProcessor.Start(fun inbox ->
            let rec loop count =
                async {
                    let! msg = inbox.TryReceive 60000
                    match msg with
                    | Some message ->
                        match message with
                            | UriData uriDataOption ->
                                match uriDataOption with
                                    | Some uriData ->
                                        let uri', _, _ = uriData
                                        match isAllowedHost uri'.Host with
                                            | true  -> botsAgent.Post message
                                            | false -> ()
                                    | None -> ()
                                return! loop count
                            | _ ->
                                match count with
                                    | Gate ->
                                        supervisor.Post Done
                                        botsAgent.Post Done
                                        dispose inbox
                                    | _ -> return! loop (count + 1)
                    | None ->
                        supervisor.Post Stop
                        return! loop count
                }
            loop 1)

    let private spawnCrawler (urlCollector : MessageAgent) (supervisor : MessageAgent) crawlFunc =
        let client = httpClient ()
        let crawlFunc' = crawlFunc client
        MailboxProcessor.Start(fun inbox ->
            let rec loop () =
                async {
                    let! msg = inbox.Receive()
                    match msg with
                        | UriData uriDataOption ->
                            match uriDataOption with
                                | Some uriData ->
                                    try
                                        use cancelTokenSource = new CancellationTokenSource()
                                        let token = cancelTokenSource.Token
                                        let asyncComp = crawlFunc' uriData
                                        let task = Async.StartAsTask(asyncComp, cancellationToken = token)
                                        let success = task.Wait 55000
                                        match success with
                                            | true  ->
                                                task.Result
                                                |> List.iter (fun x -> urlCollector.Post <| UriData (Some x))
                                            | false -> cancelTokenSource.Cancel()
                                    with _ -> ()
                                | None -> ()
                            supervisor.Post(Mailbox inbox)
                            return! loop()
                        | _ ->
                            urlCollector.Post Done
                            client.Dispose()
                            dispose inbox
                }
            loop ())

    let private spawnCanceler (supervisor : MessageAgent) =
        MessageAgent.Start(fun inbox ->
            let rec loop () =
                async {
                    let! msg = inbox.Receive()
                    match msg with
                        | Stop | Done ->
                            supervisor.Post msg
                            dispose inbox
                        | _ -> return! loop ()
                }
            loop ())

    let crawl config =
        async {
            let cq = ConcurrentQueue()
            let hashset = HashSet<string>()
            let supervisor = spawnSupervisor hashset config.Limit cq config.CompletionFunc
            let botsAgent = spawnRobotsAgent cq
            let urlCollector = spawnUrlCollector config.AllowedHosts supervisor botsAgent
            config.Seeds
            |> List.map (fun x -> UriData(Some (x, x.ToString(), None)))
            |> List.iter urlCollector.Post
            let crawlUrl' = crawlUrl config.HttpDataFunc config.RogueMode config.Depth
            let canceler = spawnCanceler supervisor
            [1 .. Gate]
            |> List.map (fun x -> spawnCrawler urlCollector supervisor crawlUrl')
            |> List.iter (fun agent -> agent.Post <| UriData None)
            return canceler
        }