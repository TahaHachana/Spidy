namespace Spidy

open System
open System.Text.RegularExpressions
open System.Threading
open Http
open Types
open Utilities

module private Robots =

    let isNotComment str = robotsCommentRegex.IsMatch str = false
        
    /// Constructs the robots.txt full URI.
    let robotstxtUrl (uri : Uri) = String.concat "" ["http://"; uri.Host; "/robots.txt"]

    /// Downloads the content of a robots.txt file in the form of a string array.
    let downloadRobotstxt url =
        async {
            try
                use cancelTokenSource = new CancellationTokenSource()
                let token = cancelTokenSource.Token
                let asyncComp = fetchUrl url
                let task = Async.StartAsTask(asyncComp, cancellationToken = token)
                let success = task.Wait(10000)
                return
                    match success with
                        | true  ->
                            task.Result |> function
                                | None   -> None
                                | Some x ->
                                    splitAtNewline x
                                    |> Array.filter isNotComment
                                    |> Some
                        | false -> cancelTokenSource.Cancel(); None
            with _ -> return None }

    /// Filters robots.txt directives that satisfy a pattern.
    let filterDirectives directives regex =
        directives
        |> List.choose (fun x -> matchOption regex x)
        |> List.map (fun x -> groupValue x 1)
        |> List.filter (fun x -> x <> "")

    /// Creates Bot instances from robots.txt directives lists.
    let bots lsts =
        lsts
        |> List.map (fun lst ->
            let userAgent = List.head lst |> userAgentRegex'.Match |> fun x -> groupValue x 1
            let crawlDelay =
                lst
                |> List.tryPick (fun x -> matchOption crawlDelayRegex x)
                |> function
                    | Some x -> groupValue x 1 |> (fun x -> try float x |> Some with _ -> None)
                    | None   -> None
            let allowDirectives    = filterDirectives lst robotsAllowRegex
            let disallowDirectives = filterDirectives lst robotsDisallowRegex
            {
                Name       = userAgent
                CrawlDelay = crawlDelay
                Allow      = allowDirectives
                Disallow   = disallowDirectives
            })

    /// Generates a regex pattern from a robots.txt directive.
    let directiveRegexPattern directive =
        Regex.Escape directive
        |> (fun x -> Regex("\\\\\*").Replace(x, ".*"))
        |> (fun x -> Regex("\\\\\$$").Replace(x, "$"))

    /// Determines if a URL is alowed for crawling.
    let isAllowed botOption url =
        match botOption with
            | None      -> Allowed
            | Some bot ->
                let disallowed = bot.Disallow
                match disallowed.IsEmpty with
                    | true  -> Allowed
                    | false ->
                        let disallowed' = disallowed |> List.map (fun x -> Disallowed, x)
                        let allowed = bot.Allow |> List.map (fun x -> Allowed   , x)
                        let directives =
                            List.append allowed disallowed'
                            |> List.map (fun (permission, pattern) ->
                                let regex = compileRegex pattern
                                permission, regex, pattern.Length)
                        directives
                        |> List.filter (fun (_, regex, _) -> regex.IsMatch(url))
                        |> function
                            | [] -> Allowed
                            | x ->
                                x
                                |> List.maxBy (fun (_, _, length) -> length)
                                |> fun (permission, _, _) -> permission

    /// Attempts to find the catchall bot (*) in a Bot list.
    let catchAllBot url =
        async {
            let uri = robotstxtUrl url
            let! directivesOption = downloadRobotstxt uri
            match directivesOption with
                | None            -> return None
                | Some directives ->
                    return
                        directives
                        |> Array.toList
                        |> splitAtPattern userAgentPattern'
                        |> bots
                        |> List.tryFind (fun x -> x.Name = "*")
                        |> function
                            | None   -> None
                            | Some x ->
                                {x with
                                    Allow    = x.Allow    |> List.map directiveRegexPattern
                                    Disallow = x.Disallow |> List.map directiveRegexPattern
                                } |> Some
            }

    let xRobotsTagHeaders (headers : Header list) =
        headers
        |> List.filter (fun x -> x.Key = "X-Robots-Tag")
        |> List.map (fun x -> x.Value)
        |> List.concat
        |> List.filter (fun x -> userAgentRegex.IsMatch x = false)

    let metaRobotsContent =
        function
            | None      -> []
            | Some html ->
                metaTags' html
                |> List.filter metaRobotsRegex.IsMatch
                |> List.map (fun x -> groupValue (metaContentRegex.Match x) 2)
                |> List.map (fun x -> x.Split([|','|], StringSplitOptions.RemoveEmptyEntries))
                |> List.map List.ofArray
                |> List.concat
                |> List.map (fun x -> x.Trim())

    let robotsInstructions headers html =
        let robotsHeaders = xRobotsTagHeaders headers
        let metasContent = metaRobotsContent html
        let robotsDirectives = List.append robotsHeaders metasContent
        let indexing  =
            matchesPattern robotsDirectives noIndexRegex
            |> function true -> Disallowed | false -> Allowed
        let following =
            matchesPattern robotsDirectives noFollowRegex
            |> function true -> NoFollow | false -> DoFollow
        {Indexing = indexing; Following = following}

    let robotsDirectives headers content permissionOption =
        let directives = robotsInstructions headers content
        match permissionOption with
            | None | Some Allowed -> directives
            | Some Disallowed     -> {directives with Indexing = Disallowed}