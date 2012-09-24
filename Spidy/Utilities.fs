namespace Spidy

open System
open System.Text.RegularExpressions
open Types

module internal Utilities =

    /// Initializes a Header instance.
    let constructHeader key (value : string seq) = {Key = key; Value = Seq.toList value}

    /// Constructs a HttpData instance.
    let constructHttpData requestUri headers robotsDirectives contentType content statusCode sourceUri links =
        {
            RequestUri  = requestUri
            Headers     = headers
            Robots      = robotsDirectives
            ContentType = contentType
            Content     = content
            StatusCode  = statusCode
            SourceUri   = sourceUri
            Links       = links
        }

    // IE9 user-agent string.
    let ie9UserAgent = "Mozilla/5.0 (Windows; U; MSIE 9.0; Windows NT 9.0; en-US)"

    /// Compiles a regex pattern.
    let compileRegex pattern = Regex(pattern, RegexOptions.Compiled)

    let absoluteUriPattern    = "(?i)^https?://[^\"]*"
    let anchorPattern         = "(?i)<a.+?>"
    let baseTagPattern        = "<base.+?>"
    let canonicalPattern      = "(?i)canonical"
    let commentPattern        = "(?s)<!--.*?--\s*>"
    let crawlDelayPattern     = "(?i)^crawl-delay:(.+)"
    let hrefPattern           = "(?i) href\\s*=\\s*(\"|')((?!#.*|mailto:|location\.|javascript:)/*[^\"'\#]+)(\"|'|\#)"
    let htmlContentPattern    = "text/html"
    let linkTagPattern        = "<link.+?>"
    let ltgtPattern           = "<([^>]+)>"
    let metaContentPattern    = "content=(\"|')(.+)(\"|')"
    let metaRobotsPattern     = "(?i)robots"
    let metaTagPattern        = "(?i)<meta.+?>"
    let noFollowPattern       = "(?i)nofollow"
    let noIndexPattern        = "(?i)noindex"
    let relAttributePattern   = "rel=(\"|')(.*?)(\"|')"
    let robotsAllowPattern    = "(?i)^allow:(.+)"
    let robotsCommentPattern  = "^#"
    let robotsDisallowPattern = "(?i)^disallow:(.+)"
    let userAgentPattern      = ".+?:"
    let userAgentPattern'     = "(?i)^user-agent"
    let userAgentPattern''    = "(?i)^user-agent:(.+)"

    let absoluteUriRegex    = compileRegex absoluteUriPattern
    let anchorRegex         = compileRegex anchorPattern
    let baseTagRegex        = compileRegex baseTagPattern
    let canonicalRegex      = compileRegex canonicalPattern
    let commentRegex        = compileRegex commentPattern
    let crawlDelayRegex     = compileRegex crawlDelayPattern
    let hrefRegex           = compileRegex hrefPattern
    let htmlContentRegex    = compileRegex htmlContentPattern
    let linkTagRegex        = compileRegex linkTagPattern
    let ltgtRegex           = compileRegex ltgtPattern
    let metaContentRegex    = compileRegex metaContentPattern
    let metaRobotsRegex     = compileRegex metaRobotsPattern
    let metaTagRegex        = compileRegex metaTagPattern
    let noFollowRegex       = compileRegex noFollowPattern
    let noIndexRegex        = compileRegex noIndexPattern
    let relAttributeRegex   = compileRegex relAttributePattern
    let robotsAllowRegex    = compileRegex robotsAllowPattern
    let robotsCommentRegex  = compileRegex robotsCommentPattern
    let robotsDisallowRegex = compileRegex robotsDisallowPattern
    let userAgentRegex      = compileRegex userAgentPattern
    let userAgentRegex'     = compileRegex userAgentPattern''

    let splitAtNewline (str : string) =
        str.Split(Environment.NewLine.ToCharArray(), StringSplitOptions.RemoveEmptyEntries)

    /// Splits a string list at elements that satisfy a regex pattern.
    let splitAtPattern pattern lst =
        let strLst = List.empty
        let regex  = compileRegex pattern
        let rec split lst acc b strLst =
            match lst with
                | h :: t ->
                    match regex.IsMatch h with
                        | true ->
                            match b with
                                | true -> split t (h :: acc) false strLst
                                | false ->
                                    let strLst' = List.rev acc :: strLst
                                    split t [h] true strLst'
                        | false -> split t (h :: acc) false strLst
                | [] -> List.rev acc :: strLst
        split lst [] true strLst

    /// Attempts to match a string with a regex.
    let matchOption (regex : Regex) str =
        let regexMatch = regex.Match str
        regexMatch.Success |> function
            | false -> None
            | true  -> Some regexMatch

    /// Returns the trimmed value of a group in a match object.
    let groupValue (regexMatch : Match) (idx : int) = regexMatch.Groups.[idx].Value.Trim()    

    /// Matches a sting with a regex and returns a sequence of Match objects.
    let regexMatches (regex : Regex) str = regex.Matches str |> Seq.cast<Match>

    /// Matches the meta tag pattern in an HTML string and
    /// returns the value of each successful match.
    let metaTags' html =
        regexMatches metaTagRegex html
        |> Seq.toList
        |> List.map (fun x -> x.Value)

    /// Attempts to find a regex match in a string list.
    let matchesPattern lst (regex : Regex) =
        lst
        |> List.tryFind regex.IsMatch
        |> function None -> false | _ -> true

    let dispose (agent : MailboxProcessor<'T>) = (agent :> IDisposable).Dispose()

    let tryCreateUri uriString baseUriOption depthOption =
        Uri.TryCreate(uriString, UriKind.Absolute)
        |> function
            | false, _ ->
                match baseUriOption with
                    | None         -> None
                    | Some baseUri ->
                        let uri' = Uri.TryCreate(baseUri, uriString)
                        match uri' with
                            | false, _ -> None
                            | true, x  -> Some x
            | true, x -> Some x
            |> function
                | None   -> None
                | Some x ->
                    match depthOption with
                        | None       -> Some x
                        | Some depth -> if x.Segments.Length <= depth + 1 then Some x else None
            |> function
                | None -> None
                | x ->
                    match baseUriOption with
                        | None -> x
                        | Some baseUri -> if baseUri = x.Value then None else x