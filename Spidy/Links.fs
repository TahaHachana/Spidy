namespace Spidy

open System
open System.IO
open System.Net.Http.Headers
open System.Text.RegularExpressions
open Utilities
open Http
open Robots
open Types

module private Links =
    
    /// Returns the href attribute value of the <base> tag if it's used in a HTML document.
    let baseUri htmlOption requestUri =
        match htmlOption with
            | None      -> None
            | Some html ->
                let baseTagMatch = baseTagRegex.Match html
                baseTagMatch.Success |> function
                    | false -> Some requestUri
                    | true  ->
                        let hrefMatch = hrefRegex.Match baseTagMatch.Value
                        match hrefMatch.Success with
                            | false -> Some requestUri
                            | true  ->
                                Uri.TryCreate(hrefMatch.Groups.[2].Value.Trim(), UriKind.Absolute) |> function
                                    | false, _ -> Some requestUri
                                    | true, uri -> Some uri

    let canonicalUri htmlOption baseUriOption depthOption =
        match htmlOption with
         | None -> None
         | Some html ->
                let linkTagMatches = linkTagRegex.Matches html |> Seq.cast<Match> |> Seq.toList
                match linkTagMatches with
                    | []  -> None
                    | _ ->
                        linkTagMatches
                        |> List.map (fun x -> x.Value)
                        |> List.tryFind (fun x ->
                            let relAttribute = relAttributeRegex.Match(x).Value
                            canonicalRegex.IsMatch relAttribute)
                        |> function
                            | None   -> None
                            | Some x ->
                                let hrefAttribute = hrefRegex.Match(x).Groups.[2].Value
                                tryCreateUri hrefAttribute baseUriOption depthOption

    let relAttributeContent anchor =
        groupValue (relAttributeRegex.Match anchor) 2
        |> (fun x -> x.Split([|','|], StringSplitOptions.RemoveEmptyEntries))
        |> List.ofArray
        |> List.map (fun x -> x.Trim())

    /// Scrapes href attributes from <a> tags in an HTML string.
    let scrapeHrefs html =
        anchorRegex.Matches html
        |> Seq.cast<Match>
        |> Seq.toList
        |> List.map (fun x -> x.Value)
        |> List.map (fun x -> x, relAttributeContent x)
        |> List.map (fun (anchor, rel) ->
            let href = groupValue (hrefRegex.Match anchor) 2
            let follow =
                rel
                |> List.tryFind (fun x -> noFollowRegex.IsMatch x)
                |> function Some _ -> NoFollow | None -> DoFollow
            href, follow)

    let partitionUris uris = uris |> List.partition (fun (href, _) -> absoluteUriRegex.IsMatch href)

    let formatRelativeUris relativeHrefs uri baseUri =
        [
            let uri' =
                match baseUri with
                    | None   -> uri
                    | Some x -> x
            for (href : string, follow) in relativeHrefs do
                let isUri, result = Uri.TryCreate(uri', href)
                match isUri with
                    | true  -> yield result.ToString(), follow
                    | false -> ()
        ]

    let removeComments html = commentRegex.Replace(html, "")

    /// Scrapes links having the specified depth from an HTML string.
    let scrapeLinks uri (htmlOption : string option) depthOption baseUri =
        let html' = removeComments htmlOption.Value
        let absoluteUris, relativeUris = scrapeHrefs html' |> partitionUris
        let relativeUris' = formatRelativeUris relativeUris uri baseUri
        let links =
            absoluteUris
            |> List.append relativeUris'
            |> Seq.distinctBy fst
            |> Seq.toList
            |> List.map (fun (url, follow) -> Uri url, follow)
        match depthOption with
            | None       -> links
            | Some depth -> links |> List.filter (fun (uri, _) -> uri.Segments.Length <= depth + 1)
    
    /// Collects links from the given Web page.
    let collectLinks uri isHtml depth baseUri contentOption =
        match isHtml with
            | false -> []
            | true  -> scrapeLinks uri contentOption depth baseUri