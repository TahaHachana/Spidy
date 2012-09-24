namespace Spidy

#if INTERACTIVE
#r "System.Net.Http"
#endif

open System
open System.Net.Http
open System.Net.Http.Headers
open Types
open Utilities

module internal Http =

    let httpHeaders (responseHeaders : HttpResponseHeaders) (contentHeaders : HttpContentHeaders) =
        [
            for x in responseHeaders do
                yield constructHeader x.Key x.Value
            for x in contentHeaders do
                yield constructHeader x.Key x.Value
        ]

    let httpHeaders' (responseHeaders : System.Net.WebHeaderCollection) =
        [
            let length = responseHeaders.Count - 1
            for x in 0 .. length do
                let key = responseHeaders.GetKey x
                let value = responseHeaders.Get x
                yield constructHeader key [value]
        ]

    let linkCanonicalHeader headers baseUriOption depthOption =
        headers
        |> List.filter (fun x -> x.Key = "Link")
        |> List.tryFind (fun x -> x.Value |> List.exists (fun y -> canonicalRegex.IsMatch y))
        |> function
            | None        -> None
            | Some header ->
                let headerValue = header.Value |> List.find (fun x -> canonicalRegex.IsMatch x)
                let ltgtMatch = ltgtRegex.Match headerValue
                match ltgtMatch.Success with
                    | false -> None
                    | true  ->
                        let matchValue = groupValue ltgtMatch 1
                        tryCreateUri matchValue baseUriOption depthOption
    
    /// Initializes a HttpClient instance and adds a user-agent request header. 
    let httpClient () =
        let client = new HttpClient()
        client.DefaultRequestHeaders.Add("User-Agent", ie9UserAgent)
        client.MaxResponseContentBufferSize <- int64 1073741824
        client

    /// Returns an asynchronous computation that will wait for the task of
    /// sending an async GET request to a Uri to complete.
    let awaitHttpResponse (client : HttpClient) (requestUrl : string) =
        client.GetAsync requestUrl
        |> Async.AwaitTask

    /// Returns an asynchronous computation that will wait for the task of
    /// reading the content of a HTTP response as a string to complete.
    let awaitReadAsString (httpContent : HttpContent) =
        httpContent.ReadAsStringAsync()
        |> Async.AwaitTask
   
    /// Downloads the content of a Web page.
    let fetchUrl requestUrl =
        async {
            try
                use client = httpClient ()
                let! response = awaitHttpResponse client requestUrl
                let response' = response.EnsureSuccessStatusCode()
                let httpContent = response'.Content
                let! html = awaitReadAsString httpContent
                return Some html
            with _ -> return None
        }

    /// Reads HttpContent as a string if its content type is HTML.        
    let readContent isHtml httpContent =
        async { 
            try
                match isHtml with
                    | false -> return None
                    | true  ->
                        let! content = awaitReadAsString httpContent
                        return Some content
            with _ -> return None
        }