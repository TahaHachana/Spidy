namespace Spidy

open System
open System.Net

module Types =

    type Agent<'T> = MailboxProcessor<'T>

    type Bot =
        {
            Name       : string
            CrawlDelay : float option
            Allow      : string list
            Disallow   : string list
        }

    type Follow = DoFollow | NoFollow

    type Header = {Key: string; Value: string list}

    type Permission = Allowed | Disallowed

    type RobotsDirectives =
        {
            Indexing  : Permission
            Following : Follow
        }

    type HttpData =
        {
            RequestUri  : string
            Headers     : Header list
            Robots      : RobotsDirectives
            ContentType : string option
            Content     : string option
            StatusCode  : HttpStatusCode
            SourceUri   : string
            Links       : (Uri * Follow) list
        }

    type Message =
        | Cancel
        | Done
        | Mailbox of Agent<Message>
        | Stop
        | UriData of (Uri * string * Permission option) option

    type MessageAgent = Agent<Message>

    type RogueMode = ON | OFF

    type CrawlerConfig =
        {
            Seeds          : Uri list
            Depth          : int option
            Limit          : int option
            AllowedHosts   : string list option
            RogueMode      : RogueMode
            HttpDataFunc   : HttpData -> Async<unit>
            CompletionFunc : Async<unit>
        }