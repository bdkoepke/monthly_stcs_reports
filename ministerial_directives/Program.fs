module MinisterialDirectives

open System
open FSharp.Data
open FParsec

type Directive =
    { Country: string
      Url: string
      EffectiveDte: DateOnly
      UpdatedAt: option<DateOnly> }

type ParseError =
    | HtmlLoadError of string
    | MissingElement of string
    | ParserError of string

let pDate: Parser<DateOnly, unit> =
    let isDateChar c =
        Char.IsLetterOrDigit c || c = ',' || Char.IsWhiteSpace c

    many1Satisfy isDateChar
    |>> fun s -> DateOnly.Parse(s.Trim(), System.Globalization.CultureInfo.InvariantCulture)

let pDirectiveText: Parser<DateOnly * string, unit> =
    let isDateChar c = c <> ':'

    many1Satisfy isDateChar
    |>> fun s -> DateOnly.Parse(s.Trim(), System.Globalization.CultureInfo.InvariantCulture)
    .>> pstring ":"
    .>> spaces
    .>>. restOfLine true

let pUpdatedDate: Parser<DateOnly, unit> = pstring "Updated on" .>> spaces >>. pDate

let pDirective (li: HtmlNode) : Parser<Directive, unit> =
    parse {
        let aElements = li.Elements("a")

        if List.isEmpty aElements then
            return! fail "Missing 'a' element in 'li'"
        else
            let a = List.head aElements
            let linkText = a.InnerText().Trim()
            let href = a.AttributeValue("href")

            match run pDirectiveText linkText with
            | Failure(msg, _, _) -> return! fail ("Failed to parse directive text: " + msg)
            | Success((effectiveDate, country), _, _) ->
                let fullText = li.InnerText()

                let updatedDate =
                    match run (charsTillString "Updated on" false 1000 >>. pUpdatedDate) fullText with
                    | Success(d, _, _) -> Some d
                    | Failure _ -> None

                return
                    { Country = country.Trim()
                      Url = "https://fintrac-canafe.canada.ca/obligations/" + href
                      EffectiveDte = effectiveDate
                      UpdatedAt = updatedDate }
    }

let parseDirectives (doc: HtmlDocument) : Result<Directive list, ParseError list> =
    let results =
        doc.CssSelect(".lst-spcd > li") |> List.map (fun li -> run (pDirective li) "")

    match
        List.choose
            (function
            | Failure(msg, _, _) -> Some(ParserError msg)
            | _ -> None)
            results
    with
    | [] ->
        Result.Ok(
            List.choose
                (function
                | Success(d, _, _) -> Some d
                | _ -> None)
                results
        )
    | errors -> Result.Error errors

let extractDirectives (url: string) =
    try
        let doc = url |> HtmlDocument.Load

        match parseDirectives doc with
        | Result.Ok directives -> directives
        | Result.Error(errors: ParseError list) -> failwithf $"Parsing failed with errors: %A{errors}"
    with ex ->
        failwithf $"Failed to load HTML: %s{ex.Message}"

type DirectivesJson =
    JsonProvider<
        """[{"country":"string", "url":"string", "effectiveDate":"2024-01-01", "updatedAt":"2024-01-01"}]""",
        SampleIsList=true
     >

let convertDirectivesToJson (directives: Directive list) : JsonValue =
    let jsonItems =
        directives
        |> List.map (fun d ->
            DirectivesJson.Root(
                country = d.Country,
                url = d.Url,
                effectiveDate = d.EffectiveDte.ToDateTime(TimeOnly.MinValue),
                updatedAt =
                    (match d.UpdatedAt with
                     | Some dt -> dt.ToDateTime(TimeOnly.MinValue)
                     | None -> DateTime.MinValue)
            ))
        |> Array.ofList

    let json = (JsonValue.Array [| for x in jsonItems -> x.JsonValue |])

    let transformProperty (k, v) =
        match k, v with
        | ("effectiveDate" | "updatedAt"), JsonValue.String s ->
            match DateTime.TryParse s with
            | true, dt when dt = DateTime.MinValue && k = "updatedAt" -> k, JsonValue.Null
            | true, dt -> k, JsonValue.String(dt.ToString("yyyy-MM-dd"))
            | _ -> k, v
        | _ -> k, v

    let transformRecord =
        function
        | JsonValue.Record props -> props |> Array.map transformProperty |> JsonValue.Record
        | v -> v

    match json with
    | JsonValue.Array items -> items |> Array.map transformRecord |> JsonValue.Array
    | _ -> json


module Main =
    [<EntryPoint>]
    let main _ =
        let directives =
            "https://fintrac-canafe.canada.ca/obligations/directives-eng"
            |> extractDirectives

        let jsonWithDateOnly = convertDirectivesToJson directives

        System.IO.File.WriteAllText("directives.json", jsonWithDateOnly.ToString())

        0
