module Program

open System.IO
open Entity
open FSharp.Data
open Lucene.Net.Documents
open Lucene.Net.QueryParsers
open Lucene.Net.Search
open Lucene.Net.Store
open Lucene.Net.Util
open Search

type KnownMatch = JsonProvider<"known_matches.json", SampleIsList=true>

let findAllMatches (entities: array<string>) (documents: array<Document>) =
    let version = Version.LUCENE_30

    using (new RAMDirectory()) (fun directory ->
        using (createAndIndexAnalyzer version directory documents) (fun analyzer ->
            using (new IndexSearcher(directory, true)) (fun searcher ->
                entities
                |> Array.choose (fun search ->
                    let parser = QueryParser(version, "entity", analyzer)
                    let query = parser.Parse(search)
                    let hits = searcher.Search(query, 10).ScoreDocs

                    match hits with
                    | [||] -> None
                    | _ ->
                        Some
                            { Name = search
                              Hits =
                                hits
                                |> Array.map (fun scoreDoc ->
                                    { ScoreDoc = scoreDoc
                                      Document = searcher.Doc(scoreDoc.Doc) }) }))))

[<EntryPoint>]
let main (args: array<string>) =
    let entitiesFilePath, knownMatchesFilePath =
        match args with
        | [| a; b |] -> (a, Some b)
        | [| a |] -> (a, None)
        | _ -> failwith "Please provide either an entities.txt list and a known_matches.json or just entities.txt."

    let documentTexts =
        Array.concat
            [ (loadCriminalCode ())
              (loadCorruptOfficials ())
              (loadAntiTerrorism ())
              (loadAutonomousSanctions ())
              (loadUnitedNations ())
              (loadVenezuelaMeasures ()) ]

    let documents =
        documentTexts
        |> Array.map (fun x ->
            let doc = Document()

            [ Field("name", entityToName x.Source x.Id x.Label, Field.Store.YES, Field.Index.ANALYZED)
              Field("entity", x.Text, Field.Store.YES, Field.Index.ANALYZED) ]
            |> List.iter doc.Add

            doc)

    let entities = File.ReadAllLines(entitiesFilePath)
    let matches = findAllMatches entities documents

    let knownMatches =
        match knownMatchesFilePath with
        | None -> Map.empty
        | Some knownMatches ->
            knownMatches
            |> File.ReadAllText
            |> KnownMatch.ParseList
            |> Array.map (fun x ->
                x.Name,
                x.Documents
                |> Array.map (fun x -> entityToName (Union.fromString<EntitySource>(x.Source).Value) x.Id x.Label)
                |> Set.ofArray)
            |> Map.ofArray

    let reviewMatches =
        matches
        |> Array.choose (fun x ->
            if knownMatches.ContainsKey x.Name then
                let names = knownMatches[x.Name]

                let reviewHits =
                    x.Hits
                    |> Array.filter (fun hit -> hit.Document.GetField("name").StringValue |> names.Contains |> not)

                match reviewHits with
                | [||] -> None
                | _ -> { x with Hits = reviewHits } |> Some
            else
                Some x)

    match reviewMatches with
    | [||] -> printfn "No matches found."
    | _ -> printfn $"Review matches: %A{reviewMatches}"

    0
