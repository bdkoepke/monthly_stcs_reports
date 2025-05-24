module Entity

open System
open FSharp.Data

[<Literal>]
let SOR_2002_284_URI = "https://laws-lois.justice.gc.ca/eng/XML/SOR-2002-284.xml"

type RegulationsEstablishingAListOfEntities = XmlProvider<SOR_2002_284_URI>

[<Literal>]
let SOR_2017_233_URI = "https://laws-lois.justice.gc.ca/eng/XML/SOR-2017-233.xml"

type JusticeForVictimsOfCorruptForeignOfficialsAct = XmlProvider<SOR_2017_233_URI>

[<Literal>]
let LSTD_NTTS_URI = "https://www.publicsafety.gc.ca/cnt/_xml/lstd-ntts-eng.xml"

type CurrentlyListedEntities = XmlProvider<LSTD_NTTS_URI>

[<Literal>]
let SEMA_LMES_URI = "https://www.international.gc.ca/world-monde/assets/office_docs/international_relations-relations_internationales/sanctions/sema-lmes.xml"

type ConsolidatedCanadianAutonomousSanctionsList = XmlProvider<SEMA_LMES_URI>

type EntitySource =
    | SOR_2002_284
    | SOR_2017_233
    | SEMA_LMES_URI

type Entity =
    { InForceStartDate: DateOnly
      Source: EntitySource
      Id: int
      Label: option<int>
      Text: string }

let entityToName (entitySource: EntitySource) (id: int) (label: option<int>) =
    let value = $"{entitySource}|{id}"

    match label with
    | Some label -> $"{value}|{label}"
    | None -> value

let loadEntities () =
    RegulationsEstablishingAListOfEntities.Load(SOR_2002_284_URI).Body.Sections
    |> Array.choose _.List
    |> Array.collect _.Items
    |> Array.choose (fun x ->
        x.Text.Value
        |> Option.map (fun y ->
            { InForceStartDate = x.InforceStartDate |> DateOnly.FromDateTime
              Source = EntitySource.SOR_2002_284
              Id = x.Id
              Label = None
              Text = y }))

let loadCorruptOfficials () =
    let schedule =
        JusticeForVictimsOfCorruptForeignOfficialsAct.Load(SOR_2017_233_URI).Schedules
        |> Array.filter (fun x -> x.ScheduleFormHeading.TitleText = "Foreign Nationals")
        |> Array.exactlyOne

    schedule.List.Value.Items
    |> Array.map (fun x ->
        { InForceStartDate = x.InforceStartDate |> DateOnly.FromDateTime
          Source = EntitySource.SOR_2017_233
          Id = x.Id
          Label = Some x.Label
          Text = x.Text })
