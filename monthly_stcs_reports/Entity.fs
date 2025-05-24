module Entity

open System
open FSharp.Data

[<Literal>]
let SOR_2002_284_URI = "https://laws-lois.justice.gc.ca/eng/XML/SOR-2002-284.xml"

type CriminalCodeEntities = XmlProvider<SOR_2002_284_URI>

[<Literal>]
let SOR_2017_233_URI = "https://laws-lois.justice.gc.ca/eng/XML/SOR-2017-233.xml"

type CorruptForeignOfficialsAct = XmlProvider<SOR_2017_233_URI>

[<Literal>]
let LSTD_NTTS_URI = "https://www.publicsafety.gc.ca/cnt/_xml/lstd-ntts-eng.xml"

type AntiTerrorismAct = XmlProvider<LSTD_NTTS_URI>

[<Literal>]
let SEMA_LMES_URI =
    "https://www.international.gc.ca/world-monde/assets/office_docs/international_relations-relations_internationales/sanctions/sema-lmes.xml"

type SpecialEconomicMeasuresAct = XmlProvider<SEMA_LMES_URI>

[<Literal>]
let CONSOLIDATED_URI =
    "https://scsanctions.un.org/resources/xml/en/consolidated.xml"

type UnitedNationsAct = XmlProvider<CONSOLIDATED_URI>

[<Literal>]
let SOR_2017_204_URI = "https://lois-laws.justice.gc.ca/eng/XML/SOR-2017-204.xml"

type SpecialEconomicMeasuresVenezuelaAct = XmlProvider<SOR_2017_204_URI>

type EntitySource =
    | CriminalCode
    | CorruptForeignOfficials
    | AutonomousSanctions
    | AntiTerrorism
    | UNSecurityCouncil
    | VenezuelaAutonomousSanctions

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

let loadCriminalCode () =
    CriminalCodeEntities.Load(SOR_2002_284_URI).Body.Sections
    |> Array.choose _.List
    |> Array.collect _.Items
    |> Array.choose (fun x ->
        x.Text.Value
        |> Option.map (fun y ->
            { InForceStartDate = x.InforceStartDate |> DateOnly.FromDateTime
              Source = EntitySource.CriminalCode
              Id = x.Id
              Label = None
              Text = y }))

let loadCorruptOfficials () =
    let schedule =
        CorruptForeignOfficialsAct.Load(SOR_2017_233_URI).Schedules
        |> Array.filter (fun x -> x.ScheduleFormHeading.TitleText = "Foreign Nationals")
        |> Array.exactlyOne

    schedule.List.Value.Items
    |> Array.map (fun x ->
        { InForceStartDate = x.InforceStartDate |> DateOnly.FromDateTime
          Source = EntitySource.CorruptForeignOfficials
          Id = x.Id
          Label = Some x.Label
          Text = x.Text })

let loadAntiTerrorism () =
    AntiTerrorismAct.Load(LSTD_NTTS_URI).Entries
    |> Array.choose (fun x ->
        match x.Id.DateTime with
        | Some _ -> None
        | None ->
            { InForceStartDate = x.Published |> DateOnly.FromDateTime
              Source = EntitySource.AntiTerrorism
              Id = x.Id.Number.Value
              Label = None
              Text = String.concat "\n" [ x.Title; x.Summary; x.Content.Value.Value ] }
            |> Some)


let loadVenezuelaMeasures () =
    SpecialEconomicMeasuresVenezuelaAct.Load(SOR_2017_204_URI).Schedules
    |> Array.choose _.List
    |> Array.collect _.Items
    |> Array.filter _.Text.Value.IsSome
    |> Array.map (fun x ->
        { InForceStartDate = x.InforceStartDate |> DateOnly.FromDateTime
          Source = EntitySource.VenezuelaAutonomousSanctions
          Id = x.Id
          Label = Some x.Label
          Text = x.Text.Value.Value })

let loadAutonomousSanctions () =
    SpecialEconomicMeasuresAct.Load(SEMA_LMES_URI).Records
    |> Array.map (fun x ->
        { InForceStartDate = x.DateOfListing |> DateOnly.FromDateTime
          Source = EntitySource.AutonomousSanctions
          Id = x.Item
          Label = None
          Text = String.concat " " ([ x.GivenName; x.LastName ] |> List.choose id) })

let loadUnitedNations () =
    UnitedNationsAct.Load(CONSOLIDATED_URI).Individuals
    |> Array.map (fun x ->
        let aliases =
            match (x.IndividualAlias |> Array.choose _.AliasName) with
            | [||] -> None
            | xs -> Some(xs |> String.concat "\n")

        let text =
            [ [ Some x.FirstName; x.SecondName; x.ThirdName; x.FourthName ]
              |> List.choose id
              |> String.concat " "
              |> Some
              aliases
              x.Title |> Option.map (_.Values >> String.concat " ")
              x.Designation |> Option.map (_.Values >> String.concat " ") ]

        { InForceStartDate = x.ListedOn |> DateOnly.FromDateTime
          Source = EntitySource.UNSecurityCouncil
          Id = x.Dataid
          Label = None
          Text = String.concat "\n" (text |> List.choose id) })
