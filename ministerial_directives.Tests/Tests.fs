module Tests

open System
open Xunit
open MinisterialDirectives
open FSharp.Data

[<Fact>]
let ``Extract directives returns expected current values using structural equality`` () =
    let actualDirectives =
        "https://fintrac-canafe.canada.ca/obligations/directives-eng"
        |> extractDirectives

    let expectedDirectives =
        [ { Country = "Russia"
            Url = "https://fintrac-canafe.canada.ca/obligations/dir-rus-eng"
            EffectiveDte = DateOnly(2024, 2, 24)
            UpdatedAt = Some(DateOnly(2025, 3, 22)) }
          { Country = "Islamic Republic of Iran"
            Url = "https://fintrac-canafe.canada.ca/obligations/dir-iri-eng"
            EffectiveDte = DateOnly(2020, 7, 25)
            UpdatedAt = Some(DateOnly(2025, 11, 17)) }
          { Country = "Democratic People’s Republic of Korea (DPRK)"
            Url = "https://fintrac-canafe.canada.ca/obligations/dir-dprk-eng"
            EffectiveDte = DateOnly(2017, 12, 9)
            UpdatedAt = Some(DateOnly(2025, 3, 22)) } ]

    Assert.Equal<Directive list>(expectedDirectives, actualDirectives)

[<Fact>]
let ``convertDirectivesToJson handles dates and null updatedAt correctly`` () =
    let directives =
        [ { Country = "Test"
            Url = "http://test.com"
            EffectiveDte = DateOnly(2024, 1, 1)
            UpdatedAt = None }
          { Country = "Test2"
            Url = "http://test2.com"
            EffectiveDte = DateOnly(2024, 5, 20)
            UpdatedAt = Some(DateOnly(2024, 6, 1)) } ]

    let actualJson = convertDirectivesToJson directives
    let expectedJson = JsonValue.Load("expected.json")

    Assert.Equal(expectedJson, actualJson)
