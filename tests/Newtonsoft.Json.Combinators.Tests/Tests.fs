module Newtonsoft.Json.Combinators.Tests

open Chessie.ErrorHandling
open Newtonsoft.Json.Combinators
open NUnit.Framework

type BasicRecord =
    { String : string
      Int : int }
with
    static member FromJson =
        tuple2
            (prop "String" >>= asString)
            (prop "Int" >>= asInt)
        |>> (fun (string, int) ->
            { String = string
              Int = int })

let (==) (actual: Result<'t, _>) (expected : 't) =
    let actual =  actual |> Trial.returnOrFail
    Assert.AreEqual(expected, actual)

[<Test>]
let ``basic record`` () =
    
    runParserStringObject "{\"String\": \"foo\", \"Int\": 5}" BasicRecord.FromJson
    == { String = "foo"
         Int = 5 }
