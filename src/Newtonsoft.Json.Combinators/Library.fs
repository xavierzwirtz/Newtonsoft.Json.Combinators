namespace Newtonsoft.Json

open Newtonsoft.Json.Linq
open Chessie.ErrorHandling

module Combinators =

    type ParserErrorType<'t> =
    | NoMember of name : string
    | TokenTypeMismatch of expected : JTokenType * actual : JTokenType
    | UnknownCase of string
    | Other of 't
    | SyntaxError of string

    type ParserError<'t> =
        { Path : string list 
          ParserErrorType : ParserErrorType<'t> }

    let printParserError other x =
        let path = x.Path |> String.concat "."
        (match x.ParserErrorType with
        | SyntaxError x -> x
        | NoMember name ->
            sprintf "Expected member \"%s\"" name
        | TokenTypeMismatch(expected, actual) -> 
            sprintf "Expected token type \"%s\" but was \"%s\"" (expected.ToString()) (actual.ToString())
        | UnknownCase x ->
            sprintf "Unkown case \"%s\"" x
        | Other x -> other x)
        + (sprintf " at path \"%s\"" path)

    type ParserState<'a> =  
        { Value : 'a 
          Path : string list }
        member this.copySetValue value =
            { Value = value
              Path = this.Path }

    let error state errorType =
        { Path = state.Path
          ParserErrorType = errorType }

    let newParser value =
        { Value = value
          Path = [] }

    let unknownCase state =
        { Path = state.Path
          ParserErrorType = UnknownCase state.Value }
        |> fail

    // bind
    let (>>=) (x : 'a -> Result<'b, ParserError<_>>) func (data : 'a) =
        match x data with
        | Bad err -> Bad err
        | Ok (x, _) -> x |> func

    // map
    let (|>>) (arg : ParserState<_> -> Result<'a,ParserError<_>>) (func : 'a -> 'b) (data : ParserState<_>) =
        match arg data with
        | Ok(x, _) -> func x |> ok
        | Bad(errs) -> Bad errs

    let map f arg =
        match arg with
        | Ok(x, _) -> f x |> ok
        | Bad(errs) -> Bad errs

    let listMap f (items : 'i seq) =
        let c, r = 
            items |> Seq.fold(fun (err, items) item ->
            if err |> Option.isSome then err, items
            else 
                match f item with
                | Bad errors -> Some errors, []
                | Ok (x, _) -> None, items @ [x])(None, [])

        match c with
        | Some x -> Bad x
        | None -> ok r

    let optionBind func x = 
        match x with
        | None -> ok None
        | Some x -> func x

    let prop name (data : ParserState<JObject>) =
        let x = data.Value.Item name
        if x = null then fail (error data (NoMember name))
        else ok ({ Value = x
                   Path = data.Path @ [name] })

    let tryProp name (data : ParserState<JObject>) =
        let x = data.Value.Item name
        if x = null then (ok None)
        else ok (Some ({ Value = x
                         Path = data.Path @ [name] }))

    let tuple2 x1 x2 data =
        x1 data |> Trial.bind(fun x1 ->
            x2 data |> Trial.bind(fun x2 ->
                    ok(x1, x2)))
 
    let tuple3 x1 x2 x3 data =
        x1 data |> Trial.bind(fun x1 ->
            x2 data |> Trial.bind(fun x2 ->
                x3 data |> Trial.bind(fun x3 ->
                    ok(x1, x2, x3))))

    let tuple4 x1 x2 x3 x4 data =
        x1 data |> Trial.bind(fun x1 ->
            x2 data |> Trial.bind(fun x2 ->
                x3 data |> Trial.bind(fun x3 ->
                    x4 data |> Trial.bind(fun x4 ->
                        ok(x1, x2, x3, x4)))))

    let tuple5 x1 x2 x3 x4 x5 data =
        x1 data |> Trial.bind(fun x1 ->
            x2 data |> Trial.bind(fun x2 ->
                x3 data |> Trial.bind(fun x3 ->
                    x4 data |> Trial.bind(fun x4 ->
                        x5 data |> Trial.bind(fun x5 ->
                            ok(x1, x2, x3, x4, x5))))))

    let tuple6 x1 x2 x3 x4 x5 x6 data =
        x1 data |> Trial.bind(fun x1 ->
            x2 data |> Trial.bind(fun x2 ->
                x3 data |> Trial.bind(fun x3 ->
                    x4 data |> Trial.bind(fun x4 ->
                        x5 data |> Trial.bind(fun x5 ->
                            x6 data |> Trial.bind(fun x6 ->
                                ok(x1, x2, x3, x4, x5, x6)))))))

    let tuple7 x1 x2 x3 x4 x5 x6 x7 data =
        x1 data |> Trial.bind(fun x1 ->
            x2 data |> Trial.bind(fun x2 ->
                x3 data |> Trial.bind(fun x3 ->
                    x4 data |> Trial.bind(fun x4 ->
                        x5 data |> Trial.bind(fun x5 ->
                            x6 data |> Trial.bind(fun x6 ->
                                x7 data |> Trial.bind(fun x7 ->
                                    ok(x1, x2, x3, x4, x5, x6, x7))))))))

    let tuple8 x1 x2 x3 x4 x5 x6 x7 x8 data =
        x1 data |> Trial.bind(fun x1 ->
            x2 data |> Trial.bind(fun x2 ->
                x3 data |> Trial.bind(fun x3 ->
                    x4 data |> Trial.bind(fun x4 ->
                        x5 data |> Trial.bind(fun x5 ->
                            x6 data |> Trial.bind(fun x6 ->
                                x7 data |> Trial.bind(fun x7 ->
                                    x8 data |> Trial.bind(fun x8 ->
                                        ok(x1, x2, x3, x4, x5, x6, x7, x8)))))))))

    let tuple9 x1 x2 x3 x4 x5 x6 x7 x8 x9 data =
        x1 data |> Trial.bind(fun x1 ->
            x2 data |> Trial.bind(fun x2 ->
                x3 data |> Trial.bind(fun x3 ->
                    x4 data |> Trial.bind(fun x4 ->
                        x5 data |> Trial.bind(fun x5 ->
                            x6 data |> Trial.bind(fun x6 ->
                                x7 data |> Trial.bind(fun x7 ->
                                    x8 data |> Trial.bind(fun x8 ->
                                        x9 data |> Trial.bind(fun x9 ->
                                            ok(x1, x2, x3, x4, x5, x6, x7, x8, x9))))))))))

    let tuple10 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 data =
        x1 data |> Trial.bind(fun x1 ->
            x2 data |> Trial.bind(fun x2 ->
                x3 data |> Trial.bind(fun x3 ->
                    x4 data |> Trial.bind(fun x4 ->
                        x5 data |> Trial.bind(fun x5 ->
                            x6 data |> Trial.bind(fun x6 ->
                                x7 data |> Trial.bind(fun x7 ->
                                    x8 data |> Trial.bind(fun x8 ->
                                        x9 data |> Trial.bind(fun x9 ->
                                            x10 data |> Trial.bind(fun x10 ->
                                                ok(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)))))))))))

    let tuple11 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 data =
        x1 data |> Trial.bind(fun x1 ->
            x2 data |> Trial.bind(fun x2 ->
                x3 data |> Trial.bind(fun x3 ->
                    x4 data |> Trial.bind(fun x4 ->
                        x5 data |> Trial.bind(fun x5 ->
                            x6 data |> Trial.bind(fun x6 ->
                                x7 data |> Trial.bind(fun x7 ->
                                    x8 data |> Trial.bind(fun x8 ->
                                        x9 data |> Trial.bind(fun x9 ->
                                            x10 data |> Trial.bind(fun x10 ->
                                                x11 data |> Trial.bind(fun x11 ->
                                                    ok(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11))))))))))))

    let tuple12 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 data =
        x1 data |> Trial.bind(fun x1 ->
            x2 data |> Trial.bind(fun x2 ->
                x3 data |> Trial.bind(fun x3 ->
                    x4 data |> Trial.bind(fun x4 ->
                        x5 data |> Trial.bind(fun x5 ->
                            x6 data |> Trial.bind(fun x6 ->
                                x7 data |> Trial.bind(fun x7 ->
                                    x8 data |> Trial.bind(fun x8 ->
                                        x9 data |> Trial.bind(fun x9 ->
                                            x10 data |> Trial.bind(fun x10 ->
                                                x11 data |> Trial.bind(fun x11 ->
                                                    x12 data |> Trial.bind(fun x12 ->
                                                        ok(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12)))))))))))))

    let tuple13 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 data =
        x1 data |> Trial.bind(fun x1 ->
            x2 data |> Trial.bind(fun x2 ->
                x3 data |> Trial.bind(fun x3 ->
                    x4 data |> Trial.bind(fun x4 ->
                        x5 data |> Trial.bind(fun x5 ->
                            x6 data |> Trial.bind(fun x6 ->
                                x7 data |> Trial.bind(fun x7 ->
                                    x8 data |> Trial.bind(fun x8 ->
                                        x9 data |> Trial.bind(fun x9 ->
                                            x10 data |> Trial.bind(fun x10 ->
                                                x11 data |> Trial.bind(fun x11 ->
                                                    x12 data |> Trial.bind(fun x12 ->
                                                        x13 data |> Trial.bind(fun x13 ->
                                                            ok(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13))))))))))))))

    let tuple14 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 data =
        x1 data |> Trial.bind(fun x1 ->
            x2 data |> Trial.bind(fun x2 ->
                x3 data |> Trial.bind(fun x3 ->
                    x4 data |> Trial.bind(fun x4 ->
                        x5 data |> Trial.bind(fun x5 ->
                            x6 data |> Trial.bind(fun x6 ->
                                x7 data |> Trial.bind(fun x7 ->
                                    x8 data |> Trial.bind(fun x8 ->
                                        x9 data |> Trial.bind(fun x9 ->
                                            x10 data |> Trial.bind(fun x10 ->
                                                x11 data |> Trial.bind(fun x11 ->
                                                    x12 data |> Trial.bind(fun x12 ->
                                                        x13 data |> Trial.bind(fun x13 ->
                                                            x14 data |> Trial.bind(fun x14 ->
                                                                    ok(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14)))))))))))))))

    let tuple15 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 data =
        x1 data |> Trial.bind(fun x1 ->
            x2 data |> Trial.bind(fun x2 ->
                x3 data |> Trial.bind(fun x3 ->
                    x4 data |> Trial.bind(fun x4 ->
                        x5 data |> Trial.bind(fun x5 ->
                            x6 data |> Trial.bind(fun x6 ->
                                x7 data |> Trial.bind(fun x7 ->
                                    x8 data |> Trial.bind(fun x8 ->
                                        x9 data |> Trial.bind(fun x9 ->
                                            x10 data |> Trial.bind(fun x10 ->
                                                x11 data |> Trial.bind(fun x11 ->
                                                    x12 data |> Trial.bind(fun x12 ->
                                                        x13 data |> Trial.bind(fun x13 ->
                                                            x14 data |> Trial.bind(fun x14 ->
                                                                x15 data |> Trial.bind(fun x15 ->
                                                                    ok(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15))))))))))))))))

    let value (x : ParserState<_>) = ok x.Value
    let tryValue (x : ParserState<_> option) = 
        x |> Option.map(fun x -> x.Value) |> ok

    let asStringParser (x : ParserState<JToken>) =
        if x.Value.Type = JTokenType.String then
            x.copySetValue (x.Value.ToObject<string>())
            |> ok
        else fail(error x (TokenTypeMismatch(JTokenType.String, x.Value.Type)))

    let asString x = (asStringParser >>= value) x

    let asIntParser (x : ParserState<JToken>) =
        if x.Value.Type = JTokenType.Integer then
            x.copySetValue (x.Value.ToObject<int>())
            |> ok
        else fail(error x (TokenTypeMismatch(JTokenType.Integer, x.Value.Type)))

    let asInt x = (asIntParser >>= value) x

    let asFloatParser (x : ParserState<JToken>) =
        if x.Value.Type = JTokenType.Float then
            x.copySetValue (x.Value.ToObject<float>())
            |> ok
        else fail(error x (TokenTypeMismatch(JTokenType.Integer, x.Value.Type)))

    let asFloat x = (asFloatParser >>= value) x

    let asArray (x : ParserState<JToken>) =
        if x.Value.Type = JTokenType.Array then
            x.Value.Children() |> Seq.mapi(fun i v ->
                { Value = v
                  Path = x.Path @ ["[" + i.ToString() + "]"] })
            |> ok
        else fail(error x (TokenTypeMismatch(JTokenType.Array, x.Value.Type)))

    let asToken (x : ParserState<JToken>) =
        ok x

    let asObject (x : ParserState<JToken>) =
        if x.Value.Type = JTokenType.Object then
            x.copySetValue (x.Value :?> JObject)
            |> ok
        else fail(error x (TokenTypeMismatch(JTokenType.Object, x.Value.Type)))

    let asObjectValue x = (asObject >>= value) x

    let asUnixTime x = 
        (asInt >>= (fun (x : int) -> 
            let epoch = System.DateTime(1970, 1, 1, 0, 0, 0, System.DateTimeKind.Utc)
            epoch.AddSeconds(float x) |> ok)) x

    let runParser value parser =
        parser (newParser value)
    
    let runParserString value parser =
        let jobject =
            try
                Newtonsoft.Json.Linq.JToken.Parse value |> Choice1Of2
            with
            | :? Newtonsoft.Json.JsonReaderException as ex -> Choice2Of2 ex
        match jobject with
        | Choice1Of2 x -> parser (newParser x)
        | Choice2Of2 ex -> 
            { Path = []
              ParserErrorType = SyntaxError ex.Message }
            |> fail
            
module Serializer =
    let toJObject data =
        let j = JObject()
        for (n, v) in data do
            match v with
            | None -> ignore()
            | Some v ->
                j.Add(n, v)
        j :> JToken
    
    let jbool (x : bool) =
        JValue(x) :> JToken
    let jstring (x : string) =
        JValue(x) :> JToken
    let jint (x : int) =
        JValue(x) :> JToken
    let jarray (items : JToken seq) =
        let j = JArray()
        for item in items do 
            j.Add(item)
        j :> JToken