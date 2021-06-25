namespace LightSolutionManager.Collections

open System.Collections.Generic

type OrderedMap<'key, 'value when 'key : comparison> private (keys, map) =
    let _map : Map<'key, 'value> = map
    let _keys = keys

    new () = new OrderedMap<_,_>([], Map.empty)

    member _.Keys : 'key list = _keys
    member self.Values : 'value seq = self.GetEnumerable() |> Seq.map ((|KeyValue|) >> snd)

    member _.TryGetValue (key, value : byref<_>) = _map.TryGetValue(key, &value)
    member _.ContainsKey key = _map.ContainsKey key
    member _.TryFind key = _map.TryFind key

    member _.Item with get key = _map.[key]

    member _.AddOrUpdate (key, value) =
        let map = Map.add key value _map
        let keys = if List.contains key _keys then _keys else _keys @ [key]
        OrderedMap(keys, map)

    member _.Remove key =
        let map = Map.remove key _map
        let keys = List.except [key] _keys
        OrderedMap(keys, map)

    member private _.GetEnumerable() = seq { for key in _keys -> KeyValuePair(key, _map.[key]) }

    member self.GetEnumerator() = self.GetEnumerable().GetEnumerator()

    interface IEnumerable<KeyValuePair<'key, 'value>> with
        member self.GetEnumerator () = self.GetEnumerator()
        member self.GetEnumerator () = self.GetEnumerator() :> System.Collections.IEnumerator

    interface IReadOnlyDictionary<'key, 'value> with
        member self.Item with get key = self.[key]
        member self.Keys with get () = upcast self.Keys
        member self.Values with get () = self.Values
        member self.ContainsKey key = self.ContainsKey key
        member _.Count with get () = List.length _keys
        member self.TryGetValue(key, value) = self.TryGetValue (key, &value)

[<RequireQualifiedAccess>]
module OrderedMap =

    let addOrUpdate key value (map: OrderedMap<_,_>) = map.AddOrUpdate(key, value)
    let remove key (map: OrderedMap<_,_>) = map.Remove key
    let keys (map: OrderedMap<_,_>) = map.Keys
    let get key (map: OrderedMap<_,_>) = map.[key]

    let ofSeq seq =
        let mutable map = OrderedMap()
        for el in seq do
            map <- map.AddOrUpdate el
        map
