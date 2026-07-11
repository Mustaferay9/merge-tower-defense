/// The three DOM touchpoints the composition root needs. Kept deliberately
/// tiny; everything else goes through Pixi or React.
module MergeTowerDefense.Interop.Dom

open Fable.Core

[<Emit("document.getElementById($0)")>]
let getElementById (id: string) : obj = jsNative

[<Emit("$0.appendChild($1)")>]
let appendChild (parent: obj) (child: obj) : unit = jsNative

[<Emit("globalThis")>]
let globalThis: obj = jsNative

[<Emit("localStorage.getItem($0)")>]
let getItem (key: string) : string option = jsNative

[<Emit("localStorage.setItem($0, $1)")>]
let setItem (key: string) (value: string) : unit = jsNative

[<Emit("$0.innerHTML = $1")>]
let setInnerHTML (element: obj) (html: string) : unit = jsNative
