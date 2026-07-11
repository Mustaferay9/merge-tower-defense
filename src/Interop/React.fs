/// Minimal hand-written React 18 bindings plus a tiny Feliz-flavoured DSL —
/// only what the Phase 2 HUD needs.
///
/// Real Feliz/Fable.React are nuget packages and thus unavailable in this
/// dotnet-free environment; this module keeps the same shape (view functions
/// returning ReactElement) so swapping to Feliz later is mechanical.
module MergeTowerDefense.Interop.React

open Fable.Core
open Fable.Core.JsInterop

type ReactElement =
    interface
    end

type ReactRoot =
    abstract render: element: ReactElement -> unit

let private react: obj = importAll "react"

[<Emit("$0.createElement($1, $2, ...$3)")>]
let private createElementSpread (reactApi: obj) (tag: obj) (props: obj) (children: obj []) : ReactElement =
    jsNative

[<Import("createRoot", "react-dom/client")>]
let createRoot (container: obj) : ReactRoot = jsNative

let element (tag: string) (props: (string * obj) list) (children: ReactElement list) : ReactElement =
    createElementSpread react tag (createObj props) (children |> List.toArray |> Array.map box)

// --- tiny HTML DSL ---------------------------------------------------------

let div props children = element "div" props children
let span props children = element "span" props children
let p props children = element "p" props children
let button props children = element "button" props children
let h1 props children = element "h1" props children
let h2 props children = element "h2" props children
let h3 props children = element "h3" props children
let br props children = element "br" props children
let img props children = element "img" props children

/// A bare string as a React child.
let str (text: string) : ReactElement = unbox text

/// Renders nothing (for optional branches).
let nothing: ReactElement = unbox null
