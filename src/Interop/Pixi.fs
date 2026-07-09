/// Minimal hand-written PixiJS v7 bindings — only the surface Phase 2 uses.
///
/// Written against Fable.Core (bundled with the Fable toolchain) instead of
/// a nuget binding package so the project builds in restricted environments
/// without the dotnet SDK. Interop code is confined to src/Interop and never
/// leaks into the pure core (Shared/State/Ui).
module MergeTowerDefense.Interop.Pixi

open Fable.Core
open Fable.Core.JsInterop

type Point =
    abstract x: float with get, set
    abstract y: float with get, set

/// Base display object surface (Container in Pixi terms). Pixi's fluent API
/// returns `this`, which the bindings surface where chaining helps.
type Container =
    abstract addChild: child: Container -> Container
    abstract removeChildren: unit -> unit
    abstract position: Point
    abstract eventMode: string with get, set
    abstract hitArea: obj with get, set
    abstract cursor: string with get, set
    abstract on: eventName: string * handler: (obj -> unit) -> Container

type Graphics =
    inherit Container
    abstract clear: unit -> Graphics
    abstract beginFill: color: int * alpha: float -> Graphics
    abstract endFill: unit -> Graphics
    abstract lineStyle: width: float * color: int * alpha: float -> Graphics
    abstract drawRect: x: float * y: float * width: float * height: float -> Graphics
    abstract drawRoundedRect: x: float * y: float * width: float * height: float * radius: float -> Graphics
    abstract drawCircle: x: float * y: float * radius: float -> Graphics
    abstract drawEllipse: x: float * y: float * width: float * height: float -> Graphics
    /// Flat [x1; y1; x2; y2; ...] vertex list. Typed as obj[] on purpose:
    /// Fable compiles float[] to a Float64Array, which Pixi's polygon
    /// parsing silently rejects — obj[] stays a plain JS array.
    abstract drawPolygon: points: obj [] -> Graphics
    abstract moveTo: x: float * y: float -> Graphics
    abstract lineTo: x: float * y: float -> Graphics

type Ticker =
    abstract deltaMS: float
    abstract maxFPS: float with get, set
    abstract add: callback: (float -> unit) -> Ticker

type Application =
    abstract stage: Container
    /// The HTMLCanvasElement Pixi renders into.
    abstract view: obj
    abstract ticker: Ticker

let private pixi: obj = importAll "pixi.js"

let createApplication (options: (string * obj) list) : Application =
    createNew pixi?Application (createObj options) |> unbox

let createContainer () : Container = createNew pixi?Container () |> unbox

let createGraphics () : Graphics = createNew pixi?Graphics () |> unbox

let createRectangle (x: float) (y: float) (width: float) (height: float) : obj =
    createNew pixi?Rectangle (x, y, width, height)

/// Canvas-space pointer position of a federated pointer event.
let pointerPosition (event: obj) : float * float =
    !!event?``global``?x, !!event?``global``?y
