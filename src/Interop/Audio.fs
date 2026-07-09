/// Procedural sound effects through the Web Audio API — no external audio
/// files required. Each effect is a short oscillator burst shaped by a gain
/// envelope; tower types get different timbres, merges get a rising arpeggio,
/// kills get a low thud, etc.
///
/// The module initialises lazily on the first user interaction (browsers
/// require a gesture before playing audio). All functions are fire-and-forget
/// and silently no-op when the AudioContext is unavailable.
module MergeTowerDefense.Interop.Audio

open Fable.Core
open Fable.Core.JsInterop

// ---------------------------------------------------------------------------
// Minimal Web Audio API bindings
// ---------------------------------------------------------------------------

type AudioContext =
    abstract currentTime: float
    abstract createOscillator: unit -> obj
    abstract createGain: unit -> obj
    abstract destination: obj
    abstract resume: unit -> obj   // returns Promise

[<Emit("typeof AudioContext !== 'undefined' ? new AudioContext() : null")>]
let private tryCreateContext () : AudioContext option = jsNative

let mutable private ctx: AudioContext option = None

/// Must be called inside a user gesture (pointerdown / click) to unlock audio.
let ensureContext () : unit =
    match ctx with
    | Some c -> c.resume () |> ignore
    | None ->
        ctx <- tryCreateContext ()
        match ctx with
        | Some c -> c.resume () |> ignore
        | None -> ()

// ---------------------------------------------------------------------------
// Internal helpers
// ---------------------------------------------------------------------------

let private osc (freq: float) (oscType: string) (duration: float) (volume: float) : unit =
    match ctx with
    | None -> ()
    | Some c ->
        let o = c.createOscillator ()
        let g = c.createGain ()
        let t = c.currentTime
        o?``type`` <- oscType
        o?frequency?value <- freq
        g?gain?setValueAtTime (volume, t)       |> ignore
        g?gain?linearRampToValueAtTime (0.0, t + duration) |> ignore
        o?connect (g)            |> ignore
        g?connect (c.destination)|> ignore
        o?start (t)              |> ignore
        o?stop (t + duration)    |> ignore

let private oscSweep (f1: float) (f2: float) (oscType: string) (duration: float) (volume: float) : unit =
    match ctx with
    | None -> ()
    | Some c ->
        let o = c.createOscillator ()
        let g = c.createGain ()
        let t = c.currentTime
        o?``type`` <- oscType
        o?frequency?setValueAtTime (f1, t) |> ignore
        o?frequency?linearRampToValueAtTime (f2, t + duration) |> ignore
        g?gain?setValueAtTime (volume, t)  |> ignore
        g?gain?linearRampToValueAtTime (0.0, t + duration) |> ignore
        o?connect (g)            |> ignore
        g?connect (c.destination)|> ignore
        o?start (t)              |> ignore
        o?stop (t + duration)    |> ignore

// ---------------------------------------------------------------------------
// Public sound effect functions
// ---------------------------------------------------------------------------

/// Short percussive blip — different pitch per tower type.
let playShootArcher () = osc 880.0 "square" 0.06 0.08
let playShootCannon () = osc 220.0 "sawtooth" 0.12 0.12
let playShootFrost  () = osc 1200.0 "sine" 0.08 0.07

/// Rising arpeggio for a successful merge.
let playMerge () =
    osc 440.0 "triangle" 0.1 0.10
    osc 554.0 "triangle" 0.1 0.10
    osc 659.0 "triangle" 0.12 0.10

/// Low thud for an enemy kill.
let playKill () =
    oscSweep 300.0 60.0 "sine" 0.15 0.15

/// Alert siren for wave start.
let playWaveStart () =
    oscSweep 600.0 900.0 "square" 0.25 0.08
    oscSweep 900.0 600.0 "square" 0.25 0.08

/// Descending triad for game over.
let playGameOver () =
    osc 440.0 "sawtooth" 0.3 0.12
    osc 349.0 "sawtooth" 0.3 0.12
    osc 261.0 "sawtooth" 0.5 0.12

/// Register ka-ching for buying.
let playBuy () = osc 1046.0 "sine" 0.05 0.08

/// Softer reverse ching for selling.
let playSell () = oscSweep 1046.0 523.0 "sine" 0.08 0.07
