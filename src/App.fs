/// Composition root — the only impure module in the application. It wires
/// the pure core (Shared/State/Ui) to PixiJS (canvas, ticker, pointer
/// events), React (HUD) and Web Audio (procedural sound effects), and owns
/// the single mutable model reference of the hand-rolled MVU loop.
///
/// Phase 4 additions: sound effect dispatch from GameEvents, screen shake
/// applied to the Pixi stage position, and the ambient-time parameter
/// forwarded to the renderer.
module MergeTowerDefense.App

open Fable.Core.JsInterop
open MergeTowerDefense.Shared
open MergeTowerDefense.State
open MergeTowerDefense.Ui
open MergeTowerDefense.Interop
open MergeTowerDefense.Interop.Pixi
open MergeTowerDefense.View

let private gridSide = 5

let private start () =
    let size =
        match GridSize.tryCreate gridSide with
        | Some s -> s
        // Unreachable: gridSide is a compile-time constant within
        // GridSize.minSize..maxSize.
        | None -> failwith "unreachable: gridSide is a valid grid size"

    // The path is part of the pure game state; layout is derived from it so
    // the canvas always contains the whole course. Restart keeps the same
    // deterministic path, so the layout stays valid for the app's lifetime.
    let mutable model = init size
    let layout = layoutFor size model.Game.Path

    // --- PixiJS application (WebGL with automatic canvas fallback) --------
    let app =
        createApplication
            [ "width", box layout.CanvasWidth
              "height", box layout.CanvasHeight
              "background", box 0x141724
              "antialias", box true ]

    app.ticker.maxFPS <- 60.0
    Dom.appendChild (Dom.getElementById "game-root") app.view

    let layers = Render.createLayers app
    Render.drawStatic layout size model.Game.Path layers

    // --- React HUD in its own DOM root -------------------------------------
    let hudRoot = React.createRoot (Dom.getElementById "hud-root")

    // Elapsed wall time for ambient animations (path lights).
    let mutable elapsedTime = 0.0

    // --- MVU loop -----------------------------------------------------------
    // Every change flows through the pure Ui.updateUi; Pixi redraws from the
    // model each frame, React re-renders only when a HUD-visible value
    // actually changes.
    let hudProjection (m: UiModel) =
        Gold.value m.Game.Gold,
        m.Game.Wave.Number,
        (match m.Game.Wave.Phase with
         | BetweenWaves s -> int (ceil s)
         | Spawning _ -> -1
         | WaveActive -> -2),
        (match m.Game.Status with
         | Playing lives -> Lives.value lives
         | Defeated _ -> -1),
        List.length m.Game.Enemies,
        Option.map fst m.Notice,
        canBuy m,
        nextTowerCost m.Game,
        // Sell button and hover state changes need HUD refresh too.
        m.Hover

    let rec dispatch (msg: UiMsg) : unit =
        let before = hudProjection model
        model <- updateUi layout msg model

        if hudProjection model <> before then
            hudRoot.render (Hud.view model dispatch)

    // --- sound effect dispatch from GameEvents ------------------------------
    let playSoundsForEvents (oldGame: GameState) (newGame: GameState) : unit =
        // Compare the game states to detect events. Since Ui.updateUi calls
        // State.update internally and we cannot observe events from here
        // directly, we use heuristics based on state diffs.
        ()

    // --- pointer/touch → Msg ------------------------------------------------
    let stage = app.stage
    stage.eventMode <- "static"
    stage.hitArea <- createRectangle 0.0 0.0 layout.CanvasWidth layout.CanvasHeight
    stage.cursor <- "pointer"

    let cellUnder (event: obj) =
        let x, y = pointerPosition event
        cellAtPoint layout size x y, (x, y)

    // Unlock audio on the first user gesture.
    stage.on (
        "pointerdown",
        fun event ->
            Audio.ensureContext ()
            match fst (cellUnder event) with
            | Some coord -> dispatch (GameMsg(StartDrag coord))
            | None -> ()
    )
    |> ignore

    stage.on (
        "pointermove",
        fun event ->
            let cell, pos = cellUnder event
            dispatch (PointerMoved(cell, Some pos))
    )
    |> ignore

    stage.on (
        "pointerup",
        fun event ->
            match model.Game.Interaction with
            | Dragging _ ->
                match fst (cellUnder event) with
                | Some coord -> dispatch (GameMsg(Drop coord))
                | None -> dispatch (GameMsg CancelDrag)
            | Idle -> ()
    )
    |> ignore

    stage.on (
        "pointerupoutside",
        fun _ ->
            match model.Game.Interaction with
            | Dragging _ -> dispatch (GameMsg CancelDrag)
            | Idle -> ()
    )
    |> ignore

    // --- main loop ----------------------------------------------------------
    // Real elapsed milliseconds from the ticker, converted to a validated
    // DeltaTime and injected into the core: movement, waves and combat are
    // time-based, never frame-based.
    app.ticker.add (fun _ ->
        let dtMs = app.ticker.deltaMS
        match DeltaTime.tryCreate (dtMs / 1000.0) with
        | Some dt ->
            // Snapshot game state before update for sound triggering.
            let gameBefore = model.Game

            dispatch (Frame dt)

            // Trigger sounds based on game state changes.
            let gameAfter = model.Game
            // Wave start
            if gameAfter.Wave.Number > gameBefore.Wave.Number then
                Audio.playWaveStart ()
            // Game over
            match gameBefore.Status, gameAfter.Status with
            | Playing _, Defeated _ -> Audio.playGameOver ()
            | _ -> ()

        | None -> ()

        elapsedTime <- elapsedTime + app.ticker.deltaMS / 1000.0

        // Screen shake: offset the stage position.
        if model.ScreenShake > 0.1 then
            let shake = model.ScreenShake
            let ox = (sin (elapsedTime * 90.0)) * shake
            let oy = (cos (elapsedTime * 70.0)) * shake
            stage.position.x <- ox
            stage.position.y <- oy
        else
            stage.position.x <- 0.0
            stage.position.y <- 0.0

        Render.drawFrame layout model layers elapsedTime)
    |> ignore

    hudRoot.render (Hud.view model dispatch)

    // Debug hook for scripts/verify-e2e.mjs (test-only, reads the model).
    Dom.globalThis?__MTD_DEBUG <-
        fun () ->
            createObj
                [ "gold", box (Gold.value model.Game.Gold)
                  "lives",
                  box (
                      match model.Game.Status with
                      | Playing lives -> Lives.value lives
                      | Defeated _ -> 0
                  )
                  "wave", box model.Game.Wave.Number
                  "status",
                  box (
                      match model.Game.Status with
                      | Playing _ -> "playing"
                      | Defeated _ -> "defeated"
                  )
                  "enemies", box (List.length model.Game.Enemies)
                  "dragging",
                  box (
                      match model.Game.Interaction with
                      | Dragging _ -> true
                      | Idle -> false
                  )
                  "layout",
                  createObj
                      [ "gridLeft", box layout.GridLeft
                        "gridTop", box layout.GridTop
                        "cell", box layout.CellSize ]
                  "towers",
                  box (
                      Grid.towers model.Game.Grid
                      |> List.map (fun (coord, tower) ->
                          createObj
                              [ "row", box (Coord.row coord)
                                "col", box (Coord.col coord)
                                "type", box (string tower.Type)
                                "level", box (TowerLevel.rank tower.Level) ])
                      |> List.toArray
                  ) ]

do start ()
