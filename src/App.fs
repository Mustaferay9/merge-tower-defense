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
    let campaign =
        match Dom.getItem "mtd_campaign" with
        | Some json ->
            try Fable.Core.JS.JSON.parse(json) |> unbox<CampaignState>
            with _ -> CampaignState.empty
        | None -> CampaignState.empty

    let maxWave =
        match Dom.getItem "mtd_maxWave" with
        | Some s -> try int s with _ -> 0
        | None -> 0

    let currentLevel = Levels.get 1 |> Option.get
    let mutable model = init campaign currentLevel maxWave
    
    let getLayout (m: UiModel) =
        let level = Levels.get m.Game.LevelId |> Option.get
        layoutFor level.Size m.Game.Path

    // --- PixiJS application (WebGL with automatic canvas fallback) --------
    let initialLayout = getLayout model
    let app =
        createApplication
            [ "width", box initialLayout.CanvasWidth
              "height", box initialLayout.CanvasHeight
              "background", box 0x141724
              "antialias", box true ]

    app.ticker.maxFPS <- 60.0
    Dom.appendChild (Dom.getElementById "game-root") app.view

    let layers = Render.createLayers app
    let mutable currentRenderedTheme = model.Game.Theme
    let mutable currentRenderedSize = currentLevel.Size
    Render.drawStatic currentRenderedTheme initialLayout currentRenderedSize model.Game.Path layers

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
         | CampaignMenu | TalentScreen -> -3
         | Victory -> -4
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
        let beforeMaxWave = model.MaxWaveReached
        let beforeCampaign = model.Campaign
        let beforeStatus = model.Game.Status

        let layout = getLayout model
        model <- updateUi layout msg model

        if beforeMaxWave <> model.MaxWaveReached || beforeCampaign <> model.Campaign then
            Dom.setItem "mtd_maxWave" (string model.MaxWaveReached)
            Dom.setItem "mtd_campaign" (Fable.Core.JS.JSON.stringify model.Campaign)

        if beforeStatus <> model.Game.Status then
            match beforeStatus, model.Game.Status with
            | CampaignMenu, Playing _ -> Audio.startAmbientMusic (sprintf "%A" model.Game.Theme)
            | Playing _, Defeated _ -> Audio.playGameOver ()
            | Defeated _, CampaignMenu | Victory, CampaignMenu -> Audio.stopAmbientMusic ()
            | _ -> ()

        if hudProjection model <> before then
            hudRoot.render (Hud.view model dispatch)

    // --- sound effect dispatch from GameEvents ------------------------------
    let playSoundsForEvents (oldGame: GameState) (newGame: GameState) : unit =

        // 2. Wave Start
        match oldGame.Wave.Phase, newGame.Wave.Phase with
        | BetweenWaves _, WaveActive -> Audio.playWaveStart ()
        | _ -> ()

        // 3. Buying / Merging / Selling
        let oldTowers = Grid.towerCount oldGame.Grid
        let newTowers = Grid.towerCount newGame.Grid
        if newGame.TowersBought > oldGame.TowersBought then
            Audio.playBuy ()
        elif newTowers < oldTowers then
            if Gold.value newGame.Gold > Gold.value oldGame.Gold then
                Audio.playSell ()
            else
                Audio.playMerge ()

        // 4. Enemy Kill
        let oldEnemies = List.length oldGame.Enemies
        let newEnemies = List.length newGame.Enemies
        if newEnemies < oldEnemies then
            let oldLives = match oldGame.Status with Playing l -> Lives.value l | _ -> 0
            let newLives = match newGame.Status with Playing l -> Lives.value l | _ -> 0
            if newLives = oldLives then
                Audio.playKill ()

        // 5. Shooting
        Grid.towers newGame.Grid
        |> List.iter (fun (coord, tower) ->
            match Grid.tryFindTower coord oldGame.Grid with
            | Some oldTower when tower.Cooldown > oldTower.Cooldown ->
                match tower.Type with
                | Archer -> Audio.playShootArcher ()
                | Cannon -> Audio.playShootCannon ()
                | Frost  -> Audio.playShootFrost ()
            | _ -> ()
        )

        // 6. Spells
        match oldGame.Interaction, newGame.Interaction with
        | CastingSpell spell, Idle when Gold.value newGame.Gold < Gold.value oldGame.Gold ->
            match spell with
            | Fireball -> Audio.playSpellFireball ()
            | FrostNova -> Audio.playSpellFrostNova ()
        | _ -> ()

    // --- pointer/touch → Msg ------------------------------------------------
    let stage = app.stage
    stage.eventMode <- "static"
    let initialLayout = getLayout model
    stage.hitArea <- createRectangle 0.0 0.0 initialLayout.CanvasWidth initialLayout.CanvasHeight
    stage.cursor <- "pointer"

    let cellUnder (event: obj) =
        let x, y = pointerPosition event
        let layout = getLayout model
        cellAtPoint layout (Grid.size model.Game.Grid) x y, (x, y)

    // Unlock audio on the first user gesture.
    stage.on (
        "pointerdown",
        fun event ->
            Audio.ensureContext ()
            match model.Game.Interaction with
            | CastingSpell spell ->
                match fst (cellUnder event) with
                | Some coord -> dispatch (GameMsg(CastSpell(spell, coord)))
                | None -> dispatch (GameMsg CancelDrag)
            | Idle | Dragging _ ->
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
            | CastingSpell _
            | Idle -> ()
    )
    |> ignore

    stage.on (
        "pointerupoutside",
        fun _ ->
            match model.Game.Interaction with
            | Dragging _ -> dispatch (GameMsg CancelDrag)
            | CastingSpell _ -> dispatch (GameMsg CancelDrag) // cancel cast when clicking outside
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

        if currentRenderedTheme <> model.Game.Theme then
            layers.Static.clear() |> ignore
            currentRenderedTheme <- model.Game.Theme
            let layout = getLayout model
            Render.drawStatic currentRenderedTheme layout (Grid.size model.Game.Grid) model.Game.Path layers

        let layout = getLayout model
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
                      | CampaignMenu | TalentScreen | Victory -> 0
                      | Playing lives -> Lives.value lives
                      | Defeated _ -> 0
                  )
                  "wave", box model.Game.Wave.Number
                  "status",
                  box (
                      match model.Game.Status with
                      | CampaignMenu -> "mainmenu"
                      | TalentScreen -> "talents"
                      | Playing _ -> "playing"
                      | Defeated _ -> "defeated"
                      | Victory -> "victory"
                  )
                  "enemies", box (List.length model.Game.Enemies)
                  "dragging",
                  box (
                      match model.Game.Interaction with
                      | Dragging _ -> true
                      | Idle
                      | CastingSpell _ -> false
                  )
                  "layout",
                  let layout = getLayout model
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
