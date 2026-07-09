/// Pure UI-layer state: everything the render/HUD layers need that is not
/// core game logic — canvas layout math, pointer-to-cell hit testing, the
/// transient HUD notice, shot flashes, particle effects, floating text,
/// screen shake, and the frame message that feeds injected DeltaTime into
/// the core engine.
///
/// This module stays as pure as Shared/State: no DOM, no PixiJS, no clock.
/// The interop shells (App.fs, Render.fs, Hud.fs) only send UiMsg values and
/// read the resulting UiModel. Since Phase 3 the economy (gold/lives) and
/// waves live in the core engine; this layer no longer holds placeholders.
module MergeTowerDefense.Ui

open MergeTowerDefense.Shared
open MergeTowerDefense.State

// ---------------------------------------------------------------------------
// Canvas layout (pure math, shared by rendering and hit testing)
// ---------------------------------------------------------------------------

/// Pixel geometry of the play field. Derived from GridSize and the Path
/// bounds only, so the renderer and the pointer hit test can never disagree.
/// GridLeft/GridTop is the pixel position of the grid's top-left corner —
/// the origin of the cell-unit coordinate system used by Path and Coord.
type Layout =
    { CanvasWidth: float
      CanvasHeight: float
      GridLeft: float
      GridTop: float
      CellSize: float }

let private cellSizePx = 72.0
/// Half of the visual width of the enemy lane, in cell units.
let private laneHalfCells = 0.45
/// Outer canvas margin, in cell units.
let private marginCells = 0.3

let layoutFor (size: GridSize) (path: Path) : Layout =
    let n = float (GridSize.value size)
    let pMinX, pMinY, pMaxX, pMaxY = Path.bounds path
    let worldMinX = min 0.0 (pMinX - laneHalfCells) - marginCells
    let worldMinY = min 0.0 (pMinY - laneHalfCells) - marginCells
    let worldMaxX = max n (pMaxX + laneHalfCells) + marginCells
    let worldMaxY = max n (pMaxY + laneHalfCells) + marginCells

    { CanvasWidth = (worldMaxX - worldMinX) * cellSizePx
      CanvasHeight = (worldMaxY - worldMinY) * cellSizePx
      GridLeft = -worldMinX * cellSizePx
      GridTop = -worldMinY * cellSizePx
      CellSize = cellSizePx }

/// Cell-unit point (the Path/Coord coordinate system) to canvas pixels.
let toPx (layout: Layout) (point: float * float) : float * float =
    let x, y = point
    layout.GridLeft + x * layout.CellSize, layout.GridTop + y * layout.CellSize

/// Centre of a cell in canvas pixels.
let cellCenter (layout: Layout) (coord: Coord) : float * float = toPx layout (Coord.center coord)

/// Top-left corner of a cell in canvas pixels.
let cellOrigin (layout: Layout) (coord: Coord) : float * float =
    toPx layout (float (Coord.col coord), float (Coord.row coord))

/// Maps a canvas-pixel position to the grid cell under it, if any.
let cellAtPoint (layout: Layout) (size: GridSize) (x: float) (y: float) : Coord option =
    if x < layout.GridLeft || y < layout.GridTop then
        None
    else
        let col = int ((x - layout.GridLeft) / layout.CellSize)
        let row = int ((y - layout.GridTop) / layout.CellSize)
        Coord.tryCreate size row col

/// Visual width of the enemy lane strip in pixels.
let laneWidthPx (layout: Layout) = 2.0 * laneHalfCells * layout.CellSize

// ---------------------------------------------------------------------------
// Particle system (pure: position, velocity, colour, size, TTL)
// ---------------------------------------------------------------------------

type Particle =
    { X: float; Y: float
      Vx: float; Vy: float
      Color: int
      Size: float
      Ttl: float
      MaxTtl: float }

type FloatingText =
    { X: float; Y: float
      Text: string
      Color: int
      Ttl: float
      MaxTtl: float }

// ---------------------------------------------------------------------------
// UI model
// ---------------------------------------------------------------------------

/// A brief tracer for a shot fired this instant (from a tower cell to a
/// target position in cell units), fading over Ttl seconds.
type Shot =
    { FromCell: Coord
      Target: float * float
      Ttl: float }

type UiModel =
    { Game: GameState
      /// Cell currently under the pointer, if any.
      Hover: Coord option
      /// Raw pointer position in canvas pixels (drives the drag ghost).
      Pointer: (float * float) option
      /// Transient HUD message with its remaining time-to-live in seconds.
      Notice: (string * float) option
      /// Fading shot tracers for the renderer.
      Shots: Shot list
      /// Visual particle effects (sparkles, explosions).
      Particles: Particle list
      /// Floating bounty/info text labels.
      FloatingTexts: FloatingText list
      /// Remaining screen shake intensity (decays per frame).
      ScreenShake: float
      /// Remaining red screen flash opacity (decays per frame).
      RedFlash: float }

type UiMsg =
    /// Forward a message to the core engine untouched.
    | GameMsg of Msg
    /// Pointer moved: hovered cell (if any) and raw canvas position.
    | PointerMoved of Coord option * (float * float) option
    /// HUD "buy tower" button for the given type.
    | Buy of TowerType
    /// HUD "sell tower" on the currently hovered cell.
    | Sell
    /// HUD restart after a game over.
    | Restart
    /// One render-loop frame worth of injected time.
    | Frame of DeltaTime

let init (size: GridSize) : UiModel =
    { Game = GameState.create size
      Hover = None
      Pointer = None
      Notice = None
      Shots = []
      Particles = []
      FloatingTexts = []
      ScreenShake = 0.0
      RedFlash = 0.0 }

// ---------------------------------------------------------------------------
// HUD-facing helpers
// ---------------------------------------------------------------------------

let private noticeTtl = 2.5
let shotTtl = 0.12

let firstEmptyCell (grid: Grid) : Coord option =
    Grid.coords grid |> List.tryFind (fun c -> Grid.cellAt c grid = Empty)

let canBuy (model: UiModel) : bool =
    match model.Game.Status with
    | Defeated _ -> false
    | Playing _ ->
        model.Game.Interaction = Idle
        && Gold.value model.Game.Gold >= nextTowerCost model.Game
        && (firstEmptyCell model.Game.Grid |> Option.isSome)

/// Turns noteworthy game events into a short HUD message, most important
/// first. Routine noise (plain returns, per-shot events) stays silent.
let private noticeOf (event: GameEvent) : (int * string) option =
    match event with
    | GameOver waves -> Some(100, sprintf "Game over — you survived %d wave(s)." waves)
    | ActionRejected(NotEnoughGold required) -> Some(80, sprintf "Not enough gold (need %d)." required)
    | ActionRejected(MergeAtMaxLevel _) -> Some(80, "Already at max level.")
    | ActionRejected(IncompatibleTarget _) -> Some(80, "Towers must share type and level to merge.")
    | ActionRejected(SpawnCellOccupied _) -> Some(80, "That cell is occupied.")
    | ActionRejected SpawnWhileDragging -> Some(80, "Finish the drag first.")
    | TowersMerged(_, _, result, _) -> Some(70, sprintf "Merged! New tower is level %d." (TowerLevel.rank result.Level))
    | TowerSold(_, _, refund) -> Some(70, sprintf "Tower sold! +%dg refund." refund)
    | WaveCompleted(wave, bonus) -> Some(60, sprintf "Wave %d cleared! +%d gold." wave bonus)
    | WaveStarted wave -> Some(50, sprintf "Wave %d incoming!" wave)
    | LifeLost remaining -> Some(40, sprintf "An enemy got through! %d lives left." remaining)
    | _ -> None

let private noticeFor (events: GameEvent list) : string option =
    match events |> List.choose noticeOf with
    | [] -> None
    | picks -> picks |> List.maxBy fst |> snd |> Some

// ---------------------------------------------------------------------------
// Particle / effect generation from game events (pure)
// ---------------------------------------------------------------------------

/// Simple pseudo-random from a seed; returns 0..1 and the next seed.
let private pseudoRandom (seed: int) : float * int =
    let s = (seed * 1103515245 + 12345) &&& 0x7FFFFFFF
    float s / float 0x7FFFFFFF, s

/// Generate N particles with pseudo-random velocities and colors from a set.
let private burstParticles (x: float) (y: float) (count: int) (colors: int list) (speed: float) (ttl: float) (baseSeed: int) : Particle list =
    let mutable seed = baseSeed
    [ for _ in 1 .. count do
        let a, s1 = pseudoRandom seed
        let r, s2 = pseudoRandom s1
        let c, s3 = pseudoRandom s2
        let sz, s4 = pseudoRandom s3
        seed <- s4
        let angle = a * 2.0 * System.Math.PI
        let radius = speed * (0.4 + r * 0.6)
        let colorIdx = int (c * float (List.length colors)) % List.length colors
        { X = x; Y = y
          Vx = cos angle * radius
          Vy = sin angle * radius
          Color = colors.[colorIdx]
          Size = 2.0 + sz * 3.0
          Ttl = ttl * (0.6 + r * 0.4)
          MaxTtl = ttl } ]

let private mergeColors = [ 0xFFD700; 0xFFFFFF; 0xFFF59D; 0xFFEE58 ]
let private killColors = [ 0xEF5350; 0xFF7043; 0xFFAB91; 0xFFA726 ]
let private frostColors = [ 0x4FC3F7; 0x81D4FA; 0xB3E5FC; 0xFFFFFF ]

/// Convert a cell-unit coordinate to a pixel position for effects.
let private effectPos (layout: Layout) (coord: Coord) = cellCenter layout coord

let private effectsOf (layout: Layout) (event: GameEvent) (game: GameState) (seed: int) : Particle list * FloatingText list * float * float =
    match event with
    | TowersMerged(_, _, _, at) ->
        let x, y = effectPos layout at
        burstParticles x y 18 mergeColors 120.0 0.5 seed, [], 3.0, 0.0

    | EnemyKilled(enemyId, bounty) ->
        // Find the killed enemy position from the game state BEFORE the kill
        // was processed. We use the path endpoint as a fallback.
        let x, y =
            match game.Enemies |> List.tryFind (fun e -> e.Id = enemyId) with
            | Some enemy -> toPx layout (Enemy.positionOn game.Path enemy)
            | None ->
                // Killed enemies have already been removed; use path center.
                let px, py = Path.waypoints game.Path |> List.last
                toPx layout (px, py)
        let particles = burstParticles x y 12 killColors 90.0 0.4 seed
        let text =
            [ { X = x; Y = y - 15.0
                Text = sprintf "+%dg" bounty
                Color = 0xFFD700
                Ttl = 0.9
                MaxTtl = 0.9 } ]
        particles, text, 0.0, 0.0

    | WaveStarted _ ->
        [], [], 6.0, 0.0

    | LifeLost _ ->
        [], [], 2.0, 0.6

    | GameOver _ ->
        [], [], 10.0, 0.8

    | TowerSold(_, at, refund) ->
        let x, y = effectPos layout at
        let text =
            [ { X = x; Y = y - 10.0
                Text = sprintf "+%dg" refund
                Color = 0x66BB6A
                Ttl = 0.9
                MaxTtl = 0.9 } ]
        [], text, 0.0, 0.0

    | EnemySlowed enemyId ->
        match game.Enemies |> List.tryFind (fun e -> e.Id = enemyId) with
        | Some enemy ->
            let x, y = toPx layout (Enemy.positionOn game.Path enemy)
            burstParticles x y 6 frostColors 60.0 0.3 seed, [], 0.0, 0.0
        | None -> [], [], 0.0, 0.0

    | _ -> [], [], 0.0, 0.0

// ---------------------------------------------------------------------------
// UI transition function (pure)
// ---------------------------------------------------------------------------

let private applyGame (layout: Layout) (msg: Msg) (model: UiModel) : UiModel =
    let gameBeforeUpdate = model.Game
    let game, events = update msg model.Game

    let notice =
        match noticeFor events with
        | Some text -> Some(text, noticeTtl)
        | None -> model.Notice

    let newShots =
        events
        |> List.choose (fun event ->
            match event with
            | TowerFired(_, origin, target) ->
                Some
                    { FromCell = origin
                      Target = target
                      Ttl = shotTtl }
            | _ -> None)

    // Generate particles and effects from events.
    let mutable seed = int (System.DateTime.Now.Ticks % 2147483647L)
    let mutable allParticles = model.Particles
    let mutable allTexts = model.FloatingTexts
    let mutable shake = model.ScreenShake
    let mutable flash = model.RedFlash

    for event in events do
        let p, t, s, f = effectsOf layout event gameBeforeUpdate seed
        allParticles <- p @ allParticles
        allTexts <- t @ allTexts
        shake <- shake + s
        flash <- max flash f
        seed <- seed + 1

    { model with
        Game = game
        Notice = notice
        Shots = newShots @ model.Shots
        Particles = allParticles
        FloatingTexts = allTexts
        ScreenShake = shake
        RedFlash = flash }

let updateUi (layout: Layout) (msg: UiMsg) (model: UiModel) : UiModel =
    match msg with
    | GameMsg gameMsg -> applyGame layout gameMsg model

    | PointerMoved(hover, pointer) ->
        { model with
            Hover = hover
            Pointer = pointer }

    | Buy towerType ->
        match firstEmptyCell model.Game.Grid with
        | Some cell -> applyGame layout (BuyTower(towerType, cell)) model
        | None ->
            { model with
                Notice = Some("No empty cell for a new tower.", noticeTtl) }

    | Sell ->
        match model.Hover with
        | Some coord ->
            match Grid.cellAt coord model.Game.Grid with
            | Occupied _ -> applyGame layout (SellTower coord) model
            | Empty ->
                { model with Notice = Some("No tower to sell here.", noticeTtl) }
        | None ->
            { model with Notice = Some("Hover over a tower to sell it.", noticeTtl) }

    | Restart -> init (Grid.size model.Game.Grid)

    | Frame dt ->
        let seconds = DeltaTime.seconds dt

        // 1. Advance the core simulation with the injected time step.
        let model = applyGame layout (Tick dt) model

        // 2. Fade the transient HUD notice and the shot tracers.
        let notice =
            model.Notice
            |> Option.bind (fun (text, ttl) ->
                let ttl' = ttl - seconds
                if ttl' <= 0.0 then None else Some(text, ttl'))

        let shots =
            model.Shots
            |> List.choose (fun shot ->
                let ttl' = shot.Ttl - seconds
                if ttl' <= 0.0 then None else Some { shot with Ttl = ttl' })

        // 3. Advance particle physics: position += velocity, TTL decay, size shrink.
        let particles =
            model.Particles
            |> List.choose (fun p ->
                let ttl' = p.Ttl - seconds
                if ttl' <= 0.0 then None
                else
                    Some { p with
                            X = p.X + p.Vx * seconds
                            Y = p.Y + p.Vy * seconds
                            Vy = p.Vy + 80.0 * seconds  // gravity
                            Ttl = ttl'
                            Size = p.Size * (ttl' / p.MaxTtl) })

        // 4. Advance floating texts: rise and fade.
        let floatingTexts =
            model.FloatingTexts
            |> List.choose (fun ft ->
                let ttl' = ft.Ttl - seconds
                if ttl' <= 0.0 then None
                else Some { ft with Y = ft.Y - 30.0 * seconds; Ttl = ttl' })

        // 5. Decay screen shake and red flash.
        let shake = max 0.0 (model.ScreenShake - seconds * 20.0)
        let flash = max 0.0 (model.RedFlash - seconds * 2.0)

        { model with
            Notice = notice
            Shots = shots
            Particles = particles
            FloatingTexts = floatingTexts
            ScreenShake = shake
            RedFlash = flash }
