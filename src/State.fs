/// The game state machine for Merge Tower Defense.
///
/// Every change to the game flows through the single pure function
///     update : Msg -> GameState -> GameState * GameEvent list
/// No mutation, no IO, no clock: time arrives as a Tick DeltaTime message
/// and the UI layer only sends Msg values and renders GameEvents.
///
/// Phase 3 adds the game systems on top of the Phase 1 merge machine: wave
/// scheduling, path-based movement, tower targeting/shooting, the economy
/// (gold and lives) and the difficulty curve.
module MergeTowerDefense.State

open MergeTowerDefense.Shared

// ---------------------------------------------------------------------------
// Interaction state machine (drag & drop)
// ---------------------------------------------------------------------------

/// An in-flight drag gesture. The dragged tower has been lifted OFF the grid
/// and lives only here — it cannot exist in two places at once, and a drag
/// without a tower is unrepresentable. (While lifted, the tower cannot fire.)
type DragState =
    { Origin: Coord
      Tower: Tower }

type Interaction =
    | Idle
    | Dragging of DragState

// ---------------------------------------------------------------------------
// Waves and game status
// ---------------------------------------------------------------------------

type WavePhase =
    /// Pause before the next wave starts.
    | BetweenWaves of secondsLeft: float
    /// The current wave still has enemies to spawn. untilNext may carry a
    /// negative credit so long ticks spawn as many as the elapsed time owes.
    | Spawning of pending: EnemyType list * untilNext: float
    /// Everything spawned; the wave ends when the field is clear.
    | WaveActive

type WaveState =
    { /// 0 before the first wave, then the 1-based current wave number.
      Number: int
      Phase: WavePhase }

/// Lives only exist while playing; a defeated game has no life counter to
/// misread. Running out of lives is the one-way transition to Defeated.
type GameStatus =
    | Playing of Lives
    | Defeated of wavesSurvived: int

/// The difficulty curve: what each wave throws at the player and what the
/// player earns for surviving it. Pure functions of the wave number.
module Waves =
    /// Seconds before the very first wave.
    let initialDelay = 5.0

    /// Seconds between clearing a wave and the next one starting.
    let interWaveDelay = 4.0

    /// Seconds between spawns within a wave; tightens as waves progress.
    let spawnInterval (wave: int) = max 0.45 (1.1 - 0.04 * float wave)

    /// Enemy health scales linearly with the wave number.
    let healthMultiplier (wave: int) = 1.0 + 0.18 * float (max 1 wave - 1)

    /// Gold awarded for clearing a wave.
    let completionBonus (wave: int) = 20 + 5 * wave

    /// Spawn order for a wave. Never empty: every wave has at least four
    /// grunts, so entering Spawning with an empty queue is unrepresentable
    /// in practice (and handled anyway).
    let composition (wave: int) : EnemyType list =
        let grunts = List.replicate (3 + wave) Grunt
        let runners = if wave >= 2 then List.replicate (wave - 1) Runner else []
        let tanks = if wave >= 4 then List.replicate ((wave - 2) / 2) Tank else []
        let bosses = if wave % 5 = 0 then List.replicate (wave / 5) Boss else []
        grunts @ runners @ tanks @ bosses

// ---------------------------------------------------------------------------
// Game state
// ---------------------------------------------------------------------------

type GameState =
    { Grid: Grid
      Interaction: Interaction
      Enemies: Enemy list
      TowerIds: TowerIdGen
      EnemyIds: EnemyIdGen
      Path: Path
      Wave: WaveState
      Gold: Gold
      /// Towers bought so far; drives the escalating purchase price.
      TowersBought: int
      Status: GameStatus }

let startingGold = 110
let startingLives = 10
let towerBaseCost = 20
let towerCostGrowth = 4

/// Price of the next tower purchase (escalates with every buy).
let nextTowerCost (state: GameState) =
    towerBaseCost + towerCostGrowth * state.TowersBought

/// Refund value when selling a tower (60 % of base cost for its level).
let sellValue (tower: Tower) =
    let rank = TowerLevel.rank tower.Level
    int (round (float towerBaseCost * float (pown 2 (rank - 1)) * 0.6))

module GameState =
    let create (size: GridSize) =
        { Grid = Grid.create size
          Interaction = Idle
          Enemies = []
          TowerIds = TowerIdGen.initial
          EnemyIds = EnemyIdGen.initial
          Path = Path.defaultFor size
          Wave = { Number = 0; Phase = BetweenWaves Waves.initialDelay }
          Gold = Gold.zero |> Gold.earn startingGold
          TowersBought = 0
          Status = Playing(Lives.create startingLives) }

// ---------------------------------------------------------------------------
// Messages, events, rejections
// ---------------------------------------------------------------------------

/// Why an action was refused. Rejections never mutate state; they only show
/// up as ActionRejected events so the UI can explain itself.
type RejectReason =
    | NotDragging
    | AlreadyDragging
    | OriginEmpty of Coord
    | IncompatibleTarget of Coord
    | MergeAtMaxLevel of Coord
    | SpawnCellOccupied of Coord
    | SpawnWhileDragging
    | NotEnoughGold of required: int
    | UnknownEnemy of EnemyId
    | GameAlreadyOver

/// Facts about what a transition did — the UI renders these; tests assert on
/// them. Events describe the past, so they carry the concrete values.
type GameEvent =
    | DragBegan of tower: Tower * origin: Coord
    | TowerMoved of tower: Tower * origin: Coord * target: Coord
    | TowersMerged of dragged: Tower * absorbed: Tower * result: Tower * at: Coord
    | TowerReturned of tower: Tower * origin: Coord
    | TowerSpawned of tower: Tower * at: Coord
    | TowerBought of tower: Tower * at: Coord * cost: int
    | TowerSold of tower: Tower * at: Coord * refund: int
    /// A shot was fired from a tower cell at a target position (cell units).
    | TowerFired of TowerId * Coord * (float * float)
    | WaveStarted of wave: int
    | WaveCompleted of wave: int * bonus: int
    | EnemySpawned of Enemy
    | EnemyReachedGoal of EnemyId
    | LifeLost of remaining: int
    | EnemyDamaged of EnemyId * remaining: Health
    | EnemyKilled of EnemyId * bounty: int
    | EnemySlowed of EnemyId
    | GameOver of wavesSurvived: int
    | ActionRejected of RejectReason

/// Everything the outside world (UI, tests) may ask of the game. The last
/// three exist for tests and tooling; the UI uses the gestures, BuyTower
/// and Tick.
type Msg =
    | StartDrag of Coord
    | Drop of Coord
    | CancelDrag
    | BuyTower of TowerType * Coord
    | SellTower of Coord
    | Tick of DeltaTime
    | SpawnTower of TowerType * Coord
    | SpawnEnemy of EnemyType
    | HitEnemy of EnemyId * Damage

// ---------------------------------------------------------------------------
// Drop preview (pure derivation for UI highlighting)
// ---------------------------------------------------------------------------

/// What dropping on a given cell would do right now.
type DropPreview =
    | MoveHere
    | MergeHere of TowerLevel
    | ReturnToOrigin
    | Blocked

/// Pure preview for hover highlights while dragging. Consistency with the
/// actual Drop transition is asserted by the test suite.
let previewDrop (target: Coord) (state: GameState) : DropPreview option =
    match state.Interaction with
    | Idle -> None
    | Dragging drag ->
        if target = drag.Origin then
            Some ReturnToOrigin
        else
            match Grid.cellAt target state.Grid with
            | Empty -> Some MoveHere
            | Occupied other ->
                match Tower.canMerge drag.Tower other with
                | Some level -> Some(MergeHere level)
                | None -> Some Blocked

// ---------------------------------------------------------------------------
// Internal helpers
// ---------------------------------------------------------------------------

/// Places a tower on a cell that the surrounding transition has just proven
/// empty (a freshly lifted drag origin, or a cell that cellAt/tryLift
/// reported empty within the same pure transition). The failure branch is
/// unreachable: SpawnTower/BuyTower are rejected mid-drag and every other
/// change goes through this same update function, so nothing can occupy the
/// cell in between.
let private placeOnEmpty (coord: Coord) (tower: Tower) (grid: Grid) : Grid =
    match Grid.tryPlace coord tower grid with
    | Some grid' -> grid'
    | None -> failwith "unreachable: cell was proven empty within this transition"

let private withPhase (phase: WavePhase) (state: GameState) =
    { state with Wave = { state.Wave with Phase = phase } }

// ---------------------------------------------------------------------------
// Tick pipeline: wave scheduling → movement/lives → combat → wave completion
// ---------------------------------------------------------------------------

/// Spawns every enemy the elapsed time owes (untilNext ≤ 0), then either
/// stays in Spawning or, once the queue is empty, goes WaveActive.
let rec private drainSpawns (state: GameState) (events: GameEvent list) =
    match state.Wave.Phase with
    | Spawning(next :: rest, untilNext) when untilNext <= 0.0 ->
        let enemy, gen =
            Enemy.spawnWith state.EnemyIds next (Waves.healthMultiplier state.Wave.Number)

        let state =
            { state with
                Enemies = state.Enemies @ [ enemy ]
                EnemyIds = gen }
            |> withPhase (Spawning(rest, untilNext + Waves.spawnInterval state.Wave.Number))

        drainSpawns state (events @ [ EnemySpawned enemy ])
    | Spawning([], _) -> withPhase WaveActive state, events
    | _ -> state, events

let private stepWave (dtSeconds: float) (state: GameState) =
    match state.Wave.Phase with
    | WaveActive -> state, []
    | BetweenWaves secondsLeft ->
        let remaining = secondsLeft - dtSeconds

        if remaining > 0.0 then
            withPhase (BetweenWaves remaining) state, []
        else
            let number = state.Wave.Number + 1

            let state =
                { state with
                    Wave =
                        { Number = number
                          // The overshoot becomes spawn credit, so wave
                          // timing does not depend on tick granularity.
                          Phase = Spawning(Waves.composition number, remaining) } }

            drainSpawns state [ WaveStarted number ]
    | Spawning(pending, untilNext) -> drainSpawns (withPhase (Spawning(pending, untilNext - dtSeconds)) state) []

/// Moves every enemy; goal-reachers cost lives and may end the game.
/// Also decrements Frost slow timers.
let private stepMovement (dt: DeltaTime) (state: GameState) =
    let dtSeconds = DeltaTime.seconds dt
    let folder (survivors, status, events) (enemy: Enemy) =
        let enemy = Enemy.tickSlow dtSeconds enemy
        match Enemy.advance state.Path dt enemy with
        | Moved progress -> { enemy with Progress = progress } :: survivors, status, events
        | ReachedGoal ->
            let events = events @ [ EnemyReachedGoal enemy.Id ]

            match status with
            | Defeated _ -> survivors, status, events
            | Playing lives ->
                match Lives.lose (EnemyType.livesCost enemy.Type) lives with
                | StillAlive lives' -> survivors, Playing lives', events @ [ LifeLost(Lives.value lives') ]
                | AllLost ->
                    let survived = max 0 (state.Wave.Number - 1)
                    survivors, Defeated survived, events @ [ LifeLost 0; GameOver survived ]

    let survivorsRev, status, events =
        List.fold folder ([], state.Status, []) state.Enemies

    { state with
        Enemies = List.rev survivorsRev
        Status = status },
    events

/// Targeting and shooting: every ready tower fires once per tick at the
/// in-range enemy that is furthest along the path ("first" targeting).
/// Towers are processed in deterministic coordinate order.
let private stepCombat (dtSeconds: float) (state: GameState) =
    match state.Status with
    | Defeated _ -> state, []
    | Playing _ ->
        let cooled =
            state.Grid
            |> Grid.mapTowers (fun t -> { t with Cooldown = max 0.0 (t.Cooldown - dtSeconds) })

        let folder (enemies: Enemy list, gold, resets: Map<TowerId, float>, events) (coord, tower: Tower) =
            if tower.Cooldown > 0.0 then
                enemies, gold, resets, events
            else
                let stats = Tower.stats tower
                let tx, ty = Coord.center coord

                let inRange (enemy: Enemy) =
                    let ex, ey = Enemy.positionOn state.Path enemy
                    let dx = ex - tx
                    let dy = ey - ty
                    dx * dx + dy * dy <= stats.Range * stats.Range

                match enemies |> List.filter inRange with
                | [] -> enemies, gold, resets, events
                | candidates ->
                    let target =
                        candidates |> List.maxBy (fun e -> PathProgress.value e.Progress)

                    let resets = Map.add tower.Id (float stats.CooldownMs / 1000.0) resets
                    let fired = TowerFired(tower.Id, coord, Enemy.positionOn state.Path target)

                    match AttackResult.ofDamage (Tower.attackDamage tower) target.Health with
                    | Survived remaining ->
                        let slowEvents =
                            if tower.Type = Frost && target.SlowUntil <= 0.0 then [ EnemySlowed target.Id ] else []
                        let slowDuration = if tower.Type = Frost then 1.5 else 0.0
                        enemies
                        |> List.map (fun e ->
                            if e.Id = target.Id then
                                { e with
                                    Health = remaining
                                    SlowUntil = max e.SlowUntil slowDuration }
                            else e),
                        gold,
                        resets,
                        events @ [ fired; EnemyDamaged(target.Id, remaining) ] @ slowEvents
                    | Killed ->
                        let bounty = EnemyType.bounty target.Type

                        enemies |> List.filter (fun e -> e.Id <> target.Id),
                        Gold.earn bounty gold,
                        resets,
                        events @ [ fired; EnemyKilled(target.Id, bounty) ]

        let enemies, gold, resets, events =
            Grid.towers cooled
            |> List.fold folder (state.Enemies, state.Gold, Map.empty, [])

        let grid =
            cooled
            |> Grid.mapTowers (fun t ->
                match Map.tryFind t.Id resets with
                | Some cooldown -> { t with Cooldown = cooldown }
                | None -> t)

        { state with
            Grid = grid
            Enemies = enemies
            Gold = gold },
        events

/// An active wave is complete once the field is clear: award the bonus and
/// start the countdown to the next wave.
let private checkWaveCompletion (state: GameState) =
    match state.Status, state.Wave.Phase with
    | Playing _, WaveActive when List.isEmpty state.Enemies ->
        let bonus = Waves.completionBonus state.Wave.Number

        { state with Gold = Gold.earn bonus state.Gold }
        |> withPhase (BetweenWaves Waves.interWaveDelay),
        [ WaveCompleted(state.Wave.Number, bonus) ]
    | _ -> state, []

// ---------------------------------------------------------------------------
// Transition function
// ---------------------------------------------------------------------------

let private updatePlaying (msg: Msg) (state: GameState) : GameState * GameEvent list =
    match msg, state.Interaction with

    // -- simulation tick -----------------------------------------------------

    | Tick dt, _ ->
        let dtSeconds = DeltaTime.seconds dt
        let state1, events1 = stepWave dtSeconds state
        let state2, events2 = stepMovement dt state1
        let state3, events3 = stepCombat dtSeconds state2
        let state4, events4 = checkWaveCompletion state3
        state4, events1 @ events2 @ events3 @ events4

    // -- drag & drop / merge ------------------------------------------------

    | StartDrag origin, Idle ->
        match Grid.tryLift origin state.Grid with
        | None -> state, [ ActionRejected(OriginEmpty origin) ]
        | Some(tower, grid) ->
            { state with
                Grid = grid
                Interaction = Dragging { Origin = origin; Tower = tower } },
            [ DragBegan(tower, origin) ]

    | StartDrag _, Dragging _ -> state, [ ActionRejected AlreadyDragging ]

    | Drop _, Idle -> state, [ ActionRejected NotDragging ]

    | Drop target, Dragging drag when target = drag.Origin ->
        { state with
            Grid = placeOnEmpty drag.Origin drag.Tower state.Grid
            Interaction = Idle },
        [ TowerReturned(drag.Tower, drag.Origin) ]

    | Drop target, Dragging drag ->
        match Grid.tryLift target state.Grid with
        | None ->
            // Target cell is empty: plain move.
            { state with
                Grid = placeOnEmpty target drag.Tower state.Grid
                Interaction = Idle },
            [ TowerMoved(drag.Tower, drag.Origin, target) ]
        | Some(targetTower, gridWithoutTarget) ->
            match Tower.canMerge drag.Tower targetTower with
            | Some mergedLevel ->
                let mergedId, towerIds = TowerIdGen.next state.TowerIds

                let merged =
                    { Id = mergedId
                      Type = targetTower.Type
                      Level = mergedLevel
                      Cooldown = 0.0 }

                { state with
                    Grid = placeOnEmpty target merged gridWithoutTarget
                    Interaction = Idle
                    TowerIds = towerIds },
                [ TowersMerged(drag.Tower, targetTower, merged, target) ]
            | None ->
                let reason =
                    if drag.Tower.Type = targetTower.Type && drag.Tower.Level = targetTower.Level then
                        MergeAtMaxLevel target
                    else
                        IncompatibleTarget target

                { state with
                    Grid = placeOnEmpty drag.Origin drag.Tower state.Grid
                    Interaction = Idle },
                [ ActionRejected reason; TowerReturned(drag.Tower, drag.Origin) ]

    | CancelDrag, Dragging drag ->
        { state with
            Grid = placeOnEmpty drag.Origin drag.Tower state.Grid
            Interaction = Idle },
        [ TowerReturned(drag.Tower, drag.Origin) ]

    | CancelDrag, Idle -> state, [ ActionRejected NotDragging ]

    // -- economy: buying towers ----------------------------------------------

    // Rejected mid-drag: this is what keeps the drag origin provably empty
    // for the whole gesture (see placeOnEmpty).
    | BuyTower _, Dragging _ -> state, [ ActionRejected SpawnWhileDragging ]

    | BuyTower(towerType, coord), Idle ->
        let cost = nextTowerCost state

        match Grid.cellAt coord state.Grid with
        | Occupied _ -> state, [ ActionRejected(SpawnCellOccupied coord) ]
        | Empty ->
            match Gold.trySpend cost state.Gold with
            | None -> state, [ ActionRejected(NotEnoughGold cost) ]
            | Some gold ->
                let id, towerIds = TowerIdGen.next state.TowerIds

                let tower =
                    { Id = id
                      Type = towerType
                      Level = Level1
                      Cooldown = 0.0 }

                { state with
                    Grid = placeOnEmpty coord tower state.Grid
                    Gold = gold
                    TowerIds = towerIds
                    TowersBought = state.TowersBought + 1 },
                [ TowerBought(tower, coord, cost) ]

    // -- economy: selling towers ----------------------------------------------

    | SellTower _, Dragging _ -> state, [ ActionRejected SpawnWhileDragging ]

    | SellTower coord, Idle ->
        match Grid.tryLift coord state.Grid with
        | None -> state, [ ActionRejected(OriginEmpty coord) ]
        | Some(tower, grid) ->
            let refund = sellValue tower
            { state with Grid = grid; Gold = Gold.earn refund state.Gold },
            [ TowerSold(tower, coord, refund) ]

    // -- test/tooling messages ------------------------------------------------

    | SpawnTower _, Dragging _ -> state, [ ActionRejected SpawnWhileDragging ]

    | SpawnTower(towerType, coord), Idle ->
        let id, towerIds = TowerIdGen.next state.TowerIds

        let tower =
            { Id = id
              Type = towerType
              Level = Level1
              Cooldown = 0.0 }

        match Grid.tryPlace coord tower state.Grid with
        | Some grid ->
            { state with
                Grid = grid
                TowerIds = towerIds },
            [ TowerSpawned(tower, coord) ]
        | None -> state, [ ActionRejected(SpawnCellOccupied coord) ]

    | SpawnEnemy enemyType, _ ->
        let enemy, enemyIds = Enemy.spawn state.EnemyIds enemyType

        { state with
            Enemies = state.Enemies @ [ enemy ]
            EnemyIds = enemyIds },
        [ EnemySpawned enemy ]

    | HitEnemy(enemyId, damage), _ ->
        match state.Enemies |> List.tryFind (fun e -> e.Id = enemyId) with
        | None -> state, [ ActionRejected(UnknownEnemy enemyId) ]
        | Some enemy ->
            match AttackResult.ofDamage damage enemy.Health with
            | Survived remaining ->
                let enemies =
                    state.Enemies
                    |> List.map (fun e -> if e.Id = enemyId then { e with Health = remaining } else e)

                { state with Enemies = enemies }, [ EnemyDamaged(enemyId, remaining) ]
            | Killed ->
                let bounty = EnemyType.bounty enemy.Type

                { state with
                    Enemies = state.Enemies |> List.filter (fun e -> e.Id <> enemyId)
                    Gold = Gold.earn bounty state.Gold },
                [ EnemyKilled(enemyId, bounty) ]

let update (msg: Msg) (state: GameState) : GameState * GameEvent list =
    match state.Status with
    | Defeated _ ->
        // The tableau is frozen after defeat; time passing is a silent no-op,
        // every attempted action is an explicit rejection.
        match msg with
        | Tick _ -> state, []
        | _ -> state, [ ActionRejected GameAlreadyOver ]
    | Playing _ -> updatePlaying msg state
