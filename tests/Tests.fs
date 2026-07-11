/// Pure unit tests for the core state engine (Phases 1–3) and the pure UI
/// layer.
///
/// Zero-dependency mini test runner so the suite compiles with any Fable
/// toolchain (including the dotnet-free fable-compiler-js path) and runs
/// under plain node. The two mutable counters below are the only mutation in
/// the whole repository, and they live outside the game core.
module MergeTowerDefense.Tests

open MergeTowerDefense.Shared
open MergeTowerDefense.State
open MergeTowerDefense.Ui

let mutable private passed = 0
let mutable private failed = 0
let startingGold = Talents.startingGold Talents.empty

let private check (name: string) (condition: bool) =
    if condition then
        passed <- passed + 1
    else
        failed <- failed + 1
        printfn "FAIL  %s" name

// ---------------------------------------------------------------------------
// Test helpers
// ---------------------------------------------------------------------------

let private gridSize n =
    match GridSize.tryCreate n with
    | Some s -> s
    | None -> failwith "test setup: invalid grid size"

let private coordIn (size: GridSize) r c =
    match Coord.tryCreate size r c with
    | Some coord -> coord
    | None -> failwith "test setup: invalid coord"

let dummyLevel (size: GridSize) =
    { Id = 0
      Name = "Dummy"
      Description = "Dummy"
      Size = size
      Theme = MapTheme.Plain
      AllowedTowers = Set.empty
      StartingGold = 0
      WaveCount = 5 }

let private size5 = gridSize 5
let private at r c = coordIn size5 r c

/// Fold a message list through update, collecting every emitted event.
let private run (msgs: Msg list) (initial: GameState) : GameState * GameEvent list =
    msgs
    |> List.fold
        (fun (state, log) msg ->
            let state', events = update msg state
            state', log @ events)
        (initial, [])

let private fresh () = GameState.create (dummyLevel size5) Talents.empty

/// Pushes the next wave far into the future so tests can exercise movement,
/// combat and economy without the scheduler interfering.
let private noWaves (state: GameState) =
    { state with Wave = { state.Wave with Phase = BetweenWaves 1e9 } }

let private hasReject (reason: RejectReason) (events: GameEvent list) =
    events |> List.contains (ActionRejected reason)

let private dmg n =
    match Damage.tryCreate n with
    | Some d -> d
    | None -> failwith "test setup: invalid damage"

let private dt seconds =
    match DeltaTime.tryCreate seconds with
    | Some d -> d
    | None -> failwith "test setup: invalid delta time"

let private goldOf (state: GameState) = Gold.value state.Gold

let private livesOf (state: GameState) =
    match state.Status with
    | Playing lives -> Lives.value lives
    | Defeated _ -> 0

let private ticks (count: int) (step: float) (state: GameState) : GameState * GameEvent list =
    run [ for _ in 1 .. count -> Tick(dt step) ] state

// ---------------------------------------------------------------------------
// Smart constructors: illegal values have no representation
// ---------------------------------------------------------------------------

let private testSmartConstructors () =
    check "GridSize rejects 1" (GridSize.tryCreate 1 = None)
    check "GridSize rejects 0" (GridSize.tryCreate 0 = None)
    check "GridSize rejects 13" (GridSize.tryCreate 13 = None)
    check "GridSize accepts 5" (GridSize.tryCreate 5 |> Option.isSome)

    check "Coord rejects negative row" (Coord.tryCreate size5 -1 0 = None)
    check "Coord rejects col out of bounds" (Coord.tryCreate size5 0 5 = None)
    check "Coord accepts corner" (Coord.tryCreate size5 4 4 |> Option.isSome)

    check "Damage rejects 0" (Damage.tryCreate 0 = None)
    check "Damage rejects negative" (Damage.tryCreate -3 = None)
    check "Damage accepts positive" (Damage.tryCreate 7 |> Option.isSome)

    check "Health rejects 0" (Health.tryCreate 0 = None)
    check "Health accepts positive" (Health.tryCreate 10 |> Option.isSome)

    check "DeltaTime rejects 0" (DeltaTime.tryCreate 0.0 = None)
    check "DeltaTime rejects negative" (DeltaTime.tryCreate -0.5 = None)
    check "DeltaTime rejects nan" (DeltaTime.tryCreate nan = None)
    check "DeltaTime rejects infinity" (DeltaTime.tryCreate infinity = None)
    check "DeltaTime accepts 0.016" (DeltaTime.tryCreate 0.016 |> Option.isSome)

    check "PathProgress rejects 1.0" (PathProgress.tryCreate 1.0 = None)
    check "PathProgress rejects negative" (PathProgress.tryCreate -0.1 = None)
    check "PathProgress accepts 0.5" (PathProgress.tryCreate 0.5 |> Option.isSome)

    check "TowerLevel.next tops out at Level5" (TowerLevel.next Level5 = None)
    check "TowerLevel.next Level1 = Level2" (TowerLevel.next Level1 = Some Level2)

    check "Gold cannot go into debt" (Gold.zero |> Gold.trySpend 1 = None)
    check "Gold spend within balance works"
        (Gold.zero |> Gold.earn 30 |> Gold.trySpend 20 |> Option.map Gold.value = Some 10)
    check "Gold ignores negative earnings" (Gold.zero |> Gold.earn -5 |> Gold.value = 0)

    check "Lives.create clamps to one" (Lives.create 0 |> Lives.value = 1)
    check "Lives.lose survives partial loss"
        (match Lives.create 5 |> Lives.lose 2 with
         | StillAlive l -> Lives.value l = 3
         | AllLost -> false)
    check "Lives.lose reports all lost" (Lives.create 2 |> Lives.lose 2 = AllLost)

    check "Path needs at least two points" (Path.tryCreate [ 0.0, 0.0 ] = None)
    check "Path rejects zero length" (Path.tryCreate [ 1.0, 1.0; 1.0, 1.0 ] = None)
    check "Path accepts a polyline" (Path.tryCreate [ 0.0, 0.0; 3.0, 0.0; 3.0, 4.0 ] |> Option.isSome)

// ---------------------------------------------------------------------------
// Path geometry
// ---------------------------------------------------------------------------

let private testPath () =
    match Path.tryCreate [ 0.0, 0.0; 3.0, 0.0; 3.0, 4.0 ] with
    | None -> check "test path builds" false
    | Some path ->
        check "path length sums segments" (abs (Path.length path - 7.0) < 1e-9)
        check "path starts at first waypoint" (Path.pointAtDistance path 0.0 = (0.0, 0.0))
        check "path walks the first segment" (Path.pointAtDistance path 1.5 = (1.5, 0.0))
        check "path turns onto the second segment"
            (let x, y = Path.pointAtDistance path 5.0
             abs (x - 3.0) < 1e-9 && abs (y - 2.0) < 1e-9)
        check "path clamps past the end" (Path.pointAtDistance path 100.0 = (3.0, 4.0))
        check "positionAt start equals first waypoint" (Path.positionAt path PathProgress.start = (0.0, 0.0))

    let default5 = Path.defaultFor size5
    let minX, minY, maxX, maxY = Path.bounds default5
    check "default path runs above the grid" (minY < 0.0)
    check "default path covers top and right flanks" (minX < 0.0 && maxX > 5.0 && maxY > 5.0)

// ---------------------------------------------------------------------------
// Spawning towers / cell occupancy
// ---------------------------------------------------------------------------

let private testSpawning () =
    let state, events = fresh () |> run [ SpawnTower(Archer, at 0 0) ]

    check "spawn fills the cell"
        (match Grid.cellAt (at 0 0) state.Grid with
         | Occupied t -> t.Type = Archer && t.Level = Level1
         | Empty -> false)

    check "spawn emits TowerSpawned"
        (match events with
         | [ TowerSpawned(t, c) ] -> t.Type = Archer && c = at 0 0
         | _ -> false)

    let state2, events2 = state |> run [ SpawnTower(Cannon, at 0 0) ]
    check "spawn on occupied cell is rejected" (hasReject (SpawnCellOccupied(at 0 0)) events2)
    check "rejected spawn leaves state untouched" (state2 = state)

    check "grid reports a single tower" (Grid.towerCount state.Grid = 1)
    check "fresh grid is not full" (Grid.isFull state.Grid = false)

    let ids =
        [ SpawnTower(Archer, at 1 0); SpawnTower(Archer, at 1 1) ]
        |> fun msgs -> run msgs state
        |> fun (s, _) -> Grid.towers s.Grid |> List.map (fun (_, t) -> TowerId.value t.Id)

    check "tower ids are unique" (List.distinct ids = ids && List.length ids = 3)

// ---------------------------------------------------------------------------
// Drag state machine
// ---------------------------------------------------------------------------

let private testDragMachine () =
    let base_, _ = fresh () |> run [ SpawnTower(Archer, at 2 2) ]

    // StartDrag on empty cell
    let s, evs = base_ |> run [ StartDrag(at 0 0) ]
    check "drag from empty cell is rejected" (hasReject (OriginEmpty(at 0 0)) evs)
    check "rejected drag keeps state Idle" (s.Interaction = Idle)

    // StartDrag on occupied cell lifts the tower off the grid
    let dragging, evs = base_ |> run [ StartDrag(at 2 2) ]

    check "drag lifts tower into DragState"
        (match dragging.Interaction with
         | Dragging d -> d.Origin = at 2 2 && d.Tower.Type = Archer
         | Idle -> false)

    check "drag origin becomes empty" (Grid.cellAt (at 2 2) dragging.Grid = Empty)
    check "drag emits DragBegan"
        (match evs with
         | [ DragBegan(t, c) ] -> t.Type = Archer && c = at 2 2
         | _ -> false)

    // Second StartDrag while dragging
    let _, evs = dragging |> run [ StartDrag(at 2 2) ]
    check "drag while dragging is rejected" (hasReject AlreadyDragging evs)

    // Drop / Cancel with no drag in flight
    let _, evs = base_ |> run [ Drop(at 0 0) ]
    check "drop while idle is rejected" (hasReject NotDragging evs)
    let _, evs = base_ |> run [ CancelDrag ]
    check "cancel while idle is rejected" (hasReject NotDragging evs)

    // Spawning/buying is frozen during a drag (keeps the origin provably empty)
    let s, evs = dragging |> run [ SpawnTower(Cannon, at 2 2) ]
    check "spawn during drag is rejected" (hasReject SpawnWhileDragging evs)
    check "spawn during drag changes nothing" (s = dragging)
    let _, evs = dragging |> run [ BuyTower(Cannon, at 2 2) ]
    check "buy during drag is rejected" (hasReject SpawnWhileDragging evs)

    // Cancel restores the tower to its origin
    let s, evs = dragging |> run [ CancelDrag ]
    check "cancel returns tower to origin"
        (match Grid.cellAt (at 2 2) s.Grid with
         | Occupied t -> t.Type = Archer
         | Empty -> false)
    check "cancel goes back to Idle" (s.Interaction = Idle)
    check "cancel emits TowerReturned"
        (match evs with
         | [ TowerReturned(_, c) ] -> c = at 2 2
         | _ -> false)

    // Drop on the origin itself is a plain return
    let s, evs = dragging |> run [ Drop(at 2 2) ]
    check "drop on origin returns tower"
        (Grid.cellAt (at 2 2) s.Grid <> Empty && s.Interaction = Idle)
    check "drop on origin emits TowerReturned"
        (match evs with
         | [ TowerReturned _ ] -> true
         | _ -> false)

    // Drop on an empty cell moves the tower
    let s, evs = dragging |> run [ Drop(at 4 4) ]
    check "drop on empty cell moves tower"
        (Grid.cellAt (at 2 2) s.Grid = Empty
         && (match Grid.cellAt (at 4 4) s.Grid with
             | Occupied t -> t.Type = Archer
             | Empty -> false))
    check "move emits TowerMoved"
        (match evs with
         | [ TowerMoved(_, o, t) ] -> o = at 2 2 && t = at 4 4
         | _ -> false)
    check "move keeps exactly one tower" (Grid.towerCount s.Grid = 1)

// ---------------------------------------------------------------------------
// Merging
// ---------------------------------------------------------------------------

let private testMerging () =
    // Same type + same level → merge into next level
    let s, evs =
        fresh ()
        |> run
            [ SpawnTower(Archer, at 0 0)
              SpawnTower(Archer, at 0 1)
              StartDrag(at 0 0)
              Drop(at 0 1) ]

    check "merge yields Level2 tower at target"
        (match Grid.cellAt (at 0 1) s.Grid with
         | Occupied t -> t.Type = Archer && t.Level = Level2
         | Empty -> false)

    check "merge leaves origin empty" (Grid.cellAt (at 0 0) s.Grid = Empty)
    check "merge leaves exactly one tower" (Grid.towerCount s.Grid = 1)
    check "merge returns to Idle" (s.Interaction = Idle)

    check "merge emits TowersMerged with fresh id"
        (evs
         |> List.exists (fun e ->
             match e with
             | TowersMerged(a, b, result, c) ->
                 c = at 0 1
                 && result.Level = Level2
                 && result.Id <> a.Id
                 && result.Id <> b.Id
             | _ -> false))

    // Different types never merge
    let s, evs =
        fresh ()
        |> run
            [ SpawnTower(Archer, at 0 0)
              SpawnTower(Cannon, at 0 1)
              StartDrag(at 0 0)
              Drop(at 0 1) ]

    check "different types do not merge" (hasReject (IncompatibleTarget(at 0 1)) evs)
    check "rejected merge returns tower to origin"
        (match Grid.cellAt (at 0 0) s.Grid with
         | Occupied t -> t.Type = Archer
         | Empty -> false)
    check "rejected merge keeps both towers" (Grid.towerCount s.Grid = 2)

    // Different levels never merge
    let s, evs =
        fresh ()
        |> run
            [ SpawnTower(Frost, at 0 0)
              SpawnTower(Frost, at 0 1)
              SpawnTower(Frost, at 1 0)
              StartDrag(at 0 0)
              Drop(at 0 1) // Frost Level2 at (0,1)
              StartDrag(at 1 0)
              Drop(at 0 1) ] // Level1 onto Level2 → reject

    check "different levels do not merge" (hasReject (IncompatibleTarget(at 0 1)) evs)
    check "level mismatch keeps both towers" (Grid.towerCount s.Grid = 2)

// ---------------------------------------------------------------------------
// Max-level ceiling, reached only through legitimate merge chains
// ---------------------------------------------------------------------------

/// Keep merging any mergeable pair on the grid until none is left.
let rec private mergeDown (state: GameState) : GameState =
    let towers = Grid.towers state.Grid

    let pair =
        towers
        |> List.tryPick (fun (c1, t1) ->
            towers
            |> List.tryPick (fun (c2, t2) ->
                if c1 <> c2 && Tower.canMerge t1 t2 |> Option.isSome then
                    Some(c1, c2)
                else
                    None))

    match pair with
    | None -> state
    | Some(a, b) ->
        let state', _ = run [ StartDrag a; Drop b ] state
        mergeDown state'

let private testMaxLevel () =
    // 32 Level1 archers on an 8x8 board merge down to two Level5 towers.
    let size8 = gridSize 8
    let coord8 r c = coordIn size8 r c

    let spawns =
        [ for i in 0 .. 31 -> SpawnTower(Archer, coord8 (i / 8) (i % 8)) ]

    let filled, _ = GameState.create (dummyLevel size8) Talents.empty |> run spawns
    let merged = mergeDown filled

    let levels =
        Grid.towers merged.Grid |> List.map (fun (_, t) -> t.Level)

    check "merge chain reduces 32 towers to 2" (List.length levels = 2)
    check "merge chain reaches Level5" (levels = [ Level5; Level5 ])

    // Two max-level towers refuse to merge and the drag resolves cleanly.
    match Grid.towers merged.Grid |> List.map fst with
    | [ a; b ] ->
        let s, evs = merged |> run [ StartDrag a; Drop b ]
        check "max-level merge is rejected" (hasReject (MergeAtMaxLevel b) evs)
        check "max-level towers both survive" (Grid.towerCount s.Grid = 2)
        check "max-level reject returns to Idle" (s.Interaction = Idle)
        check "max-level reject returns tower to origin" (Grid.cellAt a s.Grid <> Empty)
    | _ -> check "expected exactly two towers after mergeDown" false

// ---------------------------------------------------------------------------
// previewDrop stays consistent with the real Drop transition
// ---------------------------------------------------------------------------

let private testPreviewConsistency () =
    let state, _ =
        fresh ()
        |> run
            [ SpawnTower(Archer, at 0 0) // dragged
              SpawnTower(Archer, at 0 1) // merge partner
              SpawnTower(Cannon, at 0 2) // incompatible
              StartDrag(at 0 0) ]

    check "preview is None while idle" (previewDrop (at 0 0) (fresh ()) = None)

    let classifyPreview target =
        match previewDrop target state with
        | Some MoveHere -> "move"
        | Some(MergeHere _) -> "merge"
        | Some ReturnToOrigin -> "return"
        | Some Blocked -> "blocked"
        | None -> "none"

    let classifyDrop target =
        let _, evs = update (Drop target) state

        if evs |> List.exists (fun e -> match e with TowersMerged _ -> true | _ -> false) then "merge"
        elif evs |> List.exists (fun e -> match e with TowerMoved _ -> true | _ -> false) then "move"
        elif evs |> List.exists (fun e -> match e with ActionRejected _ -> true | _ -> false) then "blocked"
        else "return"

    let allAgree =
        Grid.coords state.Grid
        |> List.forall (fun c -> classifyPreview c = classifyDrop c)

    check "previewDrop matches update on every cell" allAgree
    check "preview flags merge target" (classifyPreview (at 0 1) = "merge")
    check "preview flags blocked target" (classifyPreview (at 0 2) = "blocked")
    check "preview flags origin return" (classifyPreview (at 0 0) = "return")

// ---------------------------------------------------------------------------
// Enemy movement and lives
// ---------------------------------------------------------------------------

let private testEnemies () =
    let state, evs =
        fresh () |> noWaves |> run [ SpawnEnemy Bandit; SpawnEnemy Cavalry ]

    check "spawned enemies are tracked" (List.length state.Enemies = 2)
    check "enemies spawn at path start"
        (state.Enemies
         |> List.forall (fun e -> PathProgress.value e.Progress = 0.0))
    check "spawn emits EnemySpawned"
        (evs |> List.forall (fun e -> match e with EnemySpawned _ -> true | _ -> false))

    // Small step: everyone advances, nobody exits, no combat, no waves
    let s, evs = state |> run [ Tick(dt 1.0) ]
    check "small tick keeps all enemies" (List.length s.Enemies = 2 && evs = [])
    check "small tick moves every enemy"
        (s.Enemies |> List.forall (fun e -> PathProgress.value e.Progress > 0.0))
    check "faster type is further along"
        (match s.Enemies with
         | [ grunt; runner ] -> PathProgress.value runner.Progress > PathProgress.value grunt.Progress
         | _ -> false)

    // Huge step: everyone reaches the goal, costing lives
    let s, evs = state |> run [ Tick(dt 1000.0) ]
    check "reaching the goal removes enemies" (s.Enemies = [])
    check "each exit emits EnemyReachedGoal"
        ((evs |> List.filter (fun e -> match e with EnemyReachedGoal _ -> true | _ -> false) |> List.length) = 2)
    check "each exit costs a life" (livesOf s = startingLives - 2)
    check "life loss is reported"
        (evs |> List.contains (LifeLost(startingLives - 2)))

    // Damage via the debug message: survive, then die (with bounty)
    let grunt = List.head state.Enemies // 20 hp

    let s, evs = state |> run [ HitEnemy(grunt.Id, dmg 15) ]
    check "wounded enemy survives with reduced hp"
        (match evs with
         | [ EnemyDamaged(id, _, remaining) ] -> id = grunt.Id && Health.value remaining = 5
         | _ -> false)
    check "wounded enemy stays on the field" (List.length s.Enemies = 2)

    let s2, evs = s |> run [ HitEnemy(grunt.Id, dmg 5) ]
    check "lethal damage kills with bounty"
        (match evs with
         | [ EnemyDamaged(_); EnemyKilled(id, bounty) ] -> id = grunt.Id && bounty = EnemyType.bounty Bandit
         | _ -> false)
    check "killed enemy is removed" (List.length s2.Enemies = 1)
    check "kill pays the bounty" (goldOf s2 = goldOf s + EnemyType.bounty Bandit)

    let _, evs = s2 |> run [ HitEnemy(grunt.Id, dmg 1) ]
    check "hitting a dead enemy is rejected" (hasReject (UnknownEnemy grunt.Id) evs)

    check "overkill also kills"
        (match state |> run [ HitEnemy(grunt.Id, dmg 9999) ] with
         | _, [ EnemyDamaged(_); EnemyKilled _ ] -> true
         | _ -> false)

// ---------------------------------------------------------------------------
// Wave scheduler and difficulty curve
// ---------------------------------------------------------------------------

let private testWaves () =
    // The first wave starts after the initial delay and spawns over time.
    let s, evs = fresh () |> ticks 80 0.1 // 8 seconds
    check "first wave starts after the initial delay" (s.Wave.Number = 1)
    check "wave start is announced" (evs |> List.contains (WaveStarted 1))
    check "wave spawns enemies over time"
        ((evs |> List.filter (fun e -> match e with EnemySpawned _ -> true | _ -> false) |> List.length) >= 2)

    // A single huge tick spawns the whole wave (spawn credit drains); with
    // no towers everything leaks and the wave completes within that tick.
    let _, evs2 = fresh () |> run [ Tick(dt 60.0) ]
    check "long tick drains the entire spawn queue"
        ((evs2 |> List.filter (fun e -> match e with EnemySpawned _ -> true | _ -> false) |> List.length) =
            List.length (Waves.composition 1))

    // Difficulty curve: composition grows and health scales.
    check "wave 1 is grunts only"
        (Waves.composition 1 |> List.forall (fun t -> t = Bandit))
    check "later waves add runners" (Waves.composition 3 |> List.contains Cavalry)
    check "later waves add tanks" (Waves.composition 4 |> List.contains Brute)
    check "every fifth wave has a boss" (Waves.composition 5 |> List.contains Necromancer)
    check "waves grow over time"
        (List.length (Waves.composition 8) > List.length (Waves.composition 1))
    check "health multiplier grows" (Waves.healthMultiplier 6 > Waves.healthMultiplier 1)
    check "spawn interval shrinks but stays positive"
        (Waves.spawnInterval 20 < Waves.spawnInterval 1 && Waves.spawnInterval 99 > 0.0)

    // Scaled health is applied to spawned enemies.
    let sw, _ = fresh () |> run [ Tick(dt 5.01) ]
    check "wave enemies use scaled health"
        (sw.Enemies
         |> List.forall (fun e -> Health.value e.Health >= EnemyType.baseHealth e.Type))

// ---------------------------------------------------------------------------
// Combat: targeting and shooting
// ---------------------------------------------------------------------------

let private testCombat () =
    // Archer at (0,0) covers the path entry; a grunt walks into its range.
    let armed, _ = fresh () |> noWaves |> run [ SpawnTower(Archer, at 0 0); SpawnEnemy Bandit ]

    let afterShot, evs = armed |> run [ Tick(dt 0.05) ]
    check "ready tower fires immediately"
        (evs |> List.exists (fun e -> match e with TowerFired _ -> true | _ -> false))
    check "shot damages the target"
        (evs |> List.exists (fun e -> match e with EnemyDamaged _ -> true | _ -> false))

    let towerOf (s: GameState) =
        match Grid.cellAt (at 0 0) s.Grid with
        | Occupied t -> t
        | Empty -> failwith "tower vanished"

    check "firing starts the cooldown" ((towerOf afterShot).Cooldown > 0.0)

    let _, evs2 = afterShot |> run [ Tick(dt 0.05) ]
    check "cooling tower holds fire"
        (not (evs2 |> List.exists (fun e -> match e with TowerFired _ -> true | _ -> false)))

    // Sustained fire kills the grunt and pays the bounty.
    let cleared, evsAll = armed |> ticks 40 0.1 // 4 seconds of combat
    check "sustained fire kills the enemy"
        (evsAll |> List.exists (fun e -> match e with EnemyKilled _ -> true | _ -> false))
    check "the field is cleared" (cleared.Enemies = [])
    check "combat pays the bounty" (goldOf cleared = startingGold + EnemyType.bounty Bandit)
    check "no lives were lost" (livesOf cleared = startingLives)

    // Out-of-range towers never fire: bottom-left corner is far from the path.
    let idle, evsIdle =
        fresh () |> noWaves
        |> run [ SpawnTower(Cannon, at 4 0); SpawnEnemy Bandit ]
        |> fst
        |> ticks 10 0.1

    check "out-of-range tower holds fire"
        (not (evsIdle |> List.exists (fun e -> match e with TowerFired _ -> true | _ -> false)))
    check "unharassed enemy keeps walking" (List.length idle.Enemies = 1)

    // "First" targeting: the enemy furthest along the path is hit first.
    let twoEnemies, _ =
        fresh () |> noWaves |> run [ SpawnEnemy Bandit ]
        |> fst
        |> run [ Tick(dt 1.0) ] // let the first grunt walk ahead
        |> fst
        |> run [ SpawnEnemy Bandit; SpawnTower(Archer, at 0 0) ]

    let leader =
        twoEnemies.Enemies |> List.maxBy (fun e -> PathProgress.value e.Progress)

    let _, evsTarget = twoEnemies |> run [ Tick(dt 0.05) ]
    check "targeting picks the leading enemy"
        (evsTarget
         |> List.exists (fun e ->
             match e with
             | EnemyDamaged(id, _, _) -> id = leader.Id
             | _ -> false))

// ---------------------------------------------------------------------------
// Economy: buying towers, wave bonuses
// ---------------------------------------------------------------------------

let private testEconomy () =
    let s1, evs1 = fresh () |> run [ BuyTower(Archer, at 0 0) ]
    check "buy places a Level1 tower"
        (match Grid.cellAt (at 0 0) s1.Grid with
         | Occupied t -> t.Type = Archer && t.Level = Level1
         | Empty -> false)
    check "buy charges the base cost" (goldOf s1 = startingGold - towerBaseCost)
    check "buy is reported with its cost"
        (evs1
         |> List.exists (fun e ->
             match e with
             | TowerBought(_, c, cost) -> c = at 0 0 && cost = towerBaseCost
             | _ -> false))

    check "tower prices escalate" (nextTowerCost s1 = towerBaseCost + towerCostGrowth)

    let s2, evs2 = s1 |> run [ BuyTower(Cannon, at 0 0) ]
    check "buy on an occupied cell is rejected" (hasReject (SpawnCellOccupied(at 0 0)) evs2)
    check "rejected buy does not charge" (goldOf s2 = goldOf s1 && s2.TowersBought = s1.TowersBought)

    // Drain the purse: 110 gold buys towers at 20/24/28/32; the fifth (36) fails.
    let coords = [ at 0 0; at 0 1; at 0 2; at 0 3; at 0 4 ]
    let drained, evsD = fresh () |> run [ for c in coords -> BuyTower(Archer, c) ]
    check "gold runs out on the fifth tower" (hasReject (NotEnoughGold 36) evsD)
    check "only four towers were bought" (Grid.towerCount drained.Grid = 4)
    check "remaining balance is correct" (goldOf drained = 110 - 20 - 24 - 28 - 32)

    // Clearing a wave pays the completion bonus.
    let beforeClear, _ =
        fresh () |> noWaves |> run [ SpawnEnemy Bandit; SpawnTower(Archer, at 0 0) ]

    let waveActive =
        { beforeClear with Wave = { Number = 1; Phase = WaveActive } }

    let cleared, evsC = waveActive |> ticks 40 0.1
    check "clearing the wave pays the bonus"
        (evsC |> List.contains (WaveCompleted(1, Waves.completionBonus 1)))
    check "bonus lands in the purse"
        (goldOf cleared = startingGold + EnemyType.bounty Bandit + Waves.completionBonus 1)
    check "next wave countdown starts"
        (match cleared.Wave.Phase with
         | BetweenWaves _ -> true
         | _ -> false)

// ---------------------------------------------------------------------------
// Game over
// ---------------------------------------------------------------------------

let private testGameOver () =
    // Ten grunt leaks exhaust the starting lives.
    let flooded, _ =
        fresh () |> noWaves |> run [ for _ in 1 .. startingLives -> SpawnEnemy Bandit ]

    let ended, evs = flooded |> run [ Tick(dt 1000.0) ]

    check "losing every life ends the game"
        (match ended.Status with
         | Defeated _ -> true
         | Playing _ -> false)
    check "game over is announced"
        (evs |> List.exists (fun e -> match e with GameOver _ -> true | _ -> false))

    // The tableau freezes: actions are rejected, time is a no-op.
    let s, evsDrag = ended |> run [ StartDrag(at 0 0) ]
    check "actions after defeat are rejected" (hasReject GameAlreadyOver evsDrag)
    let s2, evsTick = s |> run [ Tick(dt 1.0) ]
    check "ticks after defeat change nothing" (s2 = s && evsTick = [])
    let _, evsBuy = s2 |> run [ BuyTower(Archer, at 0 0) ]
    check "buying after defeat is rejected" (hasReject GameAlreadyOver evsBuy)

// ---------------------------------------------------------------------------
// Immutability spot checks
// ---------------------------------------------------------------------------

let private testImmutability () =
    let before = fresh ()
    let after, _ = before |> run [ SpawnTower(Archer, at 0 0); SpawnEnemy MegaBoss ]

    check "update never mutates the old state"
        (Grid.towerCount before.Grid = 0
         && before.Enemies = []
         && Grid.towerCount after.Grid = 1
         && List.length after.Enemies = 1)

    let dragging, _ = after |> run [ StartDrag(at 0 0) ]
    check "lifting does not mutate the previous grid"
        (Grid.cellAt (at 0 0) after.Grid <> Empty
         && Grid.cellAt (at 0 0) dragging.Grid = Empty)

// ---------------------------------------------------------------------------
// Derived stats
// ---------------------------------------------------------------------------

let private testStats () =
    let idA, gen = TowerIdGen.next TowerIdGen.initial
    let idB, _ = TowerIdGen.next gen

    let archer1 =
        { Id = idA; Type = Archer; Level = Level1; Cooldown = 0.0 }

    let archer3 =
        { Id = idB; Type = Archer; Level = Level3; Cooldown = 0.0 }

    check "stats scale with level"
        ((Tower.stats archer3 Talents.empty).Damage = 4 * (Tower.stats archer1 Talents.empty).Damage)
    check "range grows with level"
        ((Tower.stats archer3 Talents.empty).Range > (Tower.stats archer1 Talents.empty).Range)
    check "same tower cannot merge with itself" (Tower.canMerge archer1 archer1 = None)
    check "attack damage matches stats"
        (Damage.value (Tower.attackDamage archer3 Talents.empty) = (Tower.stats archer3 Talents.empty).Damage)

// ---------------------------------------------------------------------------
// UI layer: layout math, hit testing, HUD transitions
// ---------------------------------------------------------------------------

let private uiLayoutTests () =
    let path5 = Path.defaultFor size5
    let layout = layoutFor size5 path5

    // cellAtPoint must be the exact inverse of cellCenter on every cell.
    let allRoundTrip =
        GameState.create (dummyLevel size5) Talents.empty
        |> fun s -> Grid.coords s.Grid
        |> List.forall (fun coord ->
            let x, y = cellCenter layout coord
            cellAtPoint layout size5 x y = Some coord)

    check "cellAtPoint inverts cellCenter on every cell" allRoundTrip
    check "point left of grid maps to no cell" (cellAtPoint layout size5 (layout.GridLeft - 5.0) layout.GridTop = None)
    check "point above grid maps to no cell" (cellAtPoint layout size5 layout.GridLeft (layout.GridTop - 5.0) = None)
    check "point past last cell maps to no cell"
        (cellAtPoint layout size5 (layout.GridLeft + 5.0 * layout.CellSize + 1.0) (layout.GridTop + 1.0) = None)

    // The canvas must contain the whole path, lane width included.
    let containsPoint (x, y) =
        let px, py = toPx layout (x, y)
        px >= 0.0 && px <= layout.CanvasWidth && py >= 0.0 && py <= layout.CanvasHeight

    check "canvas contains every path waypoint"
        (Path.waypoints path5 |> List.forall containsPoint)

    check "enemy at path start renders at the first waypoint"
        (let e, _ = Enemy.spawn EnemyIdGen.initial Bandit
         Enemy.positionOn path5 e = List.head (Path.waypoints path5))

let private uiHudTests () =
    let path5 = Path.defaultFor size5
    let layout = layoutFor size5 path5
    let model = init CampaignState.empty (Levels.get 1 |> Option.get) 1 |> updateUi layout StartGame

    // Buying through the HUD: first empty cell, gold drawn from the core.
    let m1 = updateUi layout (Buy Archer) model
    check "HUD buy places a tower on the first empty cell"
        (match Grid.cellAt (at 0 0) m1.Game.Grid with
         | Occupied t -> t.Type = Archer && t.Level = Level1
         | Empty -> false)
    check "HUD buy deducts gold" (goldOf m1.Game = startingGold - towerBaseCost)

    check "cannot buy while dragging"
        (let dragging = updateUi layout (GameMsg(StartDrag(at 0 0))) m1
         canBuy dragging = false)

    check "cannot buy when broke"
        (let broke =
            [ 1 .. 4 ] |> List.fold (fun m _ -> updateUi layout (Buy Archer) m) model
         canBuy broke = false)

    // Frame: injected time drives the core scheduler.
    let after6s =
        [ 1 .. 60 ] |> List.fold (fun m _ -> updateUi layout (Frame(dt 0.1)) m) model

    check "frames drive the wave scheduler" (after6s.Game.Wave.Number = 1)
    check "wave start raises a HUD notice" (after6s.Notice |> Option.isSome)

    // Shot tracers appear when towers fire and fade out.
    let combatModel =
        { model with Game = fst (run [ SpawnTower(Archer, at 0 0); SpawnEnemy Bandit ] (noWaves model.Game)) }

    let firing = updateUi layout (Frame(dt 0.05)) combatModel
    check "tower fire leaves a shot tracer" (not (List.isEmpty firing.Shots))
    let faded = updateUi layout (Frame(dt 0.5)) firing
    check "shot tracers fade out" (List.isEmpty faded.Shots)

    // Notices: set by noteworthy events, silent otherwise, and they expire.
    let mismatch =
        [ GameMsg(SpawnTower(Archer, at 3 0))
          GameMsg(SpawnTower(Cannon, at 3 1))
          GameMsg(StartDrag(at 3 0))
          GameMsg(Drop(at 3 1)) ]
        |> List.fold (fun m msg -> updateUi layout msg m) { model with Game = noWaves model.Game }

    check "incompatible merge raises a HUD notice" (mismatch.Notice |> Option.isSome)
    check "notice expires after its time-to-live"
        (let faded =
            [ 1 .. 4 ] |> List.fold (fun m _ -> updateUi layout (Frame(dt 1.0)) m) mismatch
         faded.Notice = None)

    check "plain pointer movement raises no notice"
        ((updateUi layout (PointerMoved(Some(at 1 1), Some(10.0, 10.0))) model).Notice = None)

    // Restart resets the whole game.
    let restarted = updateUi layout Restart after6s
    check "restart returns to a fresh game"
        (restarted.Game.Wave.Number = 0
         && Grid.towerCount restarted.Game.Grid = 0
         && goldOf restarted.Game = startingGold)

// ---------------------------------------------------------------------------
// Frost slow tests (Phase 4)
// ---------------------------------------------------------------------------

let private testFrostSlow () =
    // Place a frost tower and an enemy, then tick. The frost tower should
    // slow the enemy after hitting it.
    let initial = fresh () |> noWaves
    let state, _ = run [ SpawnTower(Frost, at 0 0); SpawnEnemy Bandit ] initial

    // Tick until the frost tower fires (its cooldown is 0 at spawn).
    let state1, events1 = ticks 1 0.05 state

    // The frost tower should have fired and the enemy should be slowed.
    let hasFired = events1 |> List.exists (fun e ->
        match e with | TowerFired _ -> true | _ -> false)
    check "frost tower fires at enemy" hasFired

    let hasSlowed = events1 |> List.exists (fun e ->
        match e with | EnemySlowed _ -> true | _ -> false)
    check "frost hit produces EnemySlowed event" hasSlowed

    // The enemy's SlowUntil should be positive.
    let slowedEnemy = state1.Enemies |> List.tryHead
    check "enemy has SlowUntil > 0 after frost hit"
        (match slowedEnemy with
         | Some e -> e.SlowUntil > 0.0
         | None -> false)

    // Movement under slow is reduced: advance the enemy for 1s while slowed
    // and compare with an unslowed enemy.
    let unslowedEnemy, _ = Enemy.spawn EnemyIdGen.initial Bandit
    let pathForTest = Path.defaultFor size5
    let dtOne = dt 1.0
    let slowedResult = Enemy.advance pathForTest dtOne { unslowedEnemy with SlowUntil = 1.0 }
    let normalResult = Enemy.advance pathForTest dtOne unslowedEnemy

    check "slowed enemy moves less than normal"
        (match slowedResult, normalResult with
         | Moved sp, Moved np -> PathProgress.value sp < PathProgress.value np
         | _ -> false)

    // Slow decays after its duration.
    let tickedSlow = Enemy.tickCooldowns 2.0 { unslowedEnemy with SlowUntil = 1.5 }
    check "slow timer decays to zero" (tickedSlow.SlowUntil = 0.0)

// ---------------------------------------------------------------------------
// Tower sell tests (Phase 4)
// ---------------------------------------------------------------------------

let private testSellTower () =
    let initial = fresh () |> noWaves
    let state, _ = run [ SpawnTower(Archer, at 0 0) ] initial

    // Sell the tower.
    let state1, events1 = update (SellTower(at 0 0)) state
    check "sold tower is removed from grid"
        (Grid.cellAt (at 0 0) state1.Grid = Empty)
    check "sell earns a refund"
        (goldOf state1 > goldOf state)
    check "sell emits TowerSold event"
        (events1 |> List.exists (fun e ->
            match e with | TowerSold _ -> true | _ -> false))

    // Sell on empty cell is rejected.
    let _, events2 = update (SellTower(at 1 1)) state
    check "sell empty cell is rejected"
        (hasReject (OriginEmpty(at 1 1)) events2)

    // Sell while dragging is rejected.
    let dragging, _ = update (StartDrag(at 0 0)) state
    let _, events3 = update (SellTower(at 0 0)) dragging
    check "sell while dragging is rejected"
        (hasReject SpawnWhileDragging events3)

    // Sell value scales with level: level 1 gives 60% of 20 = 12.
    let sellRefund = sellValue { Id = fst (TowerIdGen.next TowerIdGen.initial); Type = Archer; Level = Level1; Cooldown = 0.0 }
    check "sell value for L1 tower is 12" (sellRefund = 12)

// ---------------------------------------------------------------------------
// Particle generation tests (Phase 4)
// ---------------------------------------------------------------------------

let private testParticles () =
    let path5 = Path.defaultFor size5
    let layout = layoutFor size5 path5
    let model = init CampaignState.empty (Levels.get 1 |> Option.get) 1 |> updateUi layout StartGame

    // Merge two archers and check that particles are generated.
    let withTowers =
        [ GameMsg(SpawnTower(Archer, at 0 0))
          GameMsg(SpawnTower(Archer, at 0 1)) ]
        |> List.fold (fun m msg -> updateUi layout msg m) { model with Game = noWaves model.Game }

    let merged =
        [ GameMsg(StartDrag(at 0 0)); GameMsg(Drop(at 0 1)) ]
        |> List.fold (fun m msg -> updateUi layout msg m) withTowers

    check "merge produces sparkle particles" (not (List.isEmpty merged.Particles))

// ---------------------------------------------------------------------------
// Entry point
// ---------------------------------------------------------------------------

[<EntryPoint>]
let main _argv =
    testSmartConstructors ()
    testPath ()
    testSpawning ()
    testDragMachine ()
    testMerging ()
    testMaxLevel ()
    testPreviewConsistency ()
    testEnemies ()
    testWaves ()
    testCombat ()
    testEconomy ()
    testGameOver ()
    testImmutability ()
    testStats ()
    uiLayoutTests ()
    uiHudTests ()
    testFrostSlow ()
    testSellTower ()
    testParticles ()

    printfn ""
    printfn "%d passed, %d failed" passed failed

    if failed > 0 then
        failwith "test suite failed"

    0
