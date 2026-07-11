/// Domain model for Merge Tower Defense.
///
/// Pure data only: no IO, no rendering, no mutation, no clock.
/// Design rule — make illegal states unrepresentable: every validated value
/// has a `private` representation and can only be produced through the smart
/// constructors in this file, so an invalid instance has no way to exist.
module MergeTowerDefense.Shared

// ---------------------------------------------------------------------------
// Identifiers
// ---------------------------------------------------------------------------

/// Opaque tower identifier. It can only be minted through TowerIdGen, so a
/// hand-crafted or duplicated id is unrepresentable.
type TowerId = private TowerId of int

module TowerId =
    let value (TowerId n) = n

/// Monotonic id source, threaded through the game state (pure, no globals).
type TowerIdGen = private TowerIdGen of int

module TowerIdGen =
    let initial = TowerIdGen 1
    let next (TowerIdGen n) = TowerId n, TowerIdGen(n + 1)

type EnemyId = private EnemyId of int

module EnemyId =
    let value (EnemyId n) = n

type EnemyIdGen = private EnemyIdGen of int

module EnemyIdGen =
    let initial = EnemyIdGen 1
    let next (EnemyIdGen n) = EnemyId n, EnemyIdGen(n + 1)

// ---------------------------------------------------------------------------
// Combat primitives
// ---------------------------------------------------------------------------

/// Strictly positive damage.
type Damage = private Damage of int

module Damage =
    let tryCreate n = if n > 0 then Some(Damage n) else None
    let value (Damage n) = n

/// Strictly positive hit points. "Alive with zero or negative HP" has no
/// representation; death is an explicit outcome of applyDamage, not a flag.
type Health = private Health of int

module Health =
    let tryCreate n = if n > 0 then Some(Health n) else None
    let value (Health n) = n

type AttackResult =
    | Survived of Health
    | Killed

module AttackResult =
    let ofDamage (Damage dmg) (Health hp) =
        let remaining = hp - dmg
        if remaining > 0 then Survived(Health remaining) else Killed

// ---------------------------------------------------------------------------
// Economy primitives (Phase 3)
// ---------------------------------------------------------------------------

/// Non-negative gold balance: debt is unrepresentable. Spending is a
/// fallible operation, earning is total.
type Gold = private Gold of int

module Gold =
    let zero = Gold 0
    let value (Gold n) = n

    /// Adds a non-negative amount (negative amounts are ignored).
    let earn (amount: int) (Gold n) = Gold(n + max 0 amount)

    /// None when the price exceeds the balance.
    let trySpend (price: int) (Gold n) =
        if price >= 0 && price <= n then Some(Gold(n - price)) else None

/// Strictly positive life counter. "Playing with zero lives" has no
/// representation: running out is the explicit AllLost transition, which the
/// state machine turns into the Defeated status.
type Lives = private Lives of int

type LivesResult =
    | StillAlive of Lives
    | AllLost

module Lives =
    /// Total constructor: clamps to at least one life.
    let create (n: int) = Lives(max 1 n)
    let value (Lives n) = n

    let lose (amount: int) (Lives n) =
        let remaining = n - max 0 amount
        if remaining > 0 then StillAlive(Lives remaining) else AllLost

// ---------------------------------------------------------------------------
// Towers
// ---------------------------------------------------------------------------

/// Tower level as an enumeration instead of an int: level 0, level 42 or a
/// negative level simply has no representation, and the merge chain gets a
/// type-level ceiling.
type TowerLevel =
    | Level1
    | Level2
    | Level3
    | Level4
    | Level5

module TowerLevel =
    let maxLevel = Level5

    /// The level a successful merge produces. None at the ceiling.
    let next =
        function
        | Level1 -> Some Level2
        | Level2 -> Some Level3
        | Level3 -> Some Level4
        | Level4 -> Some Level5
        | Level5 -> None

    /// 1-based rank, for stat scaling and UI.
    let rank =
        function
        | Level1 -> 1
        | Level2 -> 2
        | Level3 -> 3
        | Level4 -> 4
        | Level5 -> 5

type TowerType =
    | Archer
    | Cannon
    | Frost

type Tower =
    { Id: TowerId
      Type: TowerType
      Level: TowerLevel
      /// Seconds until the tower may fire again. State.update keeps this
      /// non-negative; fresh and freshly merged towers start ready (0.0).
      Cooldown: float }

// ---------------------------------------------------------------------------
// Talents
// ---------------------------------------------------------------------------

type Talents =
    { StartingGoldLevel: int
      ArcherDamageLevel: int
      CannonDamageLevel: int
      SpellCooldownLevel: int }

module Talents =
    let empty =
        { StartingGoldLevel = 0
          ArcherDamageLevel = 0
          CannonDamageLevel = 0
          SpellCooldownLevel = 0 }
    
    let startingGold t =
        110 + t.StartingGoldLevel * 20

    let spellCooldownMult t =
        1.0 - (float t.SpellCooldownLevel * 0.05)

type TowerStats =
    { Damage: int
      /// Attack radius in cell units.
      Range: float
      CooldownMs: int }

module Tower =
    let private baseStats =
        function
        | Archer -> { Damage = 4; Range = 3.0; CooldownMs = 600 }
        | Cannon -> { Damage = 10; Range = 2.0; CooldownMs = 1500 }
        | Frost -> { Damage = 2; Range = 2.5; CooldownMs = 900 }

    /// Stats derive from type + level and are never stored, so they can
    /// never disagree with the tower they describe.
    let stats (tower: Tower) (talents: Talents) =
        let b = baseStats tower.Type
        let r = TowerLevel.rank tower.Level

        let baseDamage =
            match tower.Type with
            | Archer -> b.Damage + (talents.ArcherDamageLevel * 2)
            | Cannon -> b.Damage + (talents.CannonDamageLevel * 4)
            | Frost -> b.Damage

        { b with
            Damage = baseDamage * pown 2 (r - 1)
            Range = b.Range + 0.25 * float (r - 1) }

    /// Attack damage as a validated Damage value. Total by construction:
    /// base damages are strictly positive and doubling keeps them positive.
    let attackDamage (tower: Tower) (talents: Talents) : Damage = Damage((stats tower talents).Damage)

    /// Two towers merge iff they are distinct, same type and same level, and
    /// below the ceiling. Returns the level the merged tower would have.
    let canMerge (a: Tower) (b: Tower) =
        if a.Id <> b.Id && a.Type = b.Type && a.Level = b.Level then
            TowerLevel.next a.Level
        else
            None

// ---------------------------------------------------------------------------
// Grid
// ---------------------------------------------------------------------------

/// Validated NxN board size.
type GridSize = private GridSize of int

module GridSize =
    let minSize = 2
    let maxSize = 12

    let tryCreate n =
        if n >= minSize && n <= maxSize then Some(GridSize n) else None

    let value (GridSize n) = n

/// A cell coordinate guaranteed to lie inside the grid it was created for:
/// out-of-bounds coordinates are unrepresentable.
type Coord =
    private
        { Row: int
          Col: int }

module Coord =
    let tryCreate (size: GridSize) row col =
        let n = GridSize.value size

        if row >= 0 && row < n && col >= 0 && col < n then
            Some { Row = row; Col = col }
        else
            None

    let row c = c.Row
    let col c = c.Col

    /// Cell centre in cell units — the coordinate system shared with Path:
    /// the grid's top-left corner is (0,0) and one unit is one cell edge.
    let center (c: Coord) : float * float = float c.Col + 0.5, float c.Row + 0.5

type CellState =
    | Empty
    | Occupied of Tower
    | BlockedCell

/// The board. Absence in the map IS the empty cell — there is no separate
/// "empty" marker that could drift out of sync with the tower map.
type Grid =
    private
        { Size: GridSize
          BlockedCells: Set<Coord>
          Towers: Map<Coord, Tower> }

module Grid =
    let create size blocked = { Size = size; BlockedCells = blocked; Towers = Map.empty }

    let size grid = grid.Size

    let cellAt coord grid =
        if Set.contains coord grid.BlockedCells then
            BlockedCell
        else
            match Map.tryFind coord grid.Towers with
            | Some tower -> Occupied tower
            | None -> Empty

    let tryFindTower coord grid =
        match Map.tryFind coord grid.Towers with
        | Some t -> Some t
        | None -> None

    /// All coordinates of the board, row-major.
    let coords grid =
        let n = GridSize.value grid.Size

        [ for r in 0 .. n - 1 do
              for c in 0 .. n - 1 -> { Row = r; Col = c } ]

    /// All towers on the board with their coordinates, in deterministic
    /// (coordinate) order.
    let towers grid = Map.toList grid.Towers

    let towerCount grid = Map.count grid.Towers

    let isFull grid =
        let n = GridSize.value grid.Size
        Map.count grid.Towers = n * n

    /// Cell-preserving update of every tower on the board (used for combat
    /// cooldown bookkeeping; must not touch identity or placement).
    let mapTowers (f: Tower -> Tower) grid =
        { grid with
            Towers = Map.map (fun _ tower -> f tower) grid.Towers }

    /// Place a tower on an EMPTY cell. An occupied target yields None, so
    /// silently overwriting (losing) a tower is unrepresentable.
    let tryPlace coord tower grid =
        match cellAt coord grid with
        | Occupied _ 
        | BlockedCell -> None
        | Empty ->
            Some
                { grid with
                    Towers = Map.add coord tower grid.Towers }

    /// Lift a tower off the board, returning it together with the grid that
    /// no longer contains it. Lifting from an empty cell yields None.
    let tryLift coord grid =
        match Map.tryFind coord grid.Towers with
        | Some tower ->
            Some(
                tower,
                { grid with
                    Towers = Map.remove coord grid.Towers }
            )
        | None -> None

// ---------------------------------------------------------------------------
// Time, movement and path geometry
// ---------------------------------------------------------------------------

/// A positive, finite time step in seconds. Time is always injected from the
/// outside; the core never reads a clock.
type DeltaTime = private DeltaTime of float

module DeltaTime =
    let tryCreate seconds =
        if seconds > 0.0 && not (System.Double.IsNaN seconds) && seconds < infinity then
            Some(DeltaTime seconds)
        else
            None

    let seconds (DeltaTime s) = s

/// Normalised position along the enemy path, always within [0, 1).
/// Reaching the end is not a state — it is the ReachedGoal transition — so
/// "an enemy standing beyond the exit" is unrepresentable.
type PathProgress = private PathProgress of float

module PathProgress =
    let start = PathProgress 0.0
    let value (PathProgress p) = p

    let tryCreate (fraction: float) =
        if fraction >= 0.0 && fraction < 1.0 && not (System.Double.IsNaN fraction) then
            Some(PathProgress fraction)
        else
            None

type MoveResult =
    | Moved of PathProgress
    | ReachedGoal

/// The polyline enemies walk, in cell units: the grid's top-left corner is
/// (0,0), one unit is one cell edge and cell (row, col) has its centre at
/// (col + 0.5, row + 0.5). Kept abstract from pixels so the core never
/// learns about the canvas. Guaranteed non-degenerate (≥ 2 finite points,
/// positive total length) by construction.
type Path =
    private
        { Points: (float * float) list
          Segments: ((float * float) * (float * float) * float) list
          Total: float }

module Path =
    let private build points =
        let segments =
            List.pairwise points
            |> List.map (fun ((x1, y1), (x2, y2)) ->
                let dx = x2 - x1
                let dy = y2 - y1
                (x1, y1), (x2, y2), sqrt (dx * dx + dy * dy))

        { Points = points
          Segments = segments
          Total = segments |> List.sumBy (fun (_, _, len) -> len) }

    let tryCreate (points: (float * float) list) : Path option =
        let finite v =
            not (System.Double.IsNaN v) && abs v < infinity

        if List.length points < 2
           || not (points |> List.forall (fun (x, y) -> finite x && finite y)) then
            None
        else
            let path = build points
            if path.Total > 0.0 then Some path else None

    /// Total length in cell units.
    let length (path: Path) = path.Total

    let waypoints (path: Path) = path.Points

    /// Axis-aligned bounds of the polyline: minX, minY, maxX, maxY.
    let bounds (path: Path) =
        let xs = path.Points |> List.map fst
        let ys = path.Points |> List.map snd
        List.min xs, List.min ys, List.max xs, List.max ys

    /// Point at a walked distance (clamped to the path ends).
    let pointAtDistance (path: Path) (distance: float) : float * float =
        let rec walk travelled segments =
            match segments with
            | [] -> List.last path.Points
            | ((x1, y1), (x2, y2), len) :: rest ->
                if distance <= travelled + len && len > 0.0 then
                    let t = max 0.0 ((distance - travelled) / len)
                    x1 + (x2 - x1) * t, y1 + (y2 - y1) * t
                else
                    walk (travelled + len) rest

        walk 0.0 path.Segments

    /// Point in cell units at a normalised progress.
    let positionAt (path: Path) (progress: PathProgress) : float * float =
        pointAtDistance path (PathProgress.value progress * path.Total)

    /// Default course: in from the top-left, along the grid's top edge, then
    /// down its right flank to the goal at the bottom-right.
    let defaultFor (size: GridSize) : Path =
        let n = float (GridSize.value size)
        build [ -0.9, -0.9; n + 0.9, -0.9; n + 0.9, n + 0.5 ]

// ---------------------------------------------------------------------------
// Enemies
// ---------------------------------------------------------------------------

type EnemyType =
    | Bandit
    | Cavalry
    | Brute
    | Warlord
    | Necromancer
    | Dragon
    | MegaBoss

module EnemyType =
    /// Base hit points per type. Values must stay strictly positive: they
    /// feed the private Health constructor in Enemy.spawnWith.
    let baseHealth =
        function
        | Bandit -> 20
        | Cavalry -> 12
        | Brute -> 60
        | Warlord -> 150
        | Necromancer -> 250
        | Dragon -> 600
        | MegaBoss -> 1500

    /// Movement speed in cells per second.
    let speed =
        function
        | Bandit -> 0.9
        | Cavalry -> 1.8
        | Brute -> 0.55
        | Warlord -> 0.7
        | Necromancer -> 0.45
        | Dragon -> 0.8
        | MegaBoss -> 0.35

    /// Gold awarded when the enemy is killed.
    let bounty =
        function
        | Bandit -> 4
        | Cavalry -> 6
        | Brute -> 12
        | Warlord -> 30
        | Necromancer -> 50
        | Dragon -> 100
        | MegaBoss -> 250

    /// Lives lost when the enemy reaches the goal.
    let livesCost =
        function
        | Bandit -> 1
        | Cavalry -> 1
        | Brute -> 2
        | Warlord -> 3
        | Necromancer -> 3
        | Dragon -> 5
        | MegaBoss -> 10

type Enemy =
    { Id: EnemyId
      Type: EnemyType
      MaxHealth: Health
      Health: Health
      Progress: PathProgress
      /// Remaining seconds of Frost slow effect. When positive the enemy
      /// moves at half speed. Decremented each tick by State.stepMovement.
      SlowUntil: float
      /// Cooldown until the next minion spawn (if applicable).
      SpawnCooldown: float }

module Enemy =
    /// Spawns at the path start; health = type base × multiplier, kept ≥ 1
    /// so the private Health constructor stays valid.
    let spawnWith (gen: EnemyIdGen) (enemyType: EnemyType) (healthMultiplier: float) : Enemy * EnemyIdGen =
        let id, gen' = EnemyIdGen.next gen

        let hp =
            max 1 (int (round (float (EnemyType.baseHealth enemyType) * healthMultiplier)))

        { Id = id
          Type = enemyType
          MaxHealth = Health hp
          Health = Health hp
          Progress = PathProgress.start
          SlowUntil = 0.0
          SpawnCooldown = match enemyType with MegaBoss -> 6.0 | Necromancer -> 4.0 | _ -> 0.0 },
        gen'

    let spawn (gen: EnemyIdGen) (enemyType: EnemyType) : Enemy * EnemyIdGen = spawnWith gen enemyType 1.0

    /// Time-based movement along the path (speed is cells per second, so
    /// the progress delta is normalised by the path length).
    /// When SlowUntil > 0 the enemy moves at half speed (Frost effect).
    let advance (path: Path) (dt: DeltaTime) (enemy: Enemy) : MoveResult =
        let (PathProgress p) = enemy.Progress
        let slowFactor = if enemy.SlowUntil > 0.0 then 0.5 else 1.0

        let p' =
            p
            + EnemyType.speed enemy.Type * slowFactor * DeltaTime.seconds dt / Path.length path

        if p' >= 1.0 then ReachedGoal else Moved(PathProgress p')

    /// Decrement the slow timer and spawn cooldown by the elapsed seconds.
    let tickCooldowns (dtSeconds: float) (enemy: Enemy) : Enemy =
        { enemy with 
            SlowUntil = max 0.0 (enemy.SlowUntil - dtSeconds)
            SpawnCooldown = max 0.0 (enemy.SpawnCooldown - dtSeconds) }

    /// Current position in cell units.
    let positionOn (path: Path) (enemy: Enemy) : float * float = Path.positionAt path enemy.Progress

    /// Movement angle in radians based on current trajectory.
    let angleOn (path: Path) (enemy: Enemy) : float =
        let p = PathProgress.value enemy.Progress
        let p2 = min 1.0 (p + 0.001)
        let x1, y1 = Path.positionAt path enemy.Progress
        let x2, y2 = Path.pointAtDistance path (p2 * Path.length path)
        if x2 = x1 && y2 = y1 then 0.0
        else System.Math.Atan2(y2 - y1, x2 - x1)

// ---------------------------------------------------------------------------
// Active Spells
// ---------------------------------------------------------------------------

type ActiveSpell =
    | Fireball
    | FrostNova

module ActiveSpell =
    let radius = function
        | Fireball -> 1.5
        | FrostNova -> 2.5
    
    let cost = function
        | Fireball -> 30
        | FrostNova -> 25

// ---------------------------------------------------------------------------
// Maps & Geography
// ---------------------------------------------------------------------------

type MapTheme =
    | Plain
    | River
    | Volcanic
    | Winter

module MapTheme =
    let path (theme: MapTheme) (size: GridSize) : Path =
        let n = float (GridSize.value size)
        match theme with
        | Plain -> 
            Path.tryCreate [ -0.9, 2.5; n + 0.9, 2.5 ] |> Option.get
        | River -> 
            Path.tryCreate [ -0.9, 3.5; 5.5, 3.5; 5.5, n + 0.9 ] |> Option.get
        | Volcanic -> 
            Path.tryCreate [ 1.5, -0.9; 1.5, n - 2.5; n + 0.9, n - 2.5 ] |> Option.get
        | Winter -> 
            Path.tryCreate [ n - 1.5, -0.9; n - 1.5, 3.5; -0.9, 3.5 ] |> Option.get

    let blockedCells (theme: MapTheme) (size: GridSize) : Set<Coord> =
        let n = GridSize.value size
        match theme with
        | Plain -> Set.empty
        | River ->
            [ for r in 0 .. n - 1 do
                if r <> 3 && r <> 4 then
                    yield Coord.tryCreate size r 4
                    yield Coord.tryCreate size r 5
              yield Coord.tryCreate size (n - 1) 4
              yield Coord.tryCreate size (n - 1) 5 ]
            |> List.choose id |> Set.ofList
        | Volcanic ->
            [ for c in 0 .. n - 1 do
                if c <> 1 && c <> 2 then
                    yield Coord.tryCreate size 1 c
              yield Coord.tryCreate size 0 1
              yield Coord.tryCreate size 0 2 ]
            |> List.choose id |> Set.ofList
        | Winter ->
            [ Coord.tryCreate size 1 1
              Coord.tryCreate size 1 2
              Coord.tryCreate size 2 1
              Coord.tryCreate size (n-2) (n-3)
              Coord.tryCreate size (n-3) (n-2)
              Coord.tryCreate size (n-2) (n-2) ]
            |> List.choose id |> Set.ofList

// ---------------------------------------------------------------------------
// Campaign & Levels
// ---------------------------------------------------------------------------

type LevelId = int

type LevelDef =
    { Id: LevelId
      Name: string
      Description: string
      Size: GridSize
      Theme: MapTheme
      AllowedTowers: Set<TowerType>
      StartingGold: int
      WaveCount: int }

module Levels =
    let all =
        [ { Id = 1
            Name = "Rookie Bootcamp"
            Description = "Defend the pass with basic infantry."
            Size = GridSize.tryCreate 5 |> Option.get
            Theme = Plain
            AllowedTowers = Set.ofList [ Archer ] // Basic
            StartingGold = 0
            WaveCount = 5 }
          { Id = 2
            Name = "The River Crossing"
            Description = "Hold the bridge against heavier forces."
            Size = GridSize.tryCreate 6 |> Option.get
            Theme = River
            AllowedTowers = Set.ofList [ Archer; Cannon ]
            StartingGold = 150
            WaveCount = 10 }
          { Id = 3
            Name = "Volcanic Keep"
            Description = "Use Frost magic to slow the horde."
            Size = GridSize.tryCreate 7 |> Option.get
            Theme = Volcanic
            AllowedTowers = Set.ofList [ Archer; Cannon; Frost ]
            StartingGold = 200
            WaveCount = 15 }
          { Id = 4
            Name = "Winter Siege"
            Description = "Command all forces to defend the Citadel."
            Size = GridSize.tryCreate 8 |> Option.get
            Theme = Winter
            AllowedTowers = Set.ofList [ Archer; Cannon; Frost ]
            StartingGold = 300
            WaveCount = 20 } ]
    
    let get id = all |> List.tryFind (fun l -> l.Id = id)

type Language = EN | TR | DE | AR | RU | ZH

type CampaignState =
    { UnlockedLevels: Set<LevelId>
      PersistentGold: int
      Talents: Talents
      Language: Language }

module CampaignState =
    let empty =
        { UnlockedLevels = Set.singleton 1
          PersistentGold = 0
          Talents = Talents.empty
          Language = EN }
