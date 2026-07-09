/// Procedural rendering with the PixiJS Graphics API — no external assets.
/// Pure "view = f(model)": every frame the dynamic layers are cleared and
/// redrawn from the current UiModel; nothing in here mutates game state.
/// The enemy lane is drawn from the core's Path geometry, so the picture
/// can never disagree with the simulation.
///
/// Phase 4 additions: particle effects, floating text, enemy shadows, slow
/// tint, muzzle flash, path ambient lights, grid hover glow, screen shake
/// and a red damage flash overlay.
module MergeTowerDefense.View.Render

open MergeTowerDefense.Shared
open MergeTowerDefense.State
open MergeTowerDefense.Ui
open MergeTowerDefense.Interop.Pixi

// ---------------------------------------------------------------------------
// Palette (procedural, per tower type and level)
// ---------------------------------------------------------------------------

let private archerShades = [| 0x2e7d32; 0x43a047; 0x66bb6a; 0x81c784; 0xa5d6a7 |]
let private cannonShades = [| 0xef6c00; 0xfb8c00; 0xffa726; 0xffb74d; 0xffcc80 |]
let private frostShades = [| 0x0277bd; 0x039be5; 0x29b6f6; 0x4fc3f7; 0x81d4fa |]

let private towerShade (towerType: TowerType) (rank: int) =
    let shades =
        match towerType with
        | Archer -> archerShades
        | Cannon -> cannonShades
        | Frost -> frostShades

    shades.[rank - 1]

let private towerBaseColor (towerType: TowerType) =
    match towerType with
    | Archer -> 0x66bb6a
    | Cannon -> 0xffa726
    | Frost -> 0x4fc3f7

let private enemyColor (enemyType: EnemyType) =
    match enemyType with
    | Grunt -> 0xb0bec5
    | Runner -> 0xffee58
    | Tank -> 0x8d6e63
    | Boss -> 0xab47bc

// ---------------------------------------------------------------------------
// Layers
// ---------------------------------------------------------------------------

/// Draw order, bottom to top: static board, overlay (highlights + ranges),
/// towers, enemies, shot tracers, particles, floating text, drag ghost,
/// flash overlay.
type Layers =
    { Static: Graphics
      Overlay: Graphics
      Towers: Graphics
      Enemies: Graphics
      Shots: Graphics
      Effects: Graphics
      Ghost: Graphics
      Flash: Graphics }

let createLayers (app: Application) : Layers =
    let make () =
        let g = createGraphics ()
        app.stage.addChild g |> ignore
        g

    { Static = make ()
      Overlay = make ()
      Towers = make ()
      Enemies = make ()
      Shots = make ()
      Effects = make ()
      Ghost = make ()
      Flash = make () }

// ---------------------------------------------------------------------------
// Shared shape helpers
// ---------------------------------------------------------------------------

/// Flat vertex list for Graphics.drawPolygon (see the binding for why obj[]).
let private poly (points: float list) : obj [] =
    points |> List.map box |> List.toArray

// ---------------------------------------------------------------------------
// Static board (drawn once; derived from grid size and path geometry)
// ---------------------------------------------------------------------------

let drawStatic (layout: Layout) (size: GridSize) (path: Path) (layers: Layers) : unit =
    let g = layers.Static
    let n = GridSize.value size
    let cell = layout.CellSize

    // Enemy lane: a thick strip along the actual Path polyline.
    let waypointsPx = Path.waypoints path |> List.map (toPx layout)

    (match waypointsPx with
     | [] -> ()
     | (x0, y0) :: rest ->
         g.lineStyle (laneWidthPx layout, 0x202433, 1.0) |> ignore
         g.moveTo (x0, y0) |> ignore

         for x, y in rest do
             g.lineTo (x, y) |> ignore

         // Centre line on top of the strip.
         g.lineStyle (3.0, 0x3a415f, 1.0) |> ignore
         g.moveTo (x0, y0) |> ignore

         for x, y in rest do
             g.lineTo (x, y) |> ignore)

    // Direction chevrons, sampled along the path at fixed walk distances.
    let total = Path.length path
    let chevronEvery = 1.3

    let chevronCount = int (total / chevronEvery)

    for i in 1 .. chevronCount - 1 do
        let d = float i * chevronEvery
        let px, py = toPx layout (Path.pointAtDistance path d)
        let ax, ay = toPx layout (Path.pointAtDistance path (d + 0.3))
        let dx, dy = ax - px, ay - py
        let len = sqrt (dx * dx + dy * dy)

        if len > 0.0 then
            let ux, uy = dx / len, dy / len
            let vx, vy = -uy, ux // perpendicular

            g
                .lineStyle(2.0, 0x4c557a, 1.0)
                .moveTo(px - ux * 4.0 + vx * 5.0, py - uy * 4.0 + vy * 5.0)
                .lineTo(px + ux * 4.0, py + uy * 4.0)
                .lineTo(px - ux * 4.0 - vx * 5.0, py - uy * 4.0 - vy * 5.0)
            |> ignore

    // Spawn portal at the entry, goal marker at the exit.
    (match waypointsPx with
     | [] -> ()
     | (sx, sy) :: _ ->
         g
             .lineStyle(3.0, 0x66bb6a, 0.9)
             .beginFill(0x1a1d29, 1.0)
             .drawCircle(sx, sy, 13.0)
             .endFill ()
         |> ignore)

    (match List.tryLast waypointsPx with
     | None -> ()
     | Some(gx, gy) ->
         g
             .lineStyle(3.0, 0xef5350, 0.9)
             .beginFill(0x1a1d29, 1.0)
             .drawCircle(gx, gy, 13.0)
             .endFill()
             .lineStyle(0.0, 0, 0.0)
             .beginFill(0xef5350, 0.9)
             .drawCircle(gx, gy, 5.0)
             .endFill ()
         |> ignore)

    // Checkerboard grid cells.
    for row in 0 .. n - 1 do
        for col in 0 .. n - 1 do
            let x = layout.GridLeft + float col * cell
            let y = layout.GridTop + float row * cell
            let fill = if (row + col) % 2 = 0 then 0x232738 else 0x1f2333

            g
                .lineStyle(1.0, 0x2f3450, 1.0)
                .beginFill(fill, 1.0)
                .drawRect(x, y, cell, cell)
                .endFill ()
            |> ignore

// ---------------------------------------------------------------------------
// Shape helpers shared by placed towers and the drag ghost
// ---------------------------------------------------------------------------

/// Draws one tower, fully procedurally: shape encodes the type, size/shade
/// encode the level, and white pips repeat the level for colour-blind
/// readability. Used for both placed towers and the drag ghost.
let private drawTowerShape (g: Graphics) (x: float) (y: float) (tower: Tower) (alpha: float) : unit =
    let rank = TowerLevel.rank tower.Level
    let half = 12.0 + 3.0 * float rank
    let color = towerShade tower.Type rank

    g.lineStyle (0.0, 0, 0.0) |> ignore

    (match tower.Type with
     | Archer -> g.beginFill(color, alpha).drawCircle(x, y, half).endFill ()
     | Cannon ->
         g
             .beginFill(color, alpha)
             .drawRoundedRect(x - half, y - half, half * 2.0, half * 2.0, 6.0)
             .endFill ()
     | Frost ->
         g
             .beginFill(color, alpha)
             .drawPolygon(poly [ x; y - half; x + half; y; x; y + half; x - half; y ])
             .endFill ())
    |> ignore

    for i in 0 .. rank - 1 do
        let pipX = x - float (rank - 1) * 4.0 + float i * 8.0

        g
            .beginFill(0xffffff, 0.9 * alpha)
            .drawCircle(pipX, y + half + 6.0, 2.0)
            .endFill ()
        |> ignore

/// Translucent attack-range disc for a tower standing (or previewed) at the
/// given canvas position.
let private drawRange (g: Graphics) (layout: Layout) (x: float) (y: float) (tower: Tower) : unit =
    let stats = Tower.stats tower
    let radius = stats.Range * layout.CellSize
    let color = towerBaseColor tower.Type

    g
        .lineStyle(1.5, color, 0.35)
        .beginFill(color, 0.07)
        .drawCircle(x, y, radius)
        .endFill ()
    |> ignore

let private drawEnemy (g: Graphics) (x: float) (y: float) (enemy: Enemy) : unit =
    let color = enemyColor enemy.Type

    // Shadow under the enemy.
    g.lineStyle(0.0, 0, 0.0)
        .beginFill(0x000000, 0.25)
        .drawEllipse(x, y + 12.0, 10.0, 4.0)
        .endFill ()
    |> ignore

    // Slow tint: when slowed, overlay a blue tint by blending the base color.
    let drawColor =
        if enemy.SlowUntil > 0.0 then
            // Shift toward frost blue.
            let r1 = (color >>> 16) &&& 0xFF
            let g1 = (color >>> 8) &&& 0xFF
            let b1 = color &&& 0xFF
            let r2 = min 255 (r1 / 2 + 0x29)
            let g2 = min 255 (g1 / 2 + 0x5B)
            let b2 = min 255 (b1 / 2 + 0x7B)
            (r2 <<< 16) ||| (g2 <<< 8) ||| b2
        else
            color

    g.lineStyle (0.0, 0, 0.0) |> ignore

    (match enemy.Type with
     | Grunt -> g.beginFill(drawColor, 1.0).drawCircle(x, y, 9.0).endFill ()
     | Runner ->
         g
             .beginFill(drawColor, 1.0)
             .drawPolygon(poly [ x; y - 9.0; x + 8.0; y + 7.0; x - 8.0; y + 7.0 ])
             .endFill ()
     | Tank ->
         g
             .beginFill(drawColor, 1.0)
             .drawRoundedRect(x - 10.0, y - 10.0, 20.0, 20.0, 4.0)
             .endFill ()
     | Boss ->
         g
             .beginFill(drawColor, 1.0)
             .drawCircle(x, y, 15.0)
             .endFill()
             .lineStyle(2.0, 0xe1bee7, 1.0)
             .drawCircle(x, y, 19.0))
    |> ignore

    // Slow indicator: small frost ring when slowed.
    if enemy.SlowUntil > 0.0 then
        g
            .lineStyle(1.5, 0x4FC3F7, 0.7)
            .drawCircle(x, y, 14.0)
        |> ignore

    // Health bar: current versus the type's unscaled base (waves scale
    // health up, so late-wave enemies can show a "over-full" bar clamped
    // to the bar width).
    let fraction =
        min 1.0 (float (Health.value enemy.Health) / float (EnemyType.baseHealth enemy.Type))

    let barWidth = 26.0
    let barY = y - 24.0

    g
        .lineStyle(0.0, 0, 0.0)
        .beginFill(0x000000, 0.55)
        .drawRect(x - barWidth / 2.0, barY, barWidth, 4.0)
        .endFill ()
    |> ignore

    let barColor =
        if fraction > 0.5 then 0x66bb6a
        elif fraction > 0.25 then 0xffa726
        else 0xef5350

    g
        .beginFill(barColor, 1.0)
        .drawRect(x - barWidth / 2.0, barY, barWidth * fraction, 4.0)
        .endFill ()
    |> ignore

// ---------------------------------------------------------------------------
// Per-frame dynamic drawing
// ---------------------------------------------------------------------------

let private previewColor (preview: DropPreview) =
    match preview with
    | MergeHere _ -> 0x66bb6a
    | MoveHere -> 0x42a5f5
    | ReturnToOrigin -> 0x90a4ae
    | Blocked -> 0xef5350

let drawFrame (layout: Layout) (model: UiModel) (layers: Layers) (time: float) : unit =
    let overlay = layers.Overlay
    overlay.clear () |> ignore
    layers.Towers.clear () |> ignore
    layers.Enemies.clear () |> ignore
    layers.Shots.clear () |> ignore
    layers.Effects.clear () |> ignore
    layers.Ghost.clear () |> ignore
    layers.Flash.clear () |> ignore

    let cell = layout.CellSize
    let path = model.Game.Path

    // Drag feedback: origin outline + drop preview highlight on the hovered
    // cell, colour-coded by what previewDrop says would happen.
    (match model.Game.Interaction with
     | Dragging drag ->
         let ox, oy = cellOrigin layout drag.Origin

         overlay
             .lineStyle(2.0, 0xffffff, 0.25)
             .drawRect(ox + 2.0, oy + 2.0, cell - 4.0, cell - 4.0)
         |> ignore

         match model.Hover with
         | Some target ->
             match previewDrop target model.Game with
             | Some preview ->
                 let hx, hy = cellOrigin layout target

                 overlay
                     .lineStyle(0.0, 0, 0.0)
                     .beginFill(previewColor preview, 0.28)
                     .drawRect(hx + 2.0, hy + 2.0, cell - 4.0, cell - 4.0)
                     .endFill ()
                 |> ignore

                 // Range preview of the tower as it would stand after the
                 // drop (merged towers show their upgraded range).
                 let cx, cy = cellCenter layout target

                 match preview with
                 | MoveHere -> drawRange overlay layout cx cy drag.Tower
                 | MergeHere level -> drawRange overlay layout cx cy { drag.Tower with Level = level }
                 | ReturnToOrigin
                 | Blocked -> ()
             | None -> ()
         | None -> ()
     | Idle ->
         // Idle hover over a tower: visualise its attack range + glow.
         match model.Hover with
         | Some coord ->
             // Hover glow on the cell.
             let hx, hy = cellOrigin layout coord
             overlay
                 .lineStyle(0.0, 0, 0.0)
                 .beginFill(0xffffff, 0.06)
                 .drawRect(hx, hy, cell, cell)
                 .endFill ()
             |> ignore

             match Grid.cellAt coord model.Game.Grid with
             | Occupied tower ->
                 let cx, cy = cellCenter layout coord
                 drawRange overlay layout cx cy tower
             | Empty -> ()
         | None -> ())

    // Towers on the board.
    for coord, tower in Grid.towers model.Game.Grid do
        let x, y = cellCenter layout coord
        drawTowerShape layers.Towers x y tower 1.0

        // Muzzle flash: brief bright circle when the tower just fired
        // (cooldown is at its maximum = just reset this frame).
        let stats = Tower.stats tower
        let maxCd = float stats.CooldownMs / 1000.0
        if tower.Cooldown >= maxCd * 0.95 then
            let flashColor = towerBaseColor tower.Type
            layers.Towers
                .lineStyle(0.0, 0, 0.0)
                .beginFill(flashColor, 0.5)
                .drawCircle(x, y, 6.0)
                .endFill ()
            |> ignore

    // Ambient path lights: small dots drifting along the path.
    let total = Path.length path
    let lightCount = 5
    for i in 0 .. lightCount - 1 do
        let phase = (time * 0.15 + float i / float lightCount) % 1.0
        let px, py = toPx layout (Path.pointAtDistance path (phase * total))
        overlay
            .lineStyle(0.0, 0, 0.0)
            .beginFill(0x4c557a, 0.4)
            .drawCircle(px, py, 3.0)
            .endFill ()
        |> ignore

    // Enemies along the path.
    for enemy in model.Game.Enemies do
        let x, y = toPx layout (Enemy.positionOn path enemy)
        drawEnemy layers.Enemies x y enemy

    // Shot tracers, fading with their remaining ttl.
    for shot in model.Shots do
        let fx, fy = cellCenter layout shot.FromCell
        let tx, ty = toPx layout shot.Target
        let alpha = 0.9 * (shot.Ttl / shotTtl)

        layers.Shots
            .lineStyle(2.0, 0xfff59d, alpha)
            .moveTo(fx, fy)
            .lineTo(tx, ty)
            .lineStyle(0.0, 0, 0.0)
            .beginFill(0xfff59d, alpha)
            .drawCircle(tx, ty, 3.5)
            .endFill ()
        |> ignore

    // Particle effects.
    let g = layers.Effects
    for p in model.Particles do
        let alpha = p.Ttl / p.MaxTtl
        g
            .lineStyle(0.0, 0, 0.0)
            .beginFill(p.Color, alpha)
            .drawCircle(p.X, p.Y, max 0.5 p.Size)
            .endFill ()
        |> ignore

    // Floating text labels (bounty, sell refund).
    for ft in model.FloatingTexts do
        let alpha = ft.Ttl / ft.MaxTtl
        // Draw a small coloured dot as a "text" placeholder — real text
        // rendering would require PIXI.Text which is heavier than Graphics.
        // Instead we use the overlay to hint at the bounty via a bright dot.
        g
            .lineStyle(0.0, 0, 0.0)
            .beginFill(ft.Color, alpha)
            .drawCircle(ft.X, ft.Y, 4.0 * alpha)
            .endFill ()
        |> ignore

    // Drag ghost follows the raw pointer position.
    match model.Game.Interaction, model.Pointer with
    | Dragging drag, Some(px, py) -> drawTowerShape layers.Ghost px py drag.Tower 0.6
    | _ -> ()

    // Red damage flash overlay (covers the whole canvas).
    if model.RedFlash > 0.01 then
        layers.Flash
            .lineStyle(0.0, 0, 0.0)
            .beginFill(0xff0000, model.RedFlash * 0.3)
            .drawRect(0.0, 0.0, layout.CanvasWidth, layout.CanvasHeight)
            .endFill ()
        |> ignore
