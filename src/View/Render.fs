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

let mutable private towerAngles = Map.empty<TowerId, float>

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
    | Bandit -> 0xb0bec5
    | Cavalry -> 0xffee58
    | Brute -> 0x8d6e63
    | Warlord -> 0xab47bc
    | Dragon -> 0xd32f2f
    | Necromancer -> 0x1de9b6
    | MegaBoss -> 0x9c27b0

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
      Atmosphere: Graphics
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
      Atmosphere = make ()
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

let private drawProp (g: Graphics) (x: float) (y: float) (cell: float) (theme: MapTheme) =
    let cx, cy = x + cell / 2.0, y + cell / 2.0
    match theme with
    | Plain -> () // No props in Plain theme
    | River ->
        // Pine tree
        g.lineStyle(0.0, 0, 0.0).beginFill(0x5D4037, 1.0).drawRect(cx - 4.0, cy, 8.0, 16.0).endFill() |> ignore // Trunk
        g.beginFill(0x2E7D32, 1.0).drawPolygon(poly [cx - 16.0; cy + 4.0; cx + 16.0; cy + 4.0; cx; cy - 12.0]).endFill() |> ignore // Bottom layer
        g.beginFill(0x388E3C, 1.0).drawPolygon(poly [cx - 12.0; cy - 4.0; cx + 12.0; cy - 4.0; cx; cy - 20.0]).endFill() |> ignore // Middle layer
        g.beginFill(0x43A047, 1.0).drawPolygon(poly [cx - 8.0; cy - 12.0; cx + 8.0; cy - 12.0; cx; cy - 26.0]).endFill() |> ignore // Top layer
    | Volcanic ->
        // Obsidian Spikes
        g.lineStyle(1.0, 0x111111, 0.8).beginFill(0x212121, 1.0).drawPolygon(poly [cx - 12.0; cy + 12.0; cx + 4.0; cy + 16.0; cx - 4.0; cy - 16.0]).endFill() |> ignore
        g.beginFill(0x333333, 1.0).drawPolygon(poly [cx; cy + 8.0; cx + 16.0; cy + 12.0; cx + 8.0; cy - 8.0]).endFill() |> ignore
        g.beginFill(0x424242, 1.0).drawPolygon(poly [cx - 6.0; cy + 14.0; cx + 8.0; cy + 14.0; cx + 2.0; cy - 4.0]).endFill() |> ignore
        // Lava glow base
        g.lineStyle(0.0, 0, 0.0).beginFill(0xFF3D00, 0.6).drawEllipse(cx, cy + 12.0, 14.0, 6.0).endFill() |> ignore
    | Winter ->
        // Snowman and Ice Crystals
        g.lineStyle(0.0, 0, 0.0).beginFill(0x81D4FA, 0.8).drawPolygon(poly [cx - 10.0; cy + 10.0; cx; cy + 14.0; cx - 4.0; cy - 10.0]).endFill() |> ignore
        g.beginFill(0x4FC3F7, 0.9).drawPolygon(poly [cx - 2.0; cy + 12.0; cx + 12.0; cy + 8.0; cx + 6.0; cy - 4.0]).endFill() |> ignore
        // Snowman
        g.beginFill(0xFFFFFF, 1.0).drawCircle(cx + 8.0, cy + 4.0, 6.0).endFill() |> ignore
        g.drawCircle(cx + 8.0, cy - 4.0, 4.0).endFill() |> ignore
        g.beginFill(0x000000, 1.0).drawCircle(cx + 6.0, cy - 5.0, 1.0).drawCircle(cx + 10.0, cy - 5.0, 1.0).endFill() |> ignore

let drawStatic (theme: MapTheme) (layout: Layout) (size: GridSize) (path: Path) (layers: Layers) : unit =

    let g = layers.Static
    let n = GridSize.value size
    let cell = layout.CellSize
    let waypointsPx = Path.waypoints path |> List.map (toPx layout)
    let blocked = MapTheme.blockedCells theme size

    // Helper to draw a textured cell
    let drawTexturedCell (x, y) (color1, color2, color3) isBlocked =
        g.lineStyle(0.0, 0, 0.0).beginFill(color1, 1.0).drawRect(x, y, cell, cell).endFill() |> ignore
        if isBlocked then
            // For blocked cells, draw organic shapes (lava bubbles, water ripples, ice shards)
            g.beginFill(color2, 0.8).drawCircle(x + cell * 0.3, y + cell * 0.3, cell * 0.2).endFill() |> ignore
            g.beginFill(color3, 0.9).drawCircle(x + cell * 0.7, y + cell * 0.6, cell * 0.15).endFill() |> ignore
            g.beginFill(color2, 0.6).drawCircle(x + cell * 0.2, y + cell * 0.8, cell * 0.1).endFill() |> ignore
        else
            // For buildable cells, draw a small platform/pedestal
            let inset = 4.0
            g.lineStyle(2.0, color3, 0.5).beginFill(color2, 1.0).drawRoundedRect(x + inset, y + inset, cell - inset * 2.0, cell - inset * 2.0, 4.0).endFill() |> ignore
            // Pedestal inner detail
            g.lineStyle(1.0, color3, 0.3).drawRoundedRect(x + inset + 2.0, y + inset + 2.0, cell - inset * 2.0 - 4.0, cell - inset * 2.0 - 4.0, 2.0) |> ignore

    match theme with
    | Plain ->
        // Background: Plain Grass
        g.lineStyle(0.0, 0, 0.0).beginFill(0x4CAF50, 1.0).drawRect(0.0, 0.0, layout.CanvasWidth, layout.CanvasHeight).endFill () |> ignore
        
        for row in 0 .. n - 1 do
            for col in 0 .. n - 1 do
                let x = layout.GridLeft + float col * cell
                let y = layout.GridTop + float row * cell
                let c1 = if (row + col) % 2 = 0 then 0x388E3C else 0x43A047
                drawTexturedCell (x, y) (c1, 0x4CAF50, 0x81C784) false

        // Dirt Path
        (match waypointsPx with
         | [] -> ()
         | (x0, y0) :: rest ->
             g.lineStyle (laneWidthPx layout + 8.0, 0x5D4037, 1.0) |> ignore
             g.moveTo(x0, y0) |> ignore
             for x, y in rest do g.lineTo (x, y) |> ignore
             g.lineStyle (laneWidthPx layout, 0x795548, 1.0) |> ignore
             g.moveTo(x0, y0) |> ignore
             for x, y in rest do g.lineTo (x, y) |> ignore)

    | River ->
        // Background: Soft Grass
        g.lineStyle(0.0, 0, 0.0).beginFill(0x2E7D32, 1.0).drawRect(0.0, 0.0, layout.CanvasWidth, layout.CanvasHeight).endFill () |> ignore
        
        for row in 0 .. n - 1 do
            for col in 0 .. n - 1 do
                let x = layout.GridLeft + float col * cell
                let y = layout.GridTop + float row * cell
                let coord = Coord.tryCreate size row col |> Option.get
                let isBlocked = Set.contains coord blocked
                
                if isBlocked then
                    // Water + Props
                    drawTexturedCell (x, y) (0x1976D2, 0x2196F3, 0x64B5F6) true
                    drawProp g x y cell theme
                else
                    // Grass platform
                    let c1 = if (row + col) % 2 = 0 then 0x388E3C else 0x43A047
                    drawTexturedCell (x, y) (c1, 0x4CAF50, 0x81C784) false

        // Dirt Path
        (match waypointsPx with
         | [] -> ()
         | (x0, y0) :: rest ->
             g.lineStyle (laneWidthPx layout + 8.0, 0x5D4037, 1.0) |> ignore
             g.moveTo(x0, y0) |> ignore
             for x, y in rest do g.lineTo (x, y) |> ignore
             g.lineStyle (laneWidthPx layout, 0x795548, 1.0) |> ignore
             g.moveTo(x0, y0) |> ignore
             for x, y in rest do g.lineTo (x, y) |> ignore)

    | Volcanic ->
        // Background: Dark Ash
        g.lineStyle(0.0, 0, 0.0).beginFill(0x1a1a1a, 1.0).drawRect(0.0, 0.0, layout.CanvasWidth, layout.CanvasHeight).endFill () |> ignore
        
        for row in 0 .. n - 1 do
            for col in 0 .. n - 1 do
                let x = layout.GridLeft + float col * cell
                let y = layout.GridTop + float row * cell
                let coord = Coord.tryCreate size row col |> Option.get
                let isBlocked = Set.contains coord blocked
                
                if isBlocked then
                    // Lava Pool + Props
                    drawTexturedCell (x, y) (0xD84315, 0xFF5722, 0xFFEB3B) true
                    drawProp g x y cell theme
                else
                    // Ash platform
                    let c1 = if (row + col) % 2 = 0 then 0x212121 else 0x333333
                    drawTexturedCell (x, y) (c1, 0x424242, 0xBF360C) false

        // Obsidian Path
        (match waypointsPx with
         | [] -> ()
         | (x0, y0) :: rest ->
             g.lineStyle (laneWidthPx layout + 6.0, 0x000000, 1.0) |> ignore
             g.moveTo(x0, y0) |> ignore
             for x, y in rest do g.lineTo (x, y) |> ignore
             g.lineStyle (laneWidthPx layout, 0x263238, 1.0) |> ignore
             g.moveTo(x0, y0) |> ignore
             for x, y in rest do g.lineTo (x, y) |> ignore)

    | Winter ->
        // Background: Frosty Ground
        g.lineStyle(0.0, 0, 0.0).beginFill(0xB0BEC5, 1.0).drawRect(0.0, 0.0, layout.CanvasWidth, layout.CanvasHeight).endFill () |> ignore
        
        for row in 0 .. n - 1 do
            for col in 0 .. n - 1 do
                let x = layout.GridLeft + float col * cell
                let y = layout.GridTop + float row * cell
                let coord = Coord.tryCreate size row col |> Option.get
                let isBlocked = Set.contains coord blocked
                
                if isBlocked then
                    // Ice crystals + Props
                    drawTexturedCell (x, y) (0x81D4FA, 0x4FC3F7, 0xFFFFFF) true
                    drawProp g x y cell theme
                else
                    // Snow platform
                    let c1 = if (row + col) % 2 = 0 then 0xCFD8DC else 0xE0E0E0
                    drawTexturedCell (x, y) (c1, 0xFAFAFA, 0x90CAF9) false

        // Cobblestone Path
        (match waypointsPx with
         | [] -> ()
         | (x0, y0) :: rest ->
             g.lineStyle (laneWidthPx layout + 8.0, 0x78909C, 1.0) |> ignore
             g.moveTo(x0, y0) |> ignore
             for x, y in rest do g.lineTo (x, y) |> ignore
             g.lineStyle (laneWidthPx layout, 0xCFD8DC, 1.0) |> ignore
             g.moveTo(x0, y0) |> ignore
             for x, y in rest do g.lineTo (x, y) |> ignore)

    // Base entry/exit points (same for all themes)
    (match waypointsPx with
     | [] -> ()
     | (sx, sy) :: _ ->
         g.lineStyle(2.0, 0x111111, 0.8).beginFill(0x8D6E63, 1.0).drawPolygon(poly [sx; sy-16.0; sx+14.0; sy+14.0; sx-14.0; sy+14.0]).endFill () |> ignore
         g.lineStyle(0.0, 0, 0.0).beginFill(0x212121, 1.0).drawPolygon(poly [sx; sy+4.0; sx+6.0; sy+14.0; sx-6.0; sy+14.0]).endFill () |> ignore
         // Glowing Portal
         g.lineStyle(0.0, 0, 0.0).beginFill(0x9C27B0, 0.4).drawCircle(sx, sy, 20.0).endFill() |> ignore
         g.beginFill(0xE040FB, 0.6).drawCircle(sx, sy, 14.0).endFill() |> ignore
         g.beginFill(0x111111, 1.0).drawCircle(sx, sy, 8.0).endFill() |> ignore)
    (match List.tryLast waypointsPx with
     | None -> ()
     | Some(gx, gy) ->
         // Detailed Castle/Core
         g.lineStyle(2.0, 0x111111, 0.8).beginFill(0x9E9E9E, 1.0).drawRect(gx - 24.0, gy - 20.0, 48.0, 40.0).endFill () |> ignore
         // Crenellations
         for cx in [gx - 24.0; gx - 8.0; gx + 8.0] do g.lineStyle(2.0, 0x111111, 0.8).beginFill(0x9E9E9E, 1.0).drawRect(cx, gy - 30.0, 16.0, 10.0).endFill () |> ignore
         // Giant Glowing Core inside the castle
         g.lineStyle(0.0, 0, 0.0).beginFill(0x00E5FF, 0.4).drawCircle(gx, gy + 4.0, 16.0).endFill () |> ignore
         g.beginFill(0x00E5FF, 0.8).drawCircle(gx, gy + 4.0, 10.0).endFill () |> ignore
         g.beginFill(0xFFFFFF, 1.0).drawCircle(gx, gy + 4.0, 4.0).endFill () |> ignore)

// ---------------------------------------------------------------------------
// Shape helpers shared by placed towers and the drag ghost
// ---------------------------------------------------------------------------

/// Draws one tower, fully procedurally: shape encodes the type, size/shade
/// encode the level, and white pips repeat the level for colour-blind
/// readability. Used for both placed towers and the drag ghost.
let private drawTowerShape (g: Graphics) (x: float) (y: float) (tower: Tower) (alpha: float) (angle: float) : unit =
    let rank = TowerLevel.rank tower.Level
    let half = 12.0 + 3.0 * float rank
    let color = towerShade tower.Type rank

    g.lineStyle (0.0, 0, 0.0) |> ignore

    (match tower.Type with
     | Archer ->
         // Wooden Watchtower
         g.lineStyle(2.0, 0x3E2723, alpha).beginFill(0x5D4037, alpha).drawRect(x - half + 2.0, y - half + 4.0, half * 2.0 - 4.0, half * 2.0).endFill() |> ignore
         // Green Roof/Canopy
         g.lineStyle(2.0, 0x1B5E20, alpha).beginFill(color, alpha).drawPolygon(poly [x; y - half - 4.0; x + half; y; x - half; y]).endFill() |> ignore
         
         // Archer Bow (Rotated)
         let dx = cos angle
         let dy = sin angle
         let nx = -dy * 6.0
         let ny = dx * 6.0
         let bx, by = x + dx * 2.0, y + dy * 2.0
         g.lineStyle(2.0, 0x000000, alpha).moveTo(x, y).lineTo(bx + dx * 6.0, by + dy * 6.0).endFill() |> ignore // Arrow shaft
         g.lineStyle(2.0, 0x5D4037, alpha).moveTo(bx + nx, by + ny).lineTo(bx + dx * 2.0, by + dy * 2.0).lineTo(bx - nx, by - ny).endFill()
     | Cannon ->
         // Stone Keep
         g.lineStyle(2.0, 0x424242, alpha).beginFill(color, alpha).drawRoundedRect(x - half, y - half, half * 2.0, half * 2.0, 4.0).endFill() |> ignore
         // Crenellations
         g.lineStyle(0.0, 0, 0.0).beginFill(color, alpha).drawRect(x - half, y - half - 4.0, 6.0, 4.0).endFill() |> ignore
         g.lineStyle(0.0, 0, 0.0).beginFill(color, alpha).drawRect(x + half - 6.0, y - half - 4.0, 6.0, 4.0).endFill() |> ignore
         
         // Cannon Barrel (Rotated)
         let barrelLength = half + 6.0
         let hw = 4.0
         let dx = cos angle
         let dy = sin angle
         let nx = -dy * hw
         let ny = dx * hw
         let p1x, p1y = x + nx, y + ny
         let p2x, p2y = x - nx, y - ny
         let p3x, p3y = x - nx + dx * barrelLength, y - ny + dy * barrelLength
         let p4x, p4y = x + nx + dx * barrelLength, y + ny + dy * barrelLength
         
         g.lineStyle(1.0, 0x000000, alpha).beginFill(0x212121, alpha).drawPolygon(poly [p1x; p1y; p2x; p2y; p3x; p3y; p4x; p4y]).endFill() |> ignore
         g.lineStyle(0.0, 0, 0.0).beginFill(0x000000, alpha).drawCircle(x + dx * (barrelLength - 2.0), y + dy * (barrelLength - 2.0), 4.0).endFill()
     | Frost ->
         // Crystal Spire
         g.lineStyle(1.0, 0x01579B, alpha).beginFill(0x0277BD, alpha).drawPolygon(poly [x; y - half; x + half; y + half; x - half; y + half]).endFill() |> ignore
         g.lineStyle(2.0, 0x81D4FA, alpha).beginFill(color, alpha).drawPolygon(poly [x; y - half - 8.0; x + 6.0; y + half - 4.0; x - 6.0; y + half - 4.0]).endFill() |> ignore
         g.lineStyle(0.0, 0, 0.0).beginFill(0xE1F5FE, alpha).drawPolygon(poly [x; y - half; x + 2.0; y + half - 6.0; x - 2.0; y + half - 6.0]).endFill())
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
let private drawRange (g: Graphics) (layout: Layout) (x: float) (y: float) (tower: Tower) (talents: Talents) : unit =
    let stats = Tower.stats tower talents
    let radius = stats.Range * layout.CellSize
    let color = towerBaseColor tower.Type

    g
        .lineStyle(1.5, color, 0.35)
        .beginFill(color, 0.07)
        .drawCircle(x, y, radius)
        .endFill ()
    |> ignore

let private drawEnemy (g: Graphics) (x: float) (y: float) (angle: float) (time: float) (enemy: Enemy) : unit =
    let color = enemyColor enemy.Type
    
    // Directional facing
    let lookX = cos angle * 3.0
    let lookY = sin angle * 3.0

    // Dynamic shadow based on enemy type
    let shadowRadius, shadowWidth = 
        match enemy.Type with
        | MegaBoss -> 40.0, 15.0
        | Dragon -> 28.0, 10.0
        | Necromancer -> 20.0, 7.0
        | Warlord -> 18.0, 6.0
        | Brute -> 14.0, 5.0
        | Bandit -> 10.0, 4.0
        | Cavalry -> 9.0, 4.0

    g.lineStyle(0.0, 0, 0.0)
        .beginFill(0x000000, 0.3)
        .drawEllipse(x, y + 14.0, shadowRadius, shadowWidth)
        .endFill ()
    |> ignore

    // Health Bar
    let maxHp = Health.value enemy.MaxHealth |> float
    let curHp = Health.value enemy.Health |> float
    let hpPercent = max 0.0 (curHp / maxHp)
    let barWidth = match enemy.Type with MegaBoss -> 40.0 | Dragon -> 28.0 | _ -> 20.0
    let barHeight = match enemy.Type with MegaBoss -> 6.0 | _ -> 4.0
    let barY = y - (match enemy.Type with MegaBoss -> 45.0 | Dragon -> 35.0 | Warlord -> 30.0 | _ -> 22.0)
    
    g.lineStyle(0.0, 0, 0.0)
     .beginFill(0x333333, 0.8)
     .drawRect(x - barWidth / 2.0, barY, barWidth, barHeight)
     .endFill() |> ignore
     
    let hpColor = if hpPercent > 0.5 then 0x4CAF50 elif hpPercent > 0.2 then 0xFFA000 else 0xF44336
    g.beginFill(hpColor, 1.0)
     .drawRect(x - barWidth / 2.0, barY, barWidth * hpPercent, barHeight)
     .endFill() |> ignore

    // Calculate vertical bobbing
    let bounceSpeed = match enemy.Type with MegaBoss -> 2.0 | Dragon -> 4.0 | _ -> 12.0
    let bounceHeight = match enemy.Type with MegaBoss -> 12.0 | Dragon -> 8.0 | _ -> 3.0
    let bounceOffset = float (hash enemy) * 1.5
    let animY = y + sin (time * bounceSpeed + bounceOffset) * bounceHeight

    // Slow tint: when slowed, overlay a blue tint by blending the base color.
    let drawColor =
        if enemy.SlowUntil > 0.0 then
            // Shift toward frost blue.
            let cr = (color >>> 16) &&& 0xFF
            let cg = (color >>> 8) &&& 0xFF
            let cb = color &&& 0xFF
            let nr = min 255 (cr / 2 + 0x29)
            let ng = min 255 (cg / 2 + 0x5B)
            let nb = min 255 (cb / 2 + 0x7B)
            (nr <<< 16) ||| (ng <<< 8) ||| nb
        else
            color

    let drawColorDarker =
        let r = (drawColor >>> 16) &&& 0xFF
        let gg = (drawColor >>> 8) &&& 0xFF
        let b = drawColor &&& 0xFF
        ((r / 2) <<< 16) ||| ((gg / 2) <<< 8) ||| (b / 2)

    // Base shape stroke for more definition
    g.lineStyle (2.0, drawColorDarker, 1.0) |> ignore

    (match enemy.Type with
     | Bandit -> 
         // Hooded figure with glowing eyes
         g.beginFill(drawColor, 1.0).drawPolygon(poly [x-8.0; animY+10.0; x+8.0; animY+10.0; x+6.0; animY-6.0; x; animY-10.0; x-6.0; animY-6.0]).endFill() |> ignore
         g.lineStyle(0.0, 0, 0.0).beginFill(0x222222, 0.8).drawCircle(x + lookX, animY-2.0 + lookY, 6.0).endFill() |> ignore // Face
         g.beginFill(0xff3333, 1.0).drawCircle(x-2.0 + lookX, animY-2.0 + lookY, 1.5).drawCircle(x+2.0 + lookX, animY-2.0 + lookY, 1.5).endFill()
     | Cavalry ->
         // Shield and Lance
         g.beginFill(drawColor, 1.0).drawPolygon(poly [x-8.0; animY-10.0; x+8.0; animY-10.0; x+8.0; animY+2.0; x; animY+12.0; x-8.0; animY+2.0]).endFill() |> ignore // Shield
         g.lineStyle(3.0, 0xdddddd, 1.0).moveTo(x-12.0 + lookX*2.0, animY+8.0 + lookY*2.0).lineTo(x+14.0 + lookX*2.0, animY-14.0 + lookY*2.0).endFill() |> ignore // Lance
         g.lineStyle(0.0, 0, 0.0).beginFill(0x444444, 0.8).drawCircle(x + lookX, animY + lookY, 4.0).endFill()
     | Brute ->
         // Heavy Armor Octagon
         g.beginFill(drawColor, 1.0).drawPolygon(poly [x-5.0; animY-12.0; x+5.0; animY-12.0; x+12.0; animY-5.0; x+12.0; animY+5.0; x+5.0; animY+12.0; x-5.0; animY+12.0; x-12.0; animY+5.0; x-12.0; animY-5.0]).endFill() |> ignore
         g.lineStyle(3.0, 0x222222, 0.6).moveTo(x-8.0, animY-8.0).lineTo(x+8.0, animY+8.0).moveTo(x+8.0, animY-8.0).lineTo(x-8.0, animY+8.0).endFill() |> ignore
         g.lineStyle(0.0, 0, 0.0).beginFill(0xffaa00, 1.0).drawCircle(x + lookX, animY + lookY, 4.0).endFill()
     | Warlord ->
         // Crowned Menace
         g.beginFill(drawColor, 1.0).drawCircle(x, animY, 14.0).endFill() |> ignore
         g.beginFill(0xffd700, 1.0).drawPolygon(poly [x-10.0; animY-8.0; x-14.0; animY-18.0; x-6.0; animY-12.0; x; animY-22.0; x+6.0; animY-12.0; x+14.0; animY-18.0; x+10.0; animY-8.0]).endFill() |> ignore
         g.lineStyle(2.0, 0xff0000, 0.9).drawCircle(x + lookX, animY + lookY, 6.0).endFill() |> ignore
         g.lineStyle(0.0, 0, 0.0).beginFill(0x000000, 0.8).drawCircle(x + lookX, animY + lookY, 3.0).endFill()
     | Dragon ->
         // Dragon with Wings
         g.beginFill(drawColorDarker, 1.0).drawPolygon(poly [x; animY-10.0; x+30.0; animY-26.0; x+14.0; animY; x+30.0; animY+10.0; x; animY+10.0]).endFill() |> ignore // Right Wing
         g.beginFill(drawColorDarker, 1.0).drawPolygon(poly [x; animY-10.0; x-30.0; animY-26.0; x-14.0; animY; x-30.0; animY+10.0; x; animY+10.0]).endFill() |> ignore // Left Wing
         g.beginFill(drawColor, 1.0).drawPolygon(poly [x-8.0; animY-20.0; x+8.0; animY-20.0; x+10.0; animY; x; animY+20.0; x-10.0; animY]).endFill() |> ignore // Body
         g.beginFill(0xffdd00, 1.0).drawPolygon(poly [x-4.0+lookX; animY-14.0+lookY; x-1.0+lookX; animY-12.0+lookY; x-4.0+lookX; animY-10.0+lookY]).drawPolygon(poly [x+4.0+lookX; animY-14.0+lookY; x+1.0+lookX; animY-12.0+lookY; x+4.0+lookX; animY-10.0+lookY]).endFill() // Eyes
     | Necromancer ->
         // Cloak and Glowing Staff Orb
         g.beginFill(0x222222, 1.0).drawPolygon(poly [x-12.0; animY+20.0; x+12.0; animY+20.0; x+8.0; animY-16.0; x; animY-24.0; x-8.0; animY-16.0]).endFill() |> ignore // Cloak
         g.lineStyle(2.0, drawColor, 0.8).drawPolygon(poly [x-10.0; animY+18.0; x+10.0; animY+18.0; x+6.0; animY-14.0; x; animY-20.0; x-6.0; animY-14.0]).endFill() |> ignore // Trim
         g.lineStyle(0.0, 0, 0.0).beginFill(0x1de9b6, 0.9).drawCircle(x+14.0+lookX, animY-14.0+lookY, 7.0).endFill() |> ignore // Orb
         g.beginFill(0xffffff, 1.0).drawCircle(x+14.0+lookX, animY-14.0+lookY, 3.0).endFill() |> ignore // Orb inner
         g.beginFill(0x1de9b6, 1.0).drawCircle(x-4.0+lookX, animY-8.0+lookY, 2.0).drawCircle(x+4.0+lookX, animY-8.0+lookY, 2.0).endFill()
     | MegaBoss ->
         // Colossal demonic figure with glowing aura
         g.lineStyle(4.0, 0xff00ff, 0.5).drawCircle(x, animY, 24.0).endFill() |> ignore // Aura
         g.beginFill(drawColorDarker, 1.0).drawPolygon(poly [x-20.0; animY-30.0; x+20.0; animY-30.0; x+16.0; animY+20.0; x-16.0; animY+20.0]).endFill() |> ignore // Huge Body
         g.beginFill(0x111111, 1.0).drawPolygon(poly [x-10.0; animY-10.0; x+10.0; animY-10.0; x; animY+10.0]).endFill() |> ignore // Armor chestplate
         g.beginFill(0xff0000, 1.0).drawCircle(x-8.0+lookX, animY-20.0+lookY, 3.0).drawCircle(x+8.0+lookX, animY-20.0+lookY, 3.0).endFill())
    |> ignore

    // Slow indicator: small frost ring when slowed.
    if enemy.SlowUntil > 0.0 then
        g
            .lineStyle(1.5, 0x4FC3F7, 0.7)
            .drawCircle(x, animY, 14.0)
        |> ignore

    // Health bar: current versus the type's unscaled base (waves scale
    // health up, so late-wave enemies can show a "over-full" bar clamped
    // to the bar width).
    let fraction =
        min 1.0 (float (Health.value enemy.Health) / float (EnemyType.baseHealth enemy.Type))

    let barWidth = 28.0
    let barY = animY - 26.0

    // Rounded background with a slight dark border
    g
        .lineStyle(1.0, 0x111111, 0.8)
        .beginFill(0x222222, 0.7)
        .drawRoundedRect(x - barWidth / 2.0, barY, barWidth, 5.0, 2.5)
        .endFill ()
    |> ignore

    let barColor =
        if fraction > 0.5 then 0x66bb6a
        elif fraction > 0.25 then 0xffa726
        else 0xef5350

    if fraction > 0.0 then
        g
            .lineStyle(0.0, 0, 0.0)
            .beginFill(barColor, 1.0)
            .drawRoundedRect(x - barWidth / 2.0 + 1.0, barY + 1.0, (barWidth - 2.0) * fraction, 3.0, 1.5)
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
    layers.Atmosphere.clear () |> ignore
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
                 | MoveHere -> drawRange overlay layout cx cy drag.Tower model.Game.Talents
                 | MergeHere level -> drawRange overlay layout cx cy { drag.Tower with Level = level } model.Game.Talents
                 | ReturnToOrigin
                 | Blocked -> ()
             | None -> ()
         | None -> ()
     | Idle
     | CastingSpell _ ->
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
                 drawRange overlay layout cx cy tower model.Game.Talents
             | Empty 
             | BlockedCell -> ()
         | None -> ())

    // Towers on the board.
    for coord, tower in Grid.towers model.Game.Grid do
        let mutable x, y = cellCenter layout coord

        // Apply recoil animation if a recent shot exists.
        match model.Shots |> List.tryFind (fun s -> s.FromCell = coord) with
        | Some s ->
            let tx, ty = s.Target
            let cx, cy = float (Coord.col coord) + 0.5, float (Coord.row coord) + 0.5
            let dx = tx - cx
            let dy = ty - cy
            let len = sqrt(dx * dx + dy * dy)
            if len > 0.0 then
                let progress = min 1.0 (max 0.0 (1.0 - (s.Ttl / 0.12)))
                let recoil = sin(progress * System.Math.PI) * -8.0
                x <- x + (dx / len) * recoil
                y <- y + (dy / len) * recoil
        | None -> ()

        let stats = Tower.stats tower model.Game.Talents
        let mutable currentAngle = 
            match Map.tryFind tower.Id towerAngles with
            | Some a -> a
            | None -> System.Math.PI / 2.0
            
        let inRange (enemy: Enemy) =
            let ex, ey = toPx layout (Enemy.positionOn model.Game.Path enemy)
            let dx = ex - x
            let dy = ey - y
            let pxDist = stats.Range * layout.CellSize
            if dx * dx + dy * dy <= pxDist * pxDist then
                Some (enemy, dx, dy)
            else None
            
        let targetAngle =
            match model.Game.Enemies |> List.choose inRange with
            | [] -> currentAngle
            | candidates ->
                let (_, dx, dy) = candidates |> List.maxBy (fun (e, _, _) -> PathProgress.value e.Progress)
                atan2 dy dx
                
        // Smooth rotation
        let diff = targetAngle - currentAngle
        let pi = System.Math.PI
        let rec normalize d =
            if d > pi then normalize (d - 2.0 * pi)
            elif d < -pi then normalize (d + 2.0 * pi)
            else d
        let diffNorm = normalize diff
        currentAngle <- currentAngle + diffNorm * 0.15
        
        towerAngles <- Map.add tower.Id currentAngle towerAngles

        drawTowerShape layers.Towers x y tower 1.0 currentAngle

        // Muzzle flash: brief bright circle when the tower just fired
        // (cooldown is at its maximum = just reset this frame).
        let stats = Tower.stats tower model.Game.Talents
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
        let angle = Enemy.angleOn path enemy
        drawEnemy layers.Enemies x y angle model.Game.GlobalTime enemy

    // Shot tracers, fading with their remaining ttl.
    for shot in model.Shots do
        let fx, fy = cellCenter layout shot.FromCell
        let tx, ty = toPx layout shot.Target
        let alpha = 0.9 * (shot.Ttl / shotTtl)

        let dx = tx - fx
        let dy = ty - fy
        let dist = sqrt(dx*dx + dy*dy)
        let nx = if dist > 0.0 then dx / dist else 0.0
        let ny = if dist > 0.0 then dy / dist else 0.0

        (match shot.Type with
         | Archer ->
             layers.Shots
                 .lineStyle(2.0, 0x5D4037, alpha)
                 .moveTo(fx, fy)
                 .lineTo(tx, ty)
                 .lineStyle(2.0, 0xE0E0E0, alpha)
                 .moveTo(tx, ty)
                 .lineTo(tx - nx * 6.0 - ny * 3.0, ty - ny * 6.0 + nx * 3.0)
                 .moveTo(tx, ty)
                 .lineTo(tx - nx * 6.0 + ny * 3.0, ty - ny * 6.0 - nx * 3.0)
         | Cannon ->
             layers.Shots
                 .lineStyle(2.0, 0x424242, alpha * 0.3)
                 .moveTo(fx, fy)
                 .lineTo(tx, ty)
                 .lineStyle(0.0, 0, 0.0)
                 .beginFill(0x212121, alpha)
                 .drawCircle(tx, ty, 5.0)
                 .endFill()
         | Frost ->
             layers.Shots
                 .lineStyle(2.0, 0x81D4FA, alpha * 0.4)
                 .moveTo(fx, fy)
                 .lineTo(tx, ty)
                 .lineStyle(0.0, 0, 0.0)
                 .beginFill(0xE1F5FE, alpha)
                 .drawPolygon(poly [tx; ty; tx - nx * 8.0 + ny * 3.0; ty - ny * 8.0 - nx * 3.0; tx - nx * 12.0; ty - ny * 12.0; tx - nx * 8.0 - ny * 3.0; ty - ny * 8.0 + nx * 3.0])
                 .endFill())
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

    // Floating text labels (DOM sync)
    let floatingTextsHtml =
        model.FloatingTexts
        |> List.map (fun ft ->
            let colorHex = sprintf "#%06x" ft.Color
            let alpha = ft.Ttl / ft.MaxTtl
            sprintf "<div style='position:absolute; left:%fpx; top:%fpx; color:%s; opacity:%f; font-weight:bold; font-size:16px; text-shadow:0 2px 4px rgba(0,0,0,0.9); pointer-events:none; transform: translate(-50%%, -50%%); transition: opacity 0.1s;'>%s</div>" ft.X ft.Y colorHex alpha ft.Text)
        |> String.concat ""
    MergeTowerDefense.Interop.Dom.setInnerHTML (MergeTowerDefense.Interop.Dom.getElementById "floating-texts") floatingTextsHtml

    // Weather Particles.
    for p in model.WeatherParticles do
        let alpha = p.Ttl / p.MaxTtl
        g
            .lineStyle(0.0, 0, 0.0)
            .beginFill(p.Color, alpha)
            .drawCircle(p.X, p.Y, max 0.5 p.Size)
            .endFill ()
        |> ignore

    // Shockwaves
    for sw in model.Shockwaves do
        let alpha = sw.Ttl / sw.MaxTtl
        g
            .lineStyle(sw.LineWidth, sw.Color, alpha)
            .beginFill(0, 0.0)
            .drawCircle(sw.X, sw.Y, sw.Radius)
            .endFill ()
        |> ignore

    // Atmosphere: Day/Night Cycle and Lighting
    let atmo = layers.Atmosphere
    ()


    // Global Boss Health Bar
    let bossOpt =
        model.Game.Enemies
        |> List.tryFind (fun e -> e.Type = Dragon || e.Type = Necromancer || e.Type = MegaBoss)

    match bossOpt with
    | Some boss ->
        let maxHp = float (EnemyType.baseHealth boss.Type) * Waves.healthMultiplier model.Game.Wave.Number
        let currentHp = float (Health.value boss.Health)
        let hpFraction = max 0.0 (currentHp / maxHp)
        
        let barWidth = layout.CanvasWidth * 0.5
        let barHeight = 24.0
        let bx = layout.CanvasWidth / 2.0 - barWidth / 2.0
        let by = 20.0
        
        let color = 
            match boss.Type with
            | MegaBoss -> 0x9c27b0
            | Dragon -> 0xd32f2f
            | _ -> 0x1de9b6
        
        overlay
            .lineStyle(2.0, 0x000000, 0.8)
            .beginFill(0x222222, 0.8)
            .drawRect(bx, by, barWidth, barHeight)
            .endFill()
        |> ignore
        
        overlay
            .lineStyle(0.0, 0, 0.0)
            .beginFill(color, 1.0)
            .drawRect(bx + 2.0, by + 2.0, (barWidth - 4.0) * hpFraction, barHeight - 4.0)
            .endFill()
        |> ignore
    | None -> ()

    // Drag ghost follows the raw pointer position.
    match model.Game.Interaction, model.Pointer with
    | Dragging drag, Some(px, py) -> drawTowerShape layers.Ghost px py drag.Tower 0.6 (System.Math.PI / 2.0)
    | _ -> ()

    match model.Game.Interaction, model.Hover with
    | CastingSpell spell, Some coord ->
        let px, py = cellCenter layout coord
        let radiusPx = float (ActiveSpell.radius spell) * layout.CellSize
        let color = 
            match spell with
            | Fireball -> 0xFF4500
            | FrostNova -> 0x4FC3F7
        layers.Ghost
            .lineStyle(3.0, color, 0.8)
            .beginFill(color, 0.15)
            .drawCircle(px, py, float radiusPx)
            .endFill()
        |> ignore
    | _ -> ()

    // Red damage flash overlay (covers the whole canvas).
    if model.RedFlash > 0.01 then
        layers.Flash
            .lineStyle(0.0, 0, 0.0)
            .beginFill(0xff0000, model.RedFlash * 0.3)
            .drawRect(0.0, 0.0, layout.CanvasWidth, layout.CanvasHeight)
            .endFill ()
        |> ignore
