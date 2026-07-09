/// React HUD layer: gold / wave / lives / enemy readouts, per-type buy
/// buttons, sell button, wave progress bar, the transient notice line and
/// the game-over panel. Lives in its own DOM root (#hud-root), completely
/// isolated from the Pixi canvas — it only receives UiModel snapshots and
/// emits UiMsg values through dispatch.
module MergeTowerDefense.View.Hud

open MergeTowerDefense.Shared
open MergeTowerDefense.State
open MergeTowerDefense.Ui
open MergeTowerDefense.Interop.React

let private stat (id: string) (label: string) (value: string) =
    div
        [ "className", box "hud-stat" ]
        [ span [ "className", box "hud-stat-label" ] [ str label ]
          span [ "className", box "hud-stat-value"; "id", box id ] [ str value ] ]

let private waveLabel (game: GameState) =
    match game.Status with
    | Defeated waves -> sprintf "%d survived" waves
    | Playing _ ->
        match game.Wave.Phase with
        | BetweenWaves seconds -> sprintf "%d — next in %.0fs" game.Wave.Number (ceil seconds)
        | Spawning _
        | WaveActive -> string game.Wave.Number

let private livesLabel (game: GameState) =
    match game.Status with
    | Playing lives -> string (Lives.value lives)
    | Defeated _ -> "0"

/// Progress bar showing the countdown between waves (0 → 1 as the delay elapses).
let private waveProgress (game: GameState) =
    match game.Status, game.Wave.Phase with
    | Playing _, BetweenWaves secondsLeft ->
        let total =
            if game.Wave.Number = 0 then Waves.initialDelay
            else Waves.interWaveDelay
        let fraction = max 0.0 (min 1.0 (1.0 - secondsLeft / total))
        div
            [ "className", box "hud-progress" ]
            [ div
                [ "className", box "hud-progress-bar"
                  "style", box (Fable.Core.JsInterop.createObj [ "width", box (sprintf "%.1f%%" (fraction * 100.0)) ]) ]
                [] ]
    | _ -> nothing

let private buyButton (model: UiModel) (dispatch: UiMsg -> unit) (towerType: TowerType) =
    let name = string towerType

    button
        [ "id", box (sprintf "buy-%s" (name.ToLowerInvariant()))
          "className", box (sprintf "hud-buy hud-buy-%s" (name.ToLowerInvariant()))
          "disabled", box (not (canBuy model))
          "onClick", box (fun (_: obj) -> dispatch (Buy towerType)) ]
        [ str (sprintf "%s — %dg" name (nextTowerCost model.Game)) ]

let private sellButton (model: UiModel) (dispatch: UiMsg -> unit) =
    let canSell =
        match model.Game.Status, model.Game.Interaction, model.Hover with
        | Playing _, Idle, Some coord ->
            match Grid.cellAt coord model.Game.Grid with
            | Occupied _ -> true
            | Empty -> false
        | _ -> false

    let label =
        match model.Hover with
        | Some coord ->
            match Grid.cellAt coord model.Game.Grid with
            | Occupied tower -> sprintf "Sell — +%dg" (sellValue tower)
            | Empty -> "Sell"
        | None -> "Sell"

    button
        [ "id", box "sell"
          "className", box "hud-buy hud-sell"
          "disabled", box (not canSell)
          "onClick", box (fun (_: obj) -> dispatch Sell) ]
        [ str label ]

let view (model: UiModel) (dispatch: UiMsg -> unit) =
    let gameOver =
        match model.Game.Status with
        | Defeated waves ->
            div
                [ "className", box "hud-gameover"; "id", box "hud-gameover" ]
                [ span [] [ str (sprintf "Game Over — you survived %d wave(s)." waves) ]
                  button
                      [ "id", box "restart"
                        "className", box "hud-restart"
                        "onClick", box (fun (_: obj) -> dispatch Restart) ]
                      [ str "Restart" ] ]
        | Playing _ -> nothing

    div
        [ "className", box "hud" ]
        [ h1 [ "className", box "hud-title" ] [ str "⚔ Merge Tower Defense" ]
          div
              [ "className", box "hud-stats" ]
              [ stat "hud-gold" "💰 Gold" (string (Gold.value model.Game.Gold))
                stat "hud-wave" "🌊 Wave" (waveLabel model.Game)
                stat "hud-lives" "❤️ Lives" (livesLabel model.Game)
                stat "hud-enemies" "👾 Enemies" (string (List.length model.Game.Enemies)) ]
          waveProgress model.Game
          div
              [ "className", box "hud-shop" ]
              [ buyButton model dispatch Archer
                buyButton model dispatch Cannon
                buyButton model dispatch Frost
                sellButton model dispatch ]
          gameOver
          div
              [ "className", box "hud-notice"; "id", box "hud-notice" ]
              [ match model.Notice with
                | Some(text, _) -> str text
                | None -> str "Drag two matching towers together to merge them." ] ]
