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
open MergeTowerDefense.I18n

let private stat (id: string) (label: string) (value: string) =
    div
        [ "className", box "hud-stat" ]
        [ span [ "className", box "hud-stat-label" ] [ str label ]
          span [ "className", box "hud-stat-value"; "id", box id ] [ str value ] ]

let private waveLabel (lang: Language) (game: GameState) =
    match game.Status with
    | CampaignMenu | TalentScreen | Victory -> "-"
    | Defeated waves -> sprintf "%d %s" waves (t lang "survived")
    | Playing _ ->
        match game.Wave.Phase with
        | BetweenWaves seconds -> sprintf "%d — %s %.0fs" game.Wave.Number (t lang "next_in") (ceil seconds)
        | Spawning _
        | WaveActive -> string game.Wave.Number

let private livesLabel (lang: Language) (game: GameState) =
    match game.Status with
    | CampaignMenu | TalentScreen | Victory -> "-"
    | Playing lives -> string (Lives.value lives)
    | Defeated _ -> "0"

/// Progress bar showing the countdown between waves (0 → 1 as the delay elapses).
let private waveProgress (game: GameState) =
    match game.Status, game.Wave.Phase with
    | CampaignMenu, _ | TalentScreen, _ | Victory, _ -> nothing
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
    let lang = model.Campaign.Language
    let name = string towerType

    button
        [ "id", box (sprintf "buy-%s" (name.ToLowerInvariant()))
          "className", box (sprintf "hud-buy hud-buy-%s" (name.ToLowerInvariant()))
          "disabled", box (not (canBuy model))
          "onClick", box (fun (_: obj) -> dispatch (Buy towerType)) ]
        [ str (sprintf "%s — %dg" (t lang "buy_tower") (nextTowerCost model.Game)) ]

let private sellButton (model: UiModel) (dispatch: UiMsg -> unit) =
    let lang = model.Campaign.Language
    let canSell =
        match model.Game.Status, model.Game.Interaction, model.Hover with
        | Playing _, Idle, Some coord ->
            match Grid.cellAt coord model.Game.Grid with
            | Occupied _ -> true
            | Empty 
            | BlockedCell -> false
        | _ -> false

    let label =
        match model.Hover with
        | Some coord ->
            match Grid.cellAt coord model.Game.Grid with
            | Occupied tower -> sprintf "%s — +%dg" (t lang "sell") (sellValue tower)
            | Empty 
            | BlockedCell -> t lang "sell"
        | None -> t lang "sell"

    button
        [ "id", box "sell"
          "className", box "hud-buy hud-sell"
          "disabled", box (not canSell)
          "onClick", box (fun (_: obj) -> dispatch Sell) ]
        [ str label ]

let private spellButton (model: UiModel) (dispatch: UiMsg -> unit) (spell: ActiveSpell) =
    let cost = ActiveSpell.cost spell
    let canCast =
        match model.Game.Status, model.Game.Interaction with
        | Playing _, Idle -> Gold.value model.Game.Gold >= cost
        | Playing _, CastingSpell current when current = spell -> true
        | _ -> false

    let isCasting =
        match model.Game.Interaction with
        | CastingSpell current when current = spell -> true
        | _ -> false

    let label, cssClass =
        match spell with
        | Fireball -> sprintf "Fireball (%dg)" cost, "hud-spell hud-spell-fireball"
        | FrostNova -> sprintf "Frost Nova (%dg)" cost, "hud-spell hud-spell-frostnova"

    let className = if isCasting then cssClass + " active" else cssClass

    let onClick =
        fun (_: obj) ->
            if isCasting then dispatch (GameMsg CancelDrag)
            else dispatch (GameMsg (StartSpellCast spell))

    button
        [ "className", box className
          "disabled", box (not canCast && not isCasting)
          "onClick", box onClick ]
        [ str label ]

let private settingsOverlay (model: UiModel) (dispatch: UiMsg -> unit) =
    if not model.IsSettingsOpen then nothing
    else
        let lang = model.Campaign.Language
        let langBtn l name =
            button
                [ "className", box (if lang = l then "hud-btn active" else "hud-btn")
                  "onClick", box (fun (_: obj) -> dispatch (ChangeLanguage l)) ]
                [ str name ]
        
        div [ "className", box "settings-overlay active" ]
            [ div [ "className", box "settings-modal"; "dir", box (if isRtl lang then "rtl" else "ltr") ]
                  [ h2 [] [ str (t lang "settings") ]
                    div [ "className", box "settings-section" ]
                        [ h3 [] [ str (t lang "language") ]
                          div [ "className", box "language-buttons" ]
                              [ langBtn EN "English"
                                langBtn TR "Türkçe"
                                langBtn DE "Deutsch"
                                langBtn AR "العربية"
                                langBtn RU "Русский"
                                langBtn ZH "中文" ] ]
                    button
                        [ "className", box "hud-restart"
                          "onClick", box (fun (_: obj) -> dispatch ToggleSettings) ]
                        [ str (t lang "close") ] ] ]

let view (model: UiModel) (dispatch: UiMsg -> unit) =
    let lang = model.Campaign.Language
    let overlay =
        match model.Game.Status with
        | CampaignMenu ->
            let levelNodes =
                Levels.all |> List.map (fun level ->
                    let isUnlocked = Set.contains level.Id model.Campaign.UnlockedLevels
                    
                    let statusClass = 
                        if isUnlocked then "level-node unlocked"
                        else "level-node locked"
                    
                    div [ "className", box "campaign-level-wrapper"
                          "key", box level.Id ]
                        [ div [ "className", box statusClass
                                "onClick", box (fun (_: obj) -> if isUnlocked then dispatch (SelectLevel level.Id)) ]
                              [ span [ "className", box "level-number" ] [ str (string level.Id) ] ]
                          div [ "className", box "level-info" ]
                              [ h3 [] [ str (t lang (sprintf "level_%d_name" level.Id)) ]
                                p [] [ str (t lang (sprintf "level_%d_desc" level.Id)) ] ] ]
                )

            div
                [ "className", box "hud-mainmenu"; "id", box "hud-mainmenu" ]
                [ h1 [] [ str "SEVEN KINGDOMS" ]
                  div [ "className", box "campaign-stats" ]
                      [ div [ "className", box "campaign-stat-item" ]
                            [ str "💰 "; str (string model.Campaign.PersistentGold) ]
                        div [ "className", box "campaign-stat-item" ]
                            [ str "🗺️ Unlocked: "; str (string (Set.count model.Campaign.UnlockedLevels)); str " / "; str (string Levels.all.Length) ] ]
                  div [ "className", box "campaign-map" ] levelNodes
                  button
                      [ "className", box "hud-start"
                        "onClick", box (fun (_: obj) -> dispatch OpenTalentTree) ]
                      [ str (t lang "talents") ]
                  button
                      [ "className", box "hud-start"; "style", box {| marginTop = "10px" |}
                        "onClick", box (fun (_: obj) -> dispatch ToggleSettings) ]
                      [ str (t lang "settings") ] ]
        | TalentScreen ->
            let t = model.Game.Talents
            let costFor level = level + 1
            let totalSpent =
                [ for i in 0 .. t.StartingGoldLevel - 1 -> costFor i ] @
                [ for i in 0 .. t.ArcherDamageLevel - 1 -> costFor i ] @
                [ for i in 0 .. t.CannonDamageLevel - 1 -> costFor i ] @
                [ for i in 0 .. t.SpellCooldownLevel - 1 -> costFor i ]
                |> List.sum
            let available = model.MaxWaveReached - totalSpent

            let talentBtn (name: string) (label: string) (level: int) =
                let cost = costFor level
                let canAfford = available >= cost
                div [ "className", box "hud-talent-item"; "style", box {| display = "flex"; justifyContent = "space-between"; margin = "10px 0" |} ]
                    [ span [] [ str (sprintf "%s (Lv %d)" label level) ]
                      button
                        [ "className", box "hud-btn"
                          "disabled", box (not canAfford)
                          "onClick", box (fun (_: obj) -> dispatch (UpgradeTalent name)) ]
                        [ str (sprintf "Upgrade (%d ★)" cost) ] ]

            div
                [ "className", box "hud-mainmenu" ]
                [ h1 [] [ str "TALENT TREE" ]
                  h3 [] [ str (sprintf "Available Stars: %d ★ (Max Wave: %d)" available model.MaxWaveReached) ]
                  div [ "className", box "hud-rules"; "style", box {| textAlign = "left" |} ]
                      [ talentBtn "Gold" "Starting Gold (+20)" t.StartingGoldLevel
                        talentBtn "Archer" "Archer Dmg (+1)" t.ArcherDamageLevel
                        talentBtn "Cannon" "Cannon Dmg (+3)" t.CannonDamageLevel
                        talentBtn "Spell" "Spell Cost (-15%)" t.SpellCooldownLevel ]
                  br [] []
                  button
                      [ "className", box "hud-start"
                        "onClick", box (fun (_: obj) -> dispatch CloseTalentTree) ]
                      [ str "Back to Menu" ] ]
        | Defeated waves ->
            div
                [ "className", box "hud-gameover"; "id", box "hud-gameover" ]
                [ span [] [ str (sprintf "%s — %d %s." (t lang "defeated") waves (t lang "survived")) ]
                  button
                      [ "id", box "restart"
                        "className", box "hud-restart"
                        "onClick", box (fun (_: obj) -> dispatch Restart) ]
                      [ str (t lang "restart") ] ]
        | Victory ->
            div
                [ "className", box "hud-gameover" ]
                [ span [] [ str (t lang "victory") ]
                  button
                      [ "className", box "hud-restart"
                        "onClick", box (fun (_: obj) -> dispatch Restart) ]
                      [ str (t lang "continue") ] ]
        | Playing _ -> nothing

    div
        [ "className", box "hud" ]
        [ h1 [ "className", box "hud-title" ] [ str "⚔ Merge Tower Defense" ]
          div
              [ "className", box "hud-stats" ]
              [ stat "hud-gold" "💰 Gold" (string (Gold.value model.Game.Gold))
                stat "hud-wave" (sprintf "🌊 %s" (t lang "wave")) (waveLabel lang model.Game)
                stat "hud-lives" (sprintf "❤️ %s" (t lang "lives")) (livesLabel lang model.Game)
                stat "hud-enemies" "👾 Enemies" (string (List.length model.Game.Enemies)) ]
          waveProgress model.Game
          div
              [ "className", box "hud-shop" ]
              [ buyButton model dispatch Archer
                buyButton model dispatch Cannon
                buyButton model dispatch Frost
                sellButton model dispatch ]
          div
              [ "className", box "hud-spells" ]
              [ spellButton model dispatch Fireball
                spellButton model dispatch FrostNova ]
          overlay
          settingsOverlay model dispatch
          div
              [ "className", box "hud-notice"; "id", box "hud-notice" ]
              [ match model.Notice with
                | Some(text, _) -> str text
                | None -> str "Drag two matching towers together to merge them." ] ]
