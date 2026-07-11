module MergeTowerDefense.I18n

open MergeTowerDefense.Shared

let t (lang: Language) (key: string) : string =
    match key, lang with
    // Main Menu
    | "start_game", EN -> "Start Game"
    | "start_game", TR -> "Oyuna Başla"
    | "start_game", DE -> "Spiel Starten"
    | "start_game", AR -> "ابدأ اللعبة"
    | "start_game", RU -> "Начать игру"
    | "start_game", ZH -> "开始游戏"

    | "settings", EN -> "Settings"
    | "settings", TR -> "Ayarlar"
    | "settings", DE -> "Einstellungen"
    | "settings", AR -> "الإعدادات"
    | "settings", RU -> "Настройки"
    | "settings", ZH -> "设置"

    // Settings Modal
    | "language", EN -> "Language"
    | "language", TR -> "Dil"
    | "language", DE -> "Sprache"
    | "language", AR -> "اللغة"
    | "language", RU -> "Язык"
    | "language", ZH -> "语言"

    | "close", EN -> "Close"
    | "close", TR -> "Kapat"
    | "close", DE -> "Schließen"
    | "close", AR -> "إغلاق"
    | "close", RU -> "Закрыть"
    | "close", ZH -> "关闭"

    // HUD / Gameplay
    | "wave", EN -> "Wave"
    | "wave", TR -> "Dalga"
    | "wave", DE -> "Welle"
    | "wave", AR -> "موجة"
    | "wave", RU -> "Волна"
    | "wave", ZH -> "波数"

    | "lives", EN -> "Lives"
    | "lives", TR -> "Can"
    | "lives", DE -> "Leben"
    | "lives", AR -> "أرواح"
    | "lives", RU -> "Жизни"
    | "lives", ZH -> "生命"

    | "survived", EN -> "survived"
    | "survived", TR -> "hayatta kaldı"
    | "survived", DE -> "überlebt"
    | "survived", AR -> "نجا"
    | "survived", RU -> "выжил"
    | "survived", ZH -> "存活"

    | "next_in", EN -> "next in"
    | "next_in", TR -> "sonraki:"
    | "next_in", DE -> "nächste in"
    | "next_in", AR -> "التالي في"
    | "next_in", RU -> "следующая через"
    | "next_in", ZH -> "下一波在"

    | "start_next_wave", EN -> "Start Next Wave"
    | "start_next_wave", TR -> "Sonraki Dalgayı Başlat"
    | "start_next_wave", DE -> "Nächste Welle starten"
    | "start_next_wave", AR -> "ابدأ الموجة التالية"
    | "start_next_wave", RU -> "Начать следующую волну"
    | "start_next_wave", ZH -> "开始下一波"

    // End Game
    | "victory", EN -> "Victory!"
    | "victory", TR -> "Zafer!"
    | "victory", DE -> "Sieg!"
    | "victory", AR -> "انتصار!"
    | "victory", RU -> "Победа!"
    | "victory", ZH -> "胜利！"

    | "defeated", EN -> "Defeated"
    | "defeated", TR -> "Yenildin"
    | "defeated", DE -> "Niederlage"
    | "defeated", AR -> "هزيمة"
    | "defeated", RU -> "Поражение"
    | "defeated", ZH -> "失败"

    | "restart", EN -> "Restart"
    | "restart", TR -> "Yeniden Başlat"
    | "restart", DE -> "Neustart"
    | "restart", AR -> "إعادة التشغيل"
    | "restart", RU -> "Перезапуск"
    | "restart", ZH -> "重新开始"
    
    | "continue", EN -> "Continue"
    | "continue", TR -> "Devam Et"
    | "continue", DE -> "Weiter"
    | "continue", AR -> "استمرار"
    | "continue", RU -> "Продолжить"
    | "continue", ZH -> "继续"

    // Buttons
    | "sell", EN -> "Sell"
    | "sell", TR -> "Sat"
    | "sell", DE -> "Verkaufen"
    | "sell", AR -> "بيع"
    | "sell", RU -> "Продать"
    | "sell", ZH -> "出售"

    | "buy_tower", EN -> "Buy Tower"
    | "buy_tower", TR -> "Kule Al"
    | "buy_tower", DE -> "Turm kaufen"
    | "buy_tower", AR -> "شراء برج"
    | "buy_tower", RU -> "Купить башню"
    | "buy_tower", ZH -> "购买塔"
    
    | "talents", EN -> "Talents"
    | "talents", TR -> "Yetenekler"
    | "talents", DE -> "Talente"
    | "talents", AR -> "المواهب"
    | "talents", RU -> "Таланты"
    | "talents", ZH -> "天赋"

    // Levels
    | "level_1_name", EN -> "Rookie Bootcamp"
    | "level_1_name", TR -> "Çaylak Eğitim Kampı"
    | "level_1_name", DE -> "Rekruten-Lager"
    | "level_1_name", AR -> "معسكر المجندين"
    | "level_1_name", RU -> "Лагерь новобранцев"
    | "level_1_name", ZH -> "新兵训练营"

    | "level_1_desc", EN -> "Defend the pass with basic infantry."
    | "level_1_desc", TR -> "Geçidi temel piyadelerle savun."
    | "level_1_desc", DE -> "Verteidige den Pass mit grundlegender Infanterie."
    | "level_1_desc", AR -> "الدفاع عن الممر مع المشاة الأساسية."
    | "level_1_desc", RU -> "Защитите перевал с помощью базовой пехоты."
    | "level_1_desc", ZH -> "用基础步兵保卫山口。"

    | "level_2_name", EN -> "The River Crossing"
    | "level_2_name", TR -> "Nehir Geçişi"
    | "level_2_name", DE -> "Die Flussüberquerung"
    | "level_2_name", AR -> "عبور النهر"
    | "level_2_name", RU -> "Переправа через реку"
    | "level_2_name", ZH -> "渡河"

    | "level_2_desc", EN -> "Hold the bridge against heavier forces."
    | "level_2_desc", TR -> "Köprüyü ağır güçlere karşı tut."
    | "level_2_desc", DE -> "Halte die Brücke gegen stärkere Kräfte."
    | "level_2_desc", AR -> "السيطرة على الجسر ضد قوات أثقل."
    | "level_2_desc", RU -> "Удерживайте мост против более сильных войск."
    | "level_2_desc", ZH -> "抵抗重兵保卫桥梁。"

    | "level_3_name", EN -> "Volcanic Keep"
    | "level_3_name", TR -> "Volkanik Kale"
    | "level_3_name", DE -> "Vulkanische Festung"
    | "level_3_name", AR -> "القلعة البركانية"
    | "level_3_name", RU -> "Вулканическая крепость"
    | "level_3_name", ZH -> "火山要塞"

    | "level_3_desc", EN -> "Use Frost magic to slow the horde."
    | "level_3_desc", TR -> "Sürüyü yavaşlatmak için Buz büyüsü kullan."
    | "level_3_desc", DE -> "Verwende Frostmagie, um die Horde zu verlangsamen."
    | "level_3_desc", AR -> "استخدم سحر الصقيع لإبطاء الحشد."
    | "level_3_desc", RU -> "Используйте магию Льда, чтобы замедлить орду."
    | "level_3_desc", ZH -> "使用冰霜魔法减缓部落。"

    | "level_4_name", EN -> "Winter Siege"
    | "level_4_name", TR -> "Kış Kuşatması"
    | "level_4_name", DE -> "Winterbelagerung"
    | "level_4_name", AR -> "حصار الشتاء"
    | "level_4_name", RU -> "Зимняя осада"
    | "level_4_name", ZH -> "冬季围城"

    | "level_4_desc", EN -> "Command all forces to defend the Citadel."
    | "level_4_desc", TR -> "Hisarı savunmak için tüm güçlere komuta et."
    | "level_4_desc", DE -> "Befehlige alle Streitkräfte, um die Zitadelle zu verteidigen."
    | "level_4_desc", AR -> "قيادة جميع القوات للدفاع عن القلعة."
    | "level_4_desc", RU -> "Командуйте всеми силами для защиты Цитадели."
    | "level_4_desc", ZH -> "指挥所有部队保卫城堡。"

    | _, EN -> key
    | _, _ -> key

let isRtl (lang: Language) : bool =
    match lang with
    | AR -> true
    | _ -> false
