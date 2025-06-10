// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use edit::arena::scratch_arena;
use edit::helpers::AsciiStringHelpers;
use edit::sys;

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum LocId {
    Ctrl,
    Alt,
    Shift,

    Ok,
    Yes,
    No,
    Cancel,
    Always,

    // File menu
    File,
    FileNew,
    FileOpen,
    FileSave,
    FileSaveAs,
    FileClose,
    FileExit,
    FileGoto,

    // Edit menu
    Edit,
    EditUndo,
    EditRedo,
    EditCut,
    EditCopy,
    EditPaste,
    EditFind,
    EditReplace,
    EditSelectAll,

    // View menu
    View,
    ViewFocusStatusbar,
    ViewWordWrap,
    ViewGoToFile,

    // Help menu
    Help,
    HelpAbout,

    // Exit dialog
    UnsavedChangesDialogTitle,
    UnsavedChangesDialogDescription,
    UnsavedChangesDialogYes,
    UnsavedChangesDialogNo,

    // About dialog
    AboutDialogTitle,
    AboutDialogVersion,

    // Shown when the clipboard size exceeds the limit for OSC 52
    LargeClipboardWarningLine1,
    LargeClipboardWarningLine2,
    LargeClipboardWarningLine3,
    SuperLargeClipboardWarning,

    // Warning dialog
    WarningDialogTitle,

    // Error dialog
    ErrorDialogTitle,
    ErrorIcuMissing,

    SearchNeedleLabel,
    SearchReplacementLabel,
    SearchMatchCase,
    SearchWholeWord,
    SearchUseRegex,
    SearchReplaceAll,
    SearchClose,

    EncodingReopen,
    EncodingConvert,

    IndentationTabs,
    IndentationSpaces,

    SaveAsDialogPathLabel,
    SaveAsDialogNameLabel,

    FileOverwriteWarning,
    FileOverwriteWarningDescription,

    Count,
}

#[allow(non_camel_case_types)]
#[derive(Clone, Copy, PartialEq, Eq)]
enum LangId {
    // Base language. It's always the first one.
    en,

    // Other languages. Sorted alphabetically.
    de,
    es,
    fr,
    it,
    ja,
    ko,
    pt_br,
    ru,
    zh_hans,
    zh_hant,

    Count,
}

#[rustfmt::skip]
const S_LANG_LUT: [[&str; LangId::Count as usize]; LocId::Count as usize] = [
    // Ctrl (the keyboard key)
    [
        /* en      */ "Ctrl",
        /* de      */ "Strg",
        /* es      */ "Ctrl",
        /* fr      */ "Ctrl",
        /* it      */ "Ctrl",
        /* ja      */ "Ctrl",
        /* ko      */ "Ctrl",
        /* pt_br   */ "Ctrl",
        /* ru      */ "Ctrl",
        /* zh_hans */ "Ctrl",
        /* zh_hant */ "Ctrl",
    ],
    // Alt (the keyboard key)
    [
        /* en      */ "Alt",
        /* de      */ "Alt",
        /* es      */ "Alt",
        /* fr      */ "Alt",
        /* it      */ "Alt",
        /* ja      */ "Alt",
        /* ko      */ "Alt",
        /* pt_br   */ "Alt",
        /* ru      */ "Alt",
        /* zh_hans */ "Alt",
        /* zh_hant */ "Alt",
    ],
    // Shift (the keyboard key)
    [
        /* en      */ "Shift",
        /* de      */ "Umschalt",
        /* es      */ "Mayús",
        /* fr      */ "Maj",
        /* it      */ "Maiusc",
        /* ja      */ "Shift",
        /* ko      */ "Shift",
        /* pt_br   */ "Shift",
        /* ru      */ "Shift",
        /* zh_hans */ "Shift",
        /* zh_hant */ "Shift",
    ],

    // Ok (used as a common dialog button)
    [
        /* en      */ "Ok",
        /* de      */ "OK",
        /* es      */ "Aceptar",
        /* fr      */ "OK",
        /* it      */ "OK",
        /* ja      */ "OK",
        /* ko      */ "확인",
        /* pt_br   */ "OK",
        /* ru      */ "ОК",
        /* zh_hans */ "确定",
        /* zh_hant */ "確定",
    ],
    // Yes (used as a common dialog button)
    [
        /* en      */ "Yes",
        /* de      */ "Ja",
        /* es      */ "Sí",
        /* fr      */ "Oui",
        /* it      */ "Sì",
        /* ja      */ "はい",
        /* ko      */ "예",
        /* pt_br   */ "Sim",
        /* ru      */ "Да",
        /* zh_hans */ "是",
        /* zh_hant */ "是",
    ],
    // No (used as a common dialog button)
    [
        /* en      */ "No",
        /* de      */ "Nein",
        /* es      */ "No",
        /* fr      */ "Non",
        /* it      */ "No",
        /* ja      */ "いいえ",
        /* ko      */ "아니오",
        /* pt_br   */ "Não",
        /* ru      */ "Нет",
        /* zh_hans */ "否",
        /* zh_hant */ "否",
    ],
    // Cancel (used as a common dialog button)
    [
        /* en      */ "Cancel",
        /* de      */ "Abbrechen",
        /* es      */ "Cancelar",
        /* fr      */ "Annuler",
        /* it      */ "Annulla",
        /* ja      */ "キャンセル",
        /* ko      */ "취소",
        /* pt_br   */ "Cancelar",
        /* ru      */ "Отмена",
        /* zh_hans */ "取消",
        /* zh_hant */ "取消",
    ],
    // Always (used as a common dialog button)
    [
        /* en      */ "Always",
        /* de      */ "Immer",
        /* es      */ "Siempre",
        /* fr      */ "Toujours",
        /* it      */ "Sempre",
        /* ja      */ "常に",
        /* ko      */ "항상",
        /* pt_br   */ "Sempre",
        /* ru      */ "Всегда",
        /* zh_hans */ "总是",
        /* zh_hant */ "總是",
    ],

    // File (a menu bar item)
    [
        /* en      */ "File",
        /* de      */ "Datei",
        /* es      */ "Archivo",
        /* fr      */ "Fichier",
        /* it      */ "File",
        /* ja      */ "ファイル",
        /* ko      */ "파일",
        /* pt_br   */ "Arquivo",
        /* ru      */ "Файл",
        /* zh_hans */ "文件",
        /* zh_hant */ "檔案",
    ],
    // FileNew
    [
        /* en      */ "New File",
        /* de      */ "Neue Datei",
        /* es      */ "Nuevo archivo",
        /* fr      */ "Nouveau fichier",
        /* it      */ "Nuovo file",
        /* ja      */ "新規ファイル",
        /* ko      */ "새 파일",
        /* pt_br   */ "Novo arquivo",
        /* ru      */ "Новый файл",
        /* zh_hans */ "新建文件",
        /* zh_hant */ "新增檔案",
    ],
    // FileOpen
    [
        /* en      */ "Open File…",
        /* de      */ "Datei öffnen…",
        /* es      */ "Abrir archivo…",
        /* fr      */ "Ouvrir un fichier…",
        /* it      */ "Apri file…",
        /* ja      */ "ファイルを開く…",
        /* ko      */ "파일 열기…",
        /* pt_br   */ "Abrir arquivo…",
        /* ru      */ "Открыть файл…",
        /* zh_hans */ "打开文件…",
        /* zh_hant */ "開啟檔案…",
    ],
    // FileSave
    [
        /* en      */ "Save",
        /* de      */ "Speichern",
        /* es      */ "Guardar",
        /* fr      */ "Enregistrer",
        /* it      */ "Salva",
        /* ja      */ "保存",
        /* ko      */ "저장",
        /* pt_br   */ "Salvar",
        /* ru      */ "Сохранить",
        /* zh_hans */ "保存",
        /* zh_hant */ "儲存",
    ],
    // FileSaveAs
    [
        /* en      */ "Save As…",
        /* de      */ "Speichern unter…",
        /* es      */ "Guardar como…",
        /* fr      */ "Enregistrer sous…",
        /* it      */ "Salva come…",
        /* ja      */ "名前を付けて保存…",
        /* ko      */ "다른 이름으로 저장…",
        /* pt_br   */ "Salvar como…",
        /* ru      */ "Сохранить как…",
        /* zh_hans */ "另存为…",
        /* zh_hant */ "另存新檔…",
    ],
    // FileClose
    [
        /* en      */ "Close File",
        /* de      */ "Datei schließen",
        /* es      */ "Cerrar archivo",
        /* fr      */ "Fermer le fichier",
        /* it      */ "Chiudi file",
        /* ja      */ "ファイルを閉じる",
        /* ko      */ "파일 닫기",
        /* pt_br   */ "Fechar arquivo",
        /* ru      */ "Закрыть файл",
        /* zh_hans */ "关闭文件",
        /* zh_hant */ "關閉檔案",
    ],
    // FileExit
    [
        /* en      */ "Exit",
        /* de      */ "Beenden",
        /* es      */ "Salir",
        /* fr      */ "Quitter",
        /* it      */ "Esci",
        /* ja      */ "終了",
        /* ko      */ "종료",
        /* pt_br   */ "Sair",
        /* ru      */ "Выход",
        /* zh_hans */ "退出",
        /* zh_hant */ "退出",
    ],
    // FileGoto
    [
        /* en      */ "Go to Line:Column…",
        /* de      */ "Gehe zu Zeile:Spalte…",
        /* es      */ "Ir a línea:columna…",
        /* fr      */ "Aller à la ligne:colonne…",
        /* it      */ "Vai a riga:colonna…",
        /* ja      */ "行:列へ移動…",
        /* ko      */ "행:열로 이동…",
        /* pt_br   */ "Ir para linha:coluna…",
        /* ru      */ "Перейти к строке:столбцу…",
        /* zh_hans */ "转到行:列…",
        /* zh_hant */ "跳至行:列…",
    ],

    // Edit (a menu bar item)
    [
        /* en      */ "Edit",
        /* de      */ "Bearbeiten",
        /* es      */ "Editar",
        /* fr      */ "Édition",
        /* it      */ "Modifica",
        /* ja      */ "編集",
        /* ko      */ "편집",
        /* pt_br   */ "Editar",
        /* ru      */ "Правка",
        /* zh_hans */ "编辑",
        /* zh_hant */ "編輯",
    ],
    // EditUndo
    [
        /* en      */ "Undo",
        /* de      */ "Rückgängig",
        /* es      */ "Deshacer",
        /* fr      */ "Annuler",
        /* it      */ "Annulla",
        /* ja      */ "元に戻す",
        /* ko      */ "실행 취소",
        /* pt_br   */ "Desfazer",
        /* ru      */ "Отменить",
        /* zh_hans */ "撤销",
        /* zh_hant */ "復原",
    ],
    // EditRedo
    [
        /* en      */ "Redo",
        /* de      */ "Wiederholen",
        /* es      */ "Rehacer",
        /* fr      */ "Rétablir",
        /* it      */ "Ripeti",
        /* ja      */ "やり直し",
        /* ko      */ "다시 실행",
        /* pt_br   */ "Refazer",
        /* ru      */ "Повторить",
        /* zh_hans */ "重做",
        /* zh_hant */ "重做",
    ],
    // EditCut
    [
        /* en      */ "Cut",
        /* de      */ "Ausschneiden",
        /* es      */ "Cortar",
        /* fr      */ "Couper",
        /* it      */ "Taglia",
        /* ja      */ "切り取り",
        /* ko      */ "잘라내기",
        /* pt_br   */ "Cortar",
        /* ru      */ "Вырезать",
        /* zh_hans */ "剪切",
        /* zh_hant */ "剪下",
    ],
    // EditCopy
    [
        /* en      */ "Copy",
        /* de      */ "Kopieren",
        /* es      */ "Copiar",
        /* fr      */ "Copier",
        /* it      */ "Copia",
        /* ja      */ "コピー",
        /* ko      */ "복사",
        /* pt_br   */ "Copiar",
        /* ru      */ "Копировать",
        /* zh_hans */ "复制",
        /* zh_hant */ "複製",
    ],
    // EditPaste
    [
        /* en      */ "Paste",
        /* de      */ "Einfügen",
        /* es      */ "Pegar",
        /* fr      */ "Coller",
        /* it      */ "Incolla",
        /* ja      */ "貼り付け",
        /* ko      */ "붙여넣기",
        /* pt_br   */ "Colar",
        /* ru      */ "Вставить",
        /* zh_hans */ "粘贴",
        /* zh_hant */ "貼上",
    ],
    // EditFind
    [
        /* en      */ "Find",
        /* de      */ "Suchen",
        /* es      */ "Buscar",
        /* fr      */ "Rechercher",
        /* it      */ "Trova",
        /* ja      */ "検索",
        /* ko      */ "찾기",
        /* pt_br   */ "Encontrar",
        /* ru      */ "Найти",
        /* zh_hans */ "查找",
        /* zh_hant */ "尋找",
    ],
    // EditReplace
    [
        /* en      */ "Replace",
        /* de      */ "Ersetzen",
        /* es      */ "Reemplazar",
        /* fr      */ "Remplacer",
        /* it      */ "Sostituisci",
        /* ja      */ "置換",
        /* ko      */ "바꾸기",
        /* pt_br   */ "Substituir",
        /* ru      */ "Заменить",
        /* zh_hans */ "替换",
        /* zh_hant */ "取代",
    ],
    // EditSelectAll
    [
        /* en      */ "Select All",
        /* de      */ "Alles auswählen",
        /* es      */ "Seleccionar todo",
        /* fr      */ "Tout sélectionner",
        /* it      */ "Seleziona tutto",
        /* ja      */ "すべて選択",
        /* ko      */ "모두 선택",
        /* pt_br   */ "Selecionar tudo",
        /* ru      */ "Выделить всё",
        /* zh_hans */ "全选",
        /* zh_hant */ "全選"
    ],

    // View (a menu bar item)
    [
        /* en      */ "View",
        /* de      */ "Ansicht",
        /* es      */ "Ver",
        /* fr      */ "Affichage",
        /* it      */ "Visualizza",
        /* ja      */ "表示",
        /* ko      */ "보기",
        /* pt_br   */ "Exibir",
        /* ru      */ "Вид",
        /* zh_hans */ "视图",
        /* zh_hant */ "檢視",
    ],
    // ViewFocusStatusbar
    [
        /* en      */ "Focus Statusbar",
        /* de      */ "Statusleiste fokussieren",
        /* es      */ "Enfocar barra de estado",
        /* fr      */ "Activer la barre d’état",
        /* it      */ "Attiva barra di stato",
        /* ja      */ "ステータスバーにフォーカス",
        /* ko      */ "상태 표시줄로 포커스 이동",
        /* pt_br   */ "Focar barra de status",
        /* ru      */ "Фокус на строку состояния",
        /* zh_hans */ "聚焦状态栏",
        /* zh_hant */ "聚焦狀態列",
    ],
    // ViewWordWrap
    [
        /* en      */ "Word Wrap",
        /* de      */ "Zeilenumbruch",
        /* es      */ "Ajuste de línea",
        /* fr      */ "Retour automatique à la ligne",
        /* it      */ "A capo automatico",
        /* ja      */ "折り返し",
        /* ko      */ "자동 줄 바꿈",
        /* pt_br   */ "Quebra de linha",
        /* ru      */ "Перенос слов",
        /* zh_hans */ "自动换行",
        /* zh_hant */ "自動換行",
    ],
    // ViewGoToFile
    [
        /* en      */ "Go to File…",
        /* de      */ "Gehe zu Datei…",
        /* es      */ "Ir a archivo…",
        /* fr      */ "Aller au fichier…",
        /* it      */ "Vai al file…",
        /* ja      */ "ファイルへ移動…",
        /* ko      */ "파일로 이동…",
        /* pt_br   */ "Ir para arquivo…",
        /* ru      */ "Перейти к файлу…",
        /* zh_hans */ "转到文件…",
        /* zh_hant */ "跳至檔案…",
    ],

    // Help (a menu bar item)
    [
        /* en      */ "Help",
        /* de      */ "Hilfe",
        /* es      */ "Ayuda",
        /* fr      */ "Aide",
        /* it      */ "Aiuto",
        /* ja      */ "ヘルプ",
        /* ko      */ "도움말",
        /* pt_br   */ "Ajuda",
        /* ru      */ "Помощь",
        /* zh_hans */ "帮助",
        /* zh_hant */ "幫助",
    ],
    // HelpAbout
    [
        /* en      */ "About",
        /* de      */ "Über",
        /* es      */ "Acerca de",
        /* fr      */ "À propos",
        /* it      */ "Informazioni",
        /* ja      */ "情報",
        /* ko      */ "정보",
        /* pt_br   */ "Sobre",
        /* ru      */ "О программе",
        /* zh_hans */ "关于",
        /* zh_hant */ "關於",
    ],

    // UnsavedChangesDialogTitle
    [
        /* en      */ "Unsaved Changes",
        /* de      */ "Ungespeicherte Änderungen",
        /* es      */ "Cambios sin guardar",
        /* fr      */ "Modifications non enregistrées",
        /* it      */ "Modifiche non salvate",
        /* ja      */ "未保存の変更",
        /* ko      */ "저장되지 않은 변경 사항",
        /* pt_br   */ "Alterações não salvas",
        /* ru      */ "Несохраненные изменения",
        /* zh_hans */ "未保存的更改",
        /* zh_hant */ "未儲存的變更",
    ],
    // UnsavedChangesDialogDescription
    [
        /* en      */ "Do you want to save the changes you made?",
        /* de      */ "Möchten Sie die vorgenommenen Änderungen speichern?",
        /* es      */ "¿Desea guardar los cambios realizados?",
        /* fr      */ "Voulez-vous enregistrer les modifications apportées ?",
        /* it      */ "Vuoi salvare le modifiche apportate?",
        /* ja      */ "変更内容を保存しますか？",
        /* ko      */ "변경한 내용을 저장하시겠습니까?",
        /* pt_br   */ "Deseja salvar as alterações feitas?",
        /* ru      */ "Вы хотите сохранить внесённые изменения?",
        /* zh_hans */ "您要保存所做的更改吗？",
        /* zh_hant */ "您要保存所做的變更嗎？",
    ],
    // UnsavedChangesDialogYes
    [
        /* en      */ "Save",
        /* de      */ "Speichern",
        /* es      */ "Guardar",
        /* fr      */ "Enregistrer",
        /* it      */ "Salva",
        /* ja      */ "保存する",
        /* ko      */ "저장",
        /* pt_br   */ "Salvar",
        /* ru      */ "Сохранить",
        /* zh_hans */ "保存",
        /* zh_hant */ "儲存",
    ],
    // UnsavedChangesDialogNo
    [
        /* en      */ "Don't Save",
        /* de      */ "Nicht speichern",
        /* es      */ "No guardar",
        /* fr      */ "Ne pas enregistrer",
        /* it      */ "Non salvare",
        /* ja      */ "保存しない",
        /* ko      */ "저장 안 함",
        /* pt_br   */ "Não salvar",
        /* ru      */ "Не сохранять",
        /* zh_hans */ "不保存",
        /* zh_hant */ "不儲存",
    ],

    // AboutDialogTitle
    [
        /* en      */ "About",
        /* de      */ "Über",
        /* es      */ "Acerca de",
        /* fr      */ "À propos",
        /* it      */ "Informazioni",
        /* ja      */ "情報",
        /* ko      */ "정보",
        /* pt_br   */ "Sobre",
        /* ru      */ "О программе",
        /* zh_hans */ "关于",
        /* zh_hant */ "關於",
    ],
    // AboutDialogVersion
    [
        /* en      */ "Version: ",
        /* de      */ "Version: ",
        /* es      */ "Versión: ",
        /* fr      */ "Version : ",
        /* it      */ "Versione: ",
        /* ja      */ "バージョン: ",
        /* ko      */ "버전: ",
        /* pt_br   */ "Versão: ",
        /* ru      */ "Версия: ",
        /* zh_hans */ "版本: ",
        /* zh_hant */ "版本: ",
    ],

    // Shown when the clipboard size exceeds the limit for OSC 52
    // LargeClipboardWarningLine1
    [
        /* en      */ "Text you copy is shared with the terminal clipboard.",
        /* de      */ "Der kopierte Text wird mit der Terminal-Zwischenablage geteilt.",
        /* es      */ "El texto que copies se comparte con el portapapeles del terminal.",
        /* fr      */ "Le texte que vous copiez est partagé avec le presse-papiers du terminal.",
        /* it      */ "Il testo copiato viene condiviso con gli appunti del terminale.",
        /* ja      */ "コピーしたテキストはターミナルのクリップボードと共有されます。",
        /* ko      */ "복사한 텍스트가 터미널 클립보드와 공유됩니다.",
        /* pt_br   */ "O texto copiado é compartilhado com a área de transferência do terminal.",
        /* ru      */ "Скопированный текст передаётся в буфер обмена терминала.",
        /* zh_hans */ "你复制的文本将共享到终端剪贴板。",
        /* zh_hant */ "您複製的文字將會與終端機剪貼簿分享。",
    ],
    // LargeClipboardWarningLine2
    [
        /* en      */ "You copied {size} which may take a long time to share.",
        /* de      */ "Sie haben {size} kopiert. Das Weitergeben könnte länger dauern.",
        /* es      */ "Copiaste {size}, lo que puede tardar en compartirse.",
        /* fr      */ "Vous avez copié {size}, ce qui peut être long à partager.",
        /* it      */ "Hai copiato {size}, potrebbe richiedere molto tempo per condividerlo.",
        /* ja      */ "{size} をコピーしました。共有に時間がかかる可能性があります。",
        /* ko      */ "{size}를 복사했습니다. 공유하는 데 시간이 오래 걸릴 수 있습니다.",
        /* pt_br   */ "Você copiou {size}, o que pode demorar para compartilhar.",
        /* ru      */ "Вы скопировали {size}; передача может занять много времени.",
        /* zh_hans */ "你复制了 {size}，共享可能需要较长时间。",
        /* zh_hant */ "您已複製 {size}，共享可能需要較長時間。",
    ],
    // LargeClipboardWarningLine3
    [
        /* en      */ "Do you want to send it anyway?",
        /* de      */ "Möchten Sie es trotzdem senden?",
        /* es      */ "¿Desea enviarlo de todas formas?",
        /* fr      */ "Voulez-vous quand même l’envoyer ?",
        /* it      */ "Vuoi inviarlo comunque?",
        /* ja      */ "それでも送信しますか？",
        /* ko      */ "그래도 전송하시겠습니까?",
        /* pt_br   */ "Deseja enviar mesmo assim?",
        /* ru      */ "Отправить в любом случае?",
        /* zh_hans */ "仍要发送吗？",
        /* zh_hant */ "仍要傳送嗎？",
    ],
    // SuperLargeClipboardWarning (as an alternative to LargeClipboardWarningLine2 and 3)
    [
        /* en      */ "The text you copied is too large to be shared.",
        /* de      */ "Der kopierte Text ist zu groß, um geteilt zu werden.",
        /* es      */ "El texto que copiaste es demasiado grande para compartirse.",
        /* fr      */ "Le texte que vous avez copié est trop volumineux pour être partagé.",
        /* it      */ "Il testo copiato è troppo grande per essere condiviso.",
        /* ja      */ "コピーしたテキストは大きすぎて共有できません。",
        /* ko      */ "복사한 텍스트가 너무 커서 공유할 수 없습니다.",
        /* pt_br   */ "O texto copiado é grande demais para ser compartilhado.",
        /* ru      */ "Скопированный текст слишком велик для передачи.",
        /* zh_hans */ "你复制的文本过大，无法共享。",
        /* zh_hant */ "您複製的文字過大，無法分享。",
    ],

    // WarningDialogTitle
    [
        /* en      */ "Warning",
        /* de      */ "Warnung",
        /* es      */ "Advertencia",
        /* fr      */ "Avertissement",
        /* it      */ "Avviso",
        /* ja      */ "警告",
        /* ko      */ "경고",
        /* pt_br   */ "Aviso",
        /* ru      */ "Предупреждение",
        /* zh_hans */ "警告",
        /* zh_hant */ "警告",
    ],

    // ErrorDialogTitle
    [
        /* en      */ "Error",
        /* de      */ "Fehler",
        /* es      */ "Error",
        /* fr      */ "Erreur",
        /* it      */ "Errore",
        /* ja      */ "エラー",
        /* ko      */ "오류",
        /* pt_br   */ "Erro",
        /* ru      */ "Ошибка",
        /* zh_hans */ "错误",
        /* zh_hant */ "錯誤",
    ],
    // ErrorIcuMissing
    [
        /* en      */ "This operation requires the ICU library",
        /* de      */ "Diese Operation erfordert die ICU-Bibliothek",
        /* es      */ "Esta operación requiere la biblioteca ICU",
        /* fr      */ "Cette opération nécessite la bibliothèque ICU",
        /* it      */ "Questa operazione richiede la libreria ICU",
        /* ja      */ "この操作にはICUライブラリが必要です",
        /* ko      */ "이 작업에는 ICU 라이브러리가 필요합니다",
        /* pt_br   */ "Esta operação requer a biblioteca ICU",
        /* ru      */ "Эта операция требует наличия библиотеки ICU",
        /* zh_hans */ "此操作需要 ICU 库",
        /* zh_hant */ "此操作需要 ICU 庫",
    ],

    // SearchNeedleLabel (for input field)
    [
        /* en      */ "Find:",
        /* de      */ "Suchen:",
        /* es      */ "Buscar:",
        /* fr      */ "Rechercher :",
        /* it      */ "Trova:",
        /* ja      */ "検索:",
        /* ko      */ "찾기:",
        /* pt_br   */ "Encontrar:",
        /* ru      */ "Найти:",
        /* zh_hans */ "查找:",
        /* zh_hant */ "尋找:",
    ],
    // SearchReplacementLabel (for input field)
    [
        /* en      */ "Replace:",
        /* de      */ "Ersetzen:",
        /* es      */ "Reemplazar:",
        /* fr      */ "Remplacer :",
        /* it      */ "Sostituire:",
        /* ja      */ "置換:",
        /* ko      */ "바꾸기:",
        /* pt_br   */ "Substituir:",
        /* ru      */ "Замена:",
        /* zh_hans */ "替换:",
        /* zh_hant */ "替換:",
    ],
    // SearchMatchCase (toggle)
    [
        /* en      */ "Match Case",
        /* de      */ "Groß/Klein",
        /* es      */ "May/Min",
        /* fr      */ "Resp. la casse",
        /* it      */ "Maius/minus",
        /* ja      */ "大/小文字を区別",
        /* ko      */ "대소문자",
        /* pt_br   */ "Maius/minus",
        /* ru      */ "Регистр",
        /* zh_hans */ "区分大小写",
        /* zh_hant */ "區分大小寫",
    ],
    // SearchWholeWord (toggle)
    [
        /* en      */ "Whole Word",
        /* de      */ "Ganzes Wort",
        /* es      */ "Palabra",
        /* fr      */ "Mot entier",
        /* it      */ "Parola",
        /* ja      */ "単語全体",
        /* ko      */ "전체 단어",
        /* pt_br   */ "Palavra",
        /* ru      */ "Слово",
        /* zh_hans */ "全字匹配",
        /* zh_hant */ "全字匹配",
    ],
    // SearchUseRegex (toggle)
    [
        /* en      */ "Use Regex",
        /* de      */ "RegEx",
        /* es      */ "RegEx",
        /* fr      */ "RegEx",
        /* it      */ "RegEx",
        /* ja      */ "正規表現",
        /* ko      */ "정규식",
        /* pt_br   */ "RegEx",
        /* ru      */ "RegEx",
        /* zh_hans */ "正则",
        /* zh_hant */ "正則",
    ],
    // SearchReplaceAll (button)
    [
        /* en      */ "Replace All",
        /* de      */ "Alle ersetzen",
        /* es      */ "Reemplazar todo",
        /* fr      */ "Remplacer tout",
        /* it      */ "Sostituisci tutto",
        /* ja      */ "すべて置換",
        /* ko      */ "모두 바꾸기",
        /* pt_br   */ "Substituir tudo",
        /* ru      */ "Заменить все",
        /* zh_hans */ "全部替换",
        /* zh_hant */ "全部取代",
    ],
    // SearchClose (button)
    [
        /* en      */ "Close",
        /* de      */ "Schließen",
        /* es      */ "Cerrar",
        /* fr      */ "Fermer",
        /* it      */ "Chiudi",
        /* ja      */ "閉じる",
        /* ko      */ "닫기",
        /* pt_br   */ "Fechar",
        /* ru      */ "Закрыть",
        /* zh_hans */ "关闭",
        /* zh_hant */ "關閉",
    ],

    // EncodingReopen
    [
        /* en      */ "Reopen with encoding…",
        /* de      */ "Mit Kodierung erneut öffnen…",
        /* es      */ "Reabrir con codificación…",
        /* fr      */ "Rouvrir avec un encodage différent…",
        /* it      */ "Riapri con codifica…",
        /* ja      */ "指定エンコーディングで再度開く…",
        /* ko      */ "인코딩으로 다시 열기…",
        /* pt_br   */ "Reabrir com codificação…",
        /* ru      */ "Открыть снова с кодировкой…",
        /* zh_hans */ "使用编码重新打开…",
        /* zh_hant */ "使用編碼重新打開…",
    ],
    // EncodingConvert
    [
        /* en      */ "Convert to encoding…",
        /* de      */ "In Kodierung konvertieren…",
        /* es      */ "Convertir a otra codificación…",
        /* fr      */ "Convertir vers l’encodage…",
        /* it      */ "Converti in codifica…",
        /* ja      */ "エンコーディングを変換…",
        /* ko      */ "인코딩으로 변환…",
        /* pt_br   */ "Converter para codificação…",
        /* ru      */ "Преобразовать в кодировку…",
        /* zh_hans */ "转换为编码…",
        /* zh_hant */ "轉換為編碼…",
    ],

    // IndentationTabs
    [
        /* en      */ "Tabs",
        /* de      */ "Tabs",
        /* es      */ "Tabulaciones",
        /* fr      */ "Tabulations",
        /* it      */ "Tabulazioni",
        /* ja      */ "タブ",
        /* ko      */ "탭",
        /* pt_br   */ "Tabulações",
        /* ru      */ "Табы",
        /* zh_hans */ "制表符",
        /* zh_hant */ "製表符",
    ],
    // IndentationSpaces
    [
        /* en      */ "Spaces",
        /* de      */ "Leerzeichen",
        /* es      */ "Espacios",
        /* fr      */ "Espaces",
        /* it      */ "Spazi",
        /* ja      */ "スペース",
        /* ko      */ "공백",
        /* pt_br   */ "Espaços",
        /* ru      */ "Пробелы",
        /* zh_hans */ "空格",
        /* zh_hant */ "空格",
    ],

    // SaveAsDialogPathLabel
    [
        /* en      */ "Folder:",
        /* de      */ "Ordner:",
        /* es      */ "Carpeta:",
        /* fr      */ "Dossier :",
        /* it      */ "Cartella:",
        /* ja      */ "フォルダ:",
        /* ko      */ "폴더:",
        /* pt_br   */ "Pasta:",
        /* ru      */ "Папка:",
        /* zh_hans */ "文件夹:",
        /* zh_hant */ "資料夾:",
    ],
    // SaveAsDialogNameLabel
    [
        /* en      */ "File name:",
        /* de      */ "Dateiname:",
        /* es      */ "Nombre de archivo:",
        /* fr      */ "Nom du fichier :",
        /* it      */ "Nome del file:",
        /* ja      */ "ファイル名:",
        /* ko      */ "파일 이름:",
        /* pt_br   */ "Nome do arquivo:",
        /* ru      */ "Имя файла:",
        /* zh_hans */ "文件名:",
        /* zh_hant */ "檔案名稱:",
    ],

    // FileOverwriteWarning
    [
        /* en      */ "Confirm Save As",
        /* de      */ "Speichern unter bestätigen",
        /* es      */ "Confirmar Guardar como",
        /* fr      */ "Confirmer Enregistrer sous",
        /* it      */ "Conferma Salva con nome",
        /* ja      */ "名前を付けて保存の確認",
        /* ko      */ "다른 이름으로 저장 확인",
        /* pt_br   */ "Confirmar Salvar como",
        /* ru      */ "Подтвердите «Сохранить как…»",
        /* zh_hans */ "确认另存为",
        /* zh_hant */ "確認另存新檔",
    ],
    // FileOverwriteWarningDescription
    [
        /* en      */ "File already exists. Do you want to overwrite it?",
        /* de      */ "Datei existiert bereits. Möchten Sie sie überschreiben?",
        /* es      */ "El archivo ya existe. ¿Desea sobrescribirlo?",
        /* fr      */ "Le fichier existe déjà. Voulez-vous l’écraser ?",
        /* it      */ "Il file esiste già. Vuoi sovrascriverlo?",
        /* ja      */ "ファイルは既に存在します。上書きしますか？",
        /* ko      */ "파일이 이미 존재합니다. 덮어쓰시겠습니까?",
        /* pt_br   */ "O arquivo já existe. Deseja sobrescrevê-lo?",
        /* ru      */ "Файл уже существует. Перезаписать?",
        /* zh_hans */ "文件已存在。要覆盖它吗？",
        /* zh_hant */ "檔案已存在。要覆蓋它嗎？",
    ],
];

static mut S_LANG: LangId = LangId::en;

pub fn init() {
    // WARNING:
    // Generic language tags such as "zh" MUST be sorted after more specific tags such
    // as "zh-hant" to ensure that the prefix match finds the most specific one first.
    const LANG_MAP: &[(&str, LangId)] = &[
        ("en", LangId::en),
        // ----------------
        ("de", LangId::de),
        ("es", LangId::es),
        ("fr", LangId::fr),
        ("it", LangId::it),
        ("ja", LangId::ja),
        ("ko", LangId::ko),
        ("pt-br", LangId::pt_br),
        ("ru", LangId::ru),
        ("zh-hant", LangId::zh_hant),
        ("zh-tw", LangId::zh_hant),
        ("zh", LangId::zh_hans),
    ];

    let scratch = scratch_arena(None);
    let langs = sys::preferred_languages(&scratch);
    let mut lang = LangId::en;

    'outer: for l in langs {
        for (prefix, id) in LANG_MAP {
            if l.starts_with_ignore_ascii_case(prefix) {
                lang = *id;
                break 'outer;
            }
        }
    }

    unsafe {
        S_LANG = lang;
    }
}

pub fn loc(id: LocId) -> &'static str {
    S_LANG_LUT[id as usize][unsafe { S_LANG as usize }]
}
