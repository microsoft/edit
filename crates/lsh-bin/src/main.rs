// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use std::fs::File;
use std::io::{BufRead, BufReader, BufWriter, IsTerminal, Write as _, stdout};
use std::path::{Path, PathBuf};
use std::process::exit;

use anyhow::bail;
use argh::FromArgs;
use lsh::compiler::SerializedCharset;
use lsh::runtime::Runtime;
use stdext::arena::scratch_arena;
use stdext::glob::glob_match;

#[derive(FromArgs, PartialEq, Debug)]
#[argh(description = "Debug and test frontend for LSH")]
struct Command {
    #[argh(subcommand)]
    sub: SubCommands,
}

#[derive(FromArgs, PartialEq, Debug)]
#[argh(subcommand)]
enum SubCommands {
    Compile(SubCommandOneCompile),
    Assembly(SubCommandAssembly),
    Html(SubCommandHtml),
    Render(SubCommandRender),
}

#[derive(FromArgs, PartialEq, Debug)]
#[argh(subcommand, name = "compile", description = "Generate Rust code from .lsh files")]
struct SubCommandOneCompile {
    #[argh(positional, description = "source .lsh files or directories")]
    lsh: Vec<PathBuf>,
}

#[derive(FromArgs, PartialEq, Debug)]
#[argh(subcommand, name = "assembly", description = "Generate assembly from .lsh files")]
struct SubCommandAssembly {
    #[argh(positional, description = "source .lsh files or directories")]
    lsh: Vec<PathBuf>,
}

#[derive(FromArgs, PartialEq, Debug)]
#[argh(subcommand, name = "html", description = "Generate a self-contained HTML highlighter")]
struct SubCommandHtml {
    #[argh(positional, description = "source .lsh files or directories")]
    lsh: Vec<PathBuf>,
}

#[derive(FromArgs, PartialEq, Debug)]
#[argh(subcommand, name = "render", description = "Highlight text files")]
struct SubCommandRender {
    #[argh(option, description = "source text file")]
    input: PathBuf,
    #[argh(positional, description = "source .lsh files or directories")]
    lsh: Vec<PathBuf>,
}

pub fn main() {
    if let Err(e) = run() {
        eprintln!("{e}");
        exit(1);
    }
}

fn run() -> anyhow::Result<()> {
    stdext::arena::init(128 * 1024 * 1024).unwrap();

    let command: Command = argh::from_env();
    let scratch = scratch_arena(None);
    let mut generator = lsh::compiler::Generator::new(&scratch);
    let mut read_lsh = |path: &Path| {
        if path.is_dir() { generator.read_directory(path) } else { generator.read_file(path) }
    };
    let mut read_lsh_inputs = |paths: &[PathBuf]| -> anyhow::Result<()> {
        if paths.is_empty() {
            bail!("At least one .lsh file or directory is required");
        }

        for path in paths {
            read_lsh(path)?;
        }

        Ok(())
    };

    match &command.sub {
        SubCommands::Compile(cmd) => {
            read_lsh_inputs(&cmd.lsh)?;
            let output = generator.generate_rust()?;
            _ = stdout().write_all(output.as_bytes());
        }
        SubCommands::Assembly(cmd) => {
            read_lsh_inputs(&cmd.lsh)?;
            let vt = stdout().is_terminal();
            let output = generator.generate_assembly(vt)?;
            _ = stdout().write_all(output.as_bytes());
        }
        SubCommands::Html(cmd) => {
            read_lsh_inputs(&cmd.lsh)?;
            let js = generator.generate_js()?;
            let output = generate_html(&js);
            _ = stdout().write_all(output.as_bytes());
        }
        SubCommands::Render(cmd) => {
            read_lsh_inputs(&cmd.lsh)?;
            run_render(generator, &cmd.input)?;
        }
    }

    Ok(())
}

fn generate_html(js: &str) -> String {
    let mut output = String::new();

    output.push_str(
        r##"<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>LSH Highlighter</title>
<style>
    :root {
        color-scheme: dark;
        --bg: #101114;
        --panel: #17191f;
        --border: #30343d;
        --text: #e7eaf0;
        --muted: #9aa3b2;
        --accent: #7aa2f7;
        --focus: #9ece6a;
    }

    * {
        box-sizing: border-box;
    }

    html,
    body {
        height: 100%;
        margin: 0;
        background: var(--bg);
        color: var(--text);
        font-family: ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif;
    }

    body {
        overflow: hidden;
        display: grid;
        grid-template-rows: auto minmax(0, 1fr);
    }

    .topbar {
        min-height: 48px;
        display: grid;
        grid-template-columns: minmax(0, 1fr) auto minmax(0, 1fr);
        align-items: center;
        gap: 12px;
        padding: 8px 10px;
        border-bottom: 1px solid var(--border);
        background: #151820;
        min-width: 0;
    }

    .top-left,
    .top-center,
    .top-right {
        min-width: 0;
        display: flex;
        align-items: center;
    }

    .top-center {
        justify-content: center;
    }

    .top-right {
        justify-content: flex-end;
    }

    .metrics {
        display: flex;
        align-items: center;
        gap: 14px;
        color: var(--muted);
        font: 12px/1.4 ui-monospace, SFMono-Regular, Consolas, "Liberation Mono", Menlo, monospace;
        white-space: nowrap;
    }

    .metric-value {
        color: var(--text);
        font-variant-numeric: tabular-nums;
    }

    select {
        min-width: 180px;
        max-width: 100%;
        height: 32px;
        border: 1px solid var(--border);
        border-radius: 6px;
        padding: 0 32px 0 10px;
        background: #20242d;
        color: var(--text);
        font: inherit;
    }

    button {
        height: 32px;
        border: 1px solid var(--border);
        border-radius: 6px;
        padding: 0 12px;
        background: #20242d;
        color: var(--text);
        font: inherit;
    }

    button:disabled {
        color: #626b79;
    }

    button:not(:disabled):hover {
        border-color: var(--accent);
    }

    select:focus,
    button:focus,
    .drop-zone:focus {
        outline: 2px solid var(--focus);
        outline-offset: -2px;
    }

    .viewer {
        position: relative;
        min-width: 0;
        min-height: 0;
        background: var(--panel);
    }

    .output {
        width: 100%;
        height: 100%;
        min-width: 0;
        min-height: 0;
        margin: 0;
        border: 0;
        padding: 16px;
        overflow: auto;
        resize: none;
        background: transparent;
        color: var(--text);
        font: 14px/1.5 ui-monospace, SFMono-Regular, Consolas, "Liberation Mono", Menlo, monospace;
        letter-spacing: 0;
        tab-size: 4;
    }

    .output {
        user-select: text;
    }

    .output-chunk {
        margin: 0;
        font: inherit;
        letter-spacing: 0;
        tab-size: inherit;
        white-space: pre;
    }

    .viewer:not(.has-content) .output {
        display: none;
    }

    .drop-zone {
        position: absolute;
        left: 50%;
        top: 50%;
        width: min(420px, calc(100vw - 32px));
        min-height: 160px;
        transform: translate(-50%, -50%);
        display: flex;
        flex-direction: column;
        align-items: center;
        justify-content: center;
        gap: 8px;
        border: 1px dashed #4b5362;
        border-radius: 8px;
        background: #151820;
        color: var(--muted);
        text-align: center;
        padding: 24px;
    }

    .drop-title {
        color: var(--text);
        font-size: 16px;
        font-weight: 600;
    }

    .drop-subtitle {
        font-size: 13px;
        line-height: 1.4;
    }

    .viewer.has-content .drop-zone {
        display: none;
    }

    .viewer.dragging .drop-zone {
        border-color: var(--accent);
        background: #1b2030;
    }

    .token[data-kind="comment"] { color: #8fbc8f; }
    .token[data-kind="method"] { color: #ffd166; }
    .token[data-kind="string"] { color: #f28b82; }
    .token[data-kind="variable"] { color: #7dd3fc; }
    .token[data-kind="constant.language"] { color: #82aaff; }
    .token[data-kind="constant.numeric"] { color: #b5cea8; }
    .token[data-kind="keyword.control"] { color: #c792ea; }
    .token[data-kind="keyword.other"] { color: #82aaff; }
    .token[data-kind="markup.bold"] { font-weight: 700; }
    .token[data-kind="markup.changed"] { color: #82aaff; }
    .token[data-kind="markup.deleted"] { color: #f28b82; }
    .token[data-kind="markup.heading"] { color: #82aaff; font-weight: 700; }
    .token[data-kind="markup.inserted"] { color: #9ece6a; }
    .token[data-kind="markup.italic"] { font-style: italic; }
    .token[data-kind="markup.link"] { color: #89ddff; text-decoration: underline; }
    .token[data-kind="markup.list"] { color: #82aaff; }
    .token[data-kind="markup.strikethrough"] { text-decoration: line-through; }
    .token[data-kind="meta.header"] { color: #82aaff; }

    @media (max-width: 760px) {
        .topbar {
            grid-template-columns: 1fr;
            justify-items: stretch;
        }

        .top-left,
        .top-center,
        .top-right,
        .metrics {
            justify-content: center;
        }

        .metrics {
            flex-wrap: wrap;
        }
    }
</style>
</head>
<body>
<header class="topbar">
    <div class="top-left">
        <select id="language" aria-label="Language"></select>
    </div>
    <div class="top-center">
        <button id="reload" type="button" disabled>Reload</button>
    </div>
    <div class="top-right">
        <div class="metrics" aria-live="polite">
            <span><span id="lines-per-second" class="metric-value">0</span> lines/s</span>
            <span><span id="megabytes-per-second" class="metric-value">0.00</span> MB/s</span>
            <span><span id="total-duration" class="metric-value">0.00</span> ms total</span>
        </div>
    </div>
</header>
<main id="viewer" class="viewer">
    <div id="drop-zone" class="drop-zone" tabindex="0" role="button" aria-label="Drop file">
        <div class="drop-title">Drop a file</div>
        <div class="drop-subtitle">The highlighted output will appear here.</div>
    </div>
    <div id="output" class="output" aria-label="Highlighted output"></div>
</main>
<script>
"##,
    );

    output.push_str(js);

    output.push_str(
        r##"</script>
<script>
(() => {
    "use strict";

    const languageSelect = document.querySelector("#language");
    const reloadButton = document.querySelector("#reload");
    const viewer = document.querySelector("#viewer");
    const dropZone = document.querySelector("#drop-zone");
    const output = document.querySelector("#output");
    const linesPerSecond = document.querySelector("#lines-per-second");
    const megabytesPerSecond = document.querySelector("#megabytes-per-second");
    const totalDuration = document.querySelector("#total-duration");
    const kindByValue = new Map(LSH.HIGHLIGHT_KINDS.map(kind => [kind.value, kind]));
    const languageById = new Map(LSH.LANGUAGES.map(language => [language.id, language]));
    const plainTextLanguageId = "plain-text";
    const chunkLineCount = 100;
    let currentText = "";
    let currentFileName = "";
    let currentChunks = [];
    let currentLineCount = 0;
    let currentRuntime = null;

    {
        const option = document.createElement("option");
        option.value = plainTextLanguageId;
        option.textContent = "Plain Text";
        languageSelect.append(option);
    }

    for (const language of LSH.LANGUAGES) {
        const option = document.createElement("option");
        option.value = language.id;
        option.textContent = language.name;
        languageSelect.append(option);
    }

    function globToRegExp(pattern) {
        let source = "^";
        for (const character of pattern) {
            switch (character) {
                case "*":
                    source += ".*";
                    break;
                case "?":
                    source += ".";
                    break;
                case ".":
                case "+":
                case "(":
                case ")":
                case "^":
                case "$":
                case "{":
                case "}":
                case "|":
                case "[":
                case "]":
                case "\\":
                    source += "\\" + character;
                    break;
                default:
                    source += character;
                    break;
            }
        }
        return new RegExp(source + "$", "i");
    }

    function selectLanguageForFileName(fileName) {
        languageSelect.value = plainTextLanguageId;
        const candidates = [fileName, "/" + fileName];
        for (let index = 0; index < LSH.FILE_ASSOCIATIONS.length; index += 1) {
            const [pattern, language] = LSH.FILE_ASSOCIATIONS[index];
            const matcher = globToRegExp(pattern);
            if (candidates.some(candidate => matcher.test(candidate))) {
                languageSelect.value = language.id;
                return;
            }
        }
    }

    function escapeHtml(text) {
        return text.replace(/[&<>"]/g, character => {
            switch (character) {
                case "&":
                    return "&amp;";
                case "<":
                    return "&lt;";
                case ">":
                    return "&gt;";
                default:
                    return "&quot;";
            }
        });
    }

    function appendHighlightedLine(html, runtime, line) {
        const highlights = runtime.parseNextLine(line);

        for (let index = 0; index + 3 < highlights.length; index += 2) {
            const start = Math.min(highlights[index], line.length);
            const end = Math.min(highlights[index + 2], line.length);
            const text = line.slice(start, Math.max(start, end));
            const kind = kindByValue.get(highlights[index + 1]);

            if (!kind || kind.identifier === "other") {
                html.push(escapeHtml(text));
            } else {
                html.push('<span class="token" data-kind="', kind.identifier, '">', escapeHtml(text), '</span>');
            }
        }

        return line.length;
    }

    function createOutputChunk(lines, lineStart) {
        const chunk = document.createElement("pre");
        chunk.className = "output-chunk";
        chunk.lshLines = lines;
        chunk.lshLineStart = lineStart;
        chunk.lshSnapshot = null;
        chunk.lshHighlighted = false;
        return chunk;
    }

    function appendTextChunks(fragment, lines) {
        currentChunks = [];
        currentLineCount = lines.length;
        for (let start = 0; start < lines.length; start += chunkLineCount) {
            const end = Math.min(start + chunkLineCount, lines.length);
            const chunkLines = lines.slice(start, end);
            const chunk = createOutputChunk(chunkLines, start);
            chunk.textContent = chunkLines.join("\n");
            currentChunks.push(chunk);
            fragment.append(chunk);
            if (end < lines.length) {
                fragment.append("\n");
            }
        }
    }

    function hasLineSeparatorAfter(chunk, index) {
        return chunk.lshLineStart + index + 1 < currentLineCount;
    }

    function lineForHighlighting(line) {
        return line.endsWith("\r") ? line.slice(0, -1) : line;
    }

    function addMeasurement(total, measurement) {
        total.lineCount += measurement.lineCount;
        total.byteCount += measurement.byteCount;
        total.changed ||= measurement.changed;
    }

    function parseChunkForSnapshot(runtime, chunk) {
        let measuredLineCount = 0;
        let measuredByteCount = 0;

        for (let index = 0; index < chunk.lshLines.length; index += 1) {
            const line = lineForHighlighting(chunk.lshLines[index]);
            runtime.parseNextLine(line);
            measuredLineCount += 1;
            measuredByteCount += line.length;
            if (hasLineSeparatorAfter(chunk, index)) {
                measuredByteCount += 1;
            }
        }

        return { lineCount: measuredLineCount, byteCount: measuredByteCount, changed: true };
    }

    function ensureChunkSnapshot(index) {
        const measurement = { lineCount: 0, byteCount: 0, changed: false };
        if (!currentRuntime || index < 0 || index >= currentChunks.length || currentChunks[index].lshSnapshot) {
            return measurement;
        }

        let cursor = index - 1;
        while (cursor > 0 && !currentChunks[cursor].lshSnapshot) {
            cursor -= 1;
        }
        if (!currentChunks[cursor].lshSnapshot) {
            return measurement;
        }

        currentRuntime.restore(currentChunks[cursor].lshSnapshot);
        for (; cursor < index; cursor += 1) {
            addMeasurement(measurement, parseChunkForSnapshot(currentRuntime, currentChunks[cursor]));
            const next = currentChunks[cursor + 1];
            if (next && !next.lshSnapshot) {
                next.lshSnapshot = currentRuntime.snapshot();
            }
        }

        return measurement;
    }

    function highlightChunk(index) {
        const measurement = ensureChunkSnapshot(index);
        const chunk = currentChunks[index];
        if (!currentRuntime || !chunk || chunk.lshHighlighted || !chunk.lshSnapshot) {
            return measurement;
        }

        currentRuntime.restore(chunk.lshSnapshot);

        const html = [];
        for (let lineIndex = 0; lineIndex < chunk.lshLines.length; lineIndex += 1) {
            const line = lineForHighlighting(chunk.lshLines[lineIndex]);
            measurement.lineCount += 1;
            measurement.byteCount += appendHighlightedLine(html, currentRuntime, line);
            if (lineIndex + 1 < chunk.lshLines.length) {
                measurement.byteCount += 1;
                html.push("\n");
            } else if (hasLineSeparatorAfter(chunk, lineIndex)) {
                measurement.byteCount += 1;
            }
        }

        chunk.innerHTML = html.join("");
        chunk.lshHighlighted = true;
        measurement.changed = true;

        const next = currentChunks[index + 1];
        if (next && !next.lshSnapshot) {
            next.lshSnapshot = currentRuntime.snapshot();
        }

        return measurement;
    }

    let renderFrame = 0;
    let highlightFrame = 0;

    function resetMetrics() {
        linesPerSecond.textContent = "0";
        megabytesPerSecond.textContent = "0.00";
        totalDuration.textContent = "0.00";
    }

    function forceLayout() {
        return output.scrollHeight;
    }

    function updateMetrics(measuredLineCount, measuredByteCount, startTime) {
        forceLayout();
        const elapsedSeconds = Math.max((performance.now() - startTime) / 1000, 0.000001);
        linesPerSecond.textContent = Math.round(measuredLineCount / elapsedSeconds).toLocaleString();
        megabytesPerSecond.textContent = (measuredByteCount / (1024 * 1024) / elapsedSeconds).toFixed(2);
        totalDuration.textContent = (elapsedSeconds * 1000).toFixed(2);
    }

    function visibleChunkIndexes() {
        const outputRect = output.getBoundingClientRect();
        const indexes = [];

        for (let index = 0; index < currentChunks.length; index += 1) {
            const chunkRect = currentChunks[index].getBoundingClientRect();
            if (chunkRect.bottom >= outputRect.top && chunkRect.top <= outputRect.bottom) {
                indexes.push(index);
            }
        }

        return indexes;
    }

    function highlightVisibleChunks(startTime = performance.now(), measuredLineCount = 0, measuredByteCount = 0) {
        if (!currentRuntime || currentChunks.length === 0) {
            return;
        }

        const measurement = { lineCount: measuredLineCount, byteCount: measuredByteCount, changed: measuredLineCount > 0 || measuredByteCount > 0 };
        for (const index of visibleChunkIndexes()) {
            if (!currentChunks[index].lshHighlighted) {
                addMeasurement(measurement, highlightChunk(index));
            }
        }

        if (measurement.changed) {
            updateMetrics(measurement.lineCount, measurement.byteCount, startTime);
        }
    }

    function scheduleVisibleChunkHighlight() {
        if (highlightFrame === 0) {
            highlightFrame = requestAnimationFrame(() => {
                highlightFrame = 0;
                highlightVisibleChunks();
            });
        }
    }

    function showEmpty() {
        cancelRender();
        currentChunks = [];
        currentLineCount = 0;
        currentRuntime = null;
        output.textContent = "";
        viewer.classList.remove("has-content");
        resetMetrics();
    }

    function showPlainText() {
        cancelRender();
        if (currentText.length === 0) {
            showEmpty();
            return;
        }

        const startTime = performance.now();
        const lines = currentText.split("\n");
        const fragment = document.createDocumentFragment();
        const measuredLineCount = lines.length;
        const measuredByteCount = currentText.length;
        currentRuntime = null;
        appendTextChunks(fragment, lines);
        output.replaceChildren(fragment);
        viewer.classList.add("has-content");
        updateMetrics(measuredLineCount, measuredByteCount, startTime);
    }

    function selectedLanguage() {
        return languageById.get(languageSelect.value) ?? null;
    }

    function updateReloadButton() {
        reloadButton.disabled = currentText.length === 0;
    }

    function showCurrentText() {
        updateReloadButton();
        if (currentText.length === 0) {
            showEmpty();
        } else if (selectedLanguage()) {
            scheduleRender();
        } else {
            showPlainText();
        }
    }

    function render() {
        renderFrame = 0;
        const language = selectedLanguage();
        if (!language) {
            showPlainText();
            return;
        }

        output.textContent = "";

        if (currentText.length === 0) {
            showEmpty();
            return;
        }

        const lines = currentText.split("\n");
        const fragment = document.createDocumentFragment();
        const measuredLineCount = lines.length;
        const startTime = performance.now();
        const measuredByteCount = currentText.length;
        currentRuntime = new LSH.Runtime(LSH.ASSEMBLY, LSH.STRINGS, LSH.CHARSETS, language.entrypoint);
        appendTextChunks(fragment, lines);
        if (currentChunks.length > 0) {
            currentChunks[0].lshSnapshot = currentRuntime.snapshot();
        }
        output.replaceChildren(fragment);
        viewer.classList.add("has-content");
        highlightVisibleChunks(startTime, measuredLineCount, measuredByteCount);
    }

    function cancelRender() {
        if (renderFrame !== 0) {
            cancelAnimationFrame(renderFrame);
            renderFrame = 0;
        }
        if (highlightFrame !== 0) {
            cancelAnimationFrame(highlightFrame);
            highlightFrame = 0;
        }
    }

    function scheduleRender() {
        if (highlightFrame !== 0) {
            cancelAnimationFrame(highlightFrame);
            highlightFrame = 0;
        }
        if (renderFrame === 0) {
            renderFrame = requestAnimationFrame(render);
        }
    }

    async function loadFile(file) {
        if (!file) {
            return;
        }

        currentFileName = file.name;
        currentText = await file.text();
        selectLanguageForFileName(currentFileName);
        showCurrentText();
    }

    function handleDrag(event) {
        event.preventDefault();
        event.stopPropagation();
    }

    viewer.addEventListener("dragenter", event => {
        handleDrag(event);
        viewer.classList.add("dragging");
    });
    viewer.addEventListener("dragover", handleDrag);
    viewer.addEventListener("dragleave", event => {
        handleDrag(event);
        if (!viewer.contains(event.relatedTarget)) {
            viewer.classList.remove("dragging");
        }
    });
    viewer.addEventListener("drop", event => {
        handleDrag(event);
        viewer.classList.remove("dragging");
        loadFile(event.dataTransfer.files[0]);
    });

    dropZone.addEventListener("keydown", event => {
        if (event.key === "Enter" || event.key === " ") {
            event.preventDefault();
        }
    });
    output.addEventListener("scroll", scheduleVisibleChunkHighlight, { passive: true });
    reloadButton.addEventListener("click", showCurrentText);
    languageSelect.addEventListener("change", showCurrentText);
    showCurrentText();
})();
</script>
</body>
</html>
"##,
    );

    output
}

fn run_render(generator: lsh::compiler::Generator, path: &Path) -> anyhow::Result<()> {
    let assembly = generator.assemble()?;

    let Some(entrypoint) = assembly.entrypoints.iter().find(|ep| {
        ep.paths
            .iter()
            .any(|pattern| glob_match(pattern.as_bytes(), path.as_os_str().as_encoded_bytes()))
    }) else {
        bail!("No matching highlighting definition found");
    };

    let mut color_map = Vec::new();
    let mut unknown_kinds = Vec::new();
    for hk in &assembly.highlight_kinds {
        let color = match hk.identifier {
            "other" => "",

            "comment" => "\x1b[32m",  // Green
            "method" => "\x1b[93m",   // Bright Yellow
            "string" => "\x1b[91m",   // Bright Red
            "variable" => "\x1b[96m", // Bright Cyan

            "constant.language" => "\x1b[94m",   // Bright Blue
            "constant.numeric" => "\x1b[92m",    // Bright Green
            "keyword.control" => "\x1b[95m",     // Bright Magenta
            "keyword.other" => "\x1b[94m",       // Bright Blue
            "markup.bold" => "\x1b[1m",          // Bold
            "markup.changed" => "\x1b[94m",      // Bright Blue
            "markup.deleted" => "\x1b[91m",      // Bright Red
            "markup.heading" => "\x1b[94m",      // Bright Blue
            "markup.inserted" => "\x1b[92m",     // Bright Green
            "markup.italic" => "\x1b[3m",        // Italic
            "markup.link" => "\x1b[4m",          // Underlined
            "markup.list" => "\x1b[94m",         // Bright Blue
            "markup.strikethrough" => "\x1b[9m", // Strikethrough
            "meta.header" => "\x1b[94m",         // Bright Blue

            _ => {
                unknown_kinds.push(hk.identifier.to_string());
                ""
            }
        };

        if !color.is_empty() {
            if color_map.len() <= hk.value as usize {
                color_map.resize(hk.value as usize + 1, "");
            }
            color_map[hk.value as usize] = color;
        }
    }
    if !unknown_kinds.is_empty() {
        eprintln!("\x1b[33mWarning: Unknown highlight kinds:");
        for kind in &unknown_kinds {
            eprintln!("  - {}", kind);
        }
        eprintln!("\x1b[m");
    }

    // Convert Assembly data to static references by leaking memory
    // This is fine for a CLI tool that runs once and exits
    let charsets: Vec<SerializedCharset> =
        assembly.charsets.into_iter().map(|cs| cs.serialize()).collect();

    let mut runtime = Runtime::new(
        &assembly.instructions,
        &assembly.strings,
        &charsets,
        entrypoint.address as u32,
    );

    let reader = BufReader::with_capacity(128 * 1024, File::open(path)?);
    let mut stdout = BufWriter::with_capacity(128 * 1024, stdout());

    for line in reader.lines() {
        let line = line?;
        let scratch = scratch_arena(None);
        let highlights = runtime.parse_next_line::<u32>(&scratch, line.as_bytes());

        for w in highlights.windows(2) {
            let curr = &w[0];
            let next = &w[1];
            let start = curr.start;
            let end = next.start;
            let kind = curr.kind;
            let text = &line[start..end];

            if let Some(color) = color_map.get(kind as usize) {
                write!(stdout, "{color}{text}\x1b[m")?;
            } else {
                stdout.write_all(text.as_bytes())?;
            }
        }
        writeln!(stdout)?;
    }

    Ok(())
}
