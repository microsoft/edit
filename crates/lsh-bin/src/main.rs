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
        --panel-alt: #12141a;
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
    }

    .app {
        display: grid;
        grid-template-columns: minmax(0, 1fr) minmax(0, 1fr);
        height: 100vh;
    }

    .pane {
        min-width: 0;
        min-height: 0;
        display: grid;
        grid-template-rows: auto minmax(0, 1fr);
        background: var(--panel);
    }

    .pane + .pane {
        border-left: 1px solid var(--border);
        background: var(--panel-alt);
    }

    .toolbar {
        min-height: 48px;
        display: flex;
        align-items: center;
        gap: 12px;
        padding: 8px 10px;
        border-bottom: 1px solid var(--border);
        background: #151820;
    }

    .metrics {
        width: 100%;
        display: flex;
        align-items: center;
        justify-content: flex-end;
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

    select:focus,
    textarea:focus {
        outline: 2px solid var(--focus);
        outline-offset: -2px;
    }

    textarea,
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
        white-space: pre;
    }

    textarea {
        caret-color: var(--accent);
    }

    .output {
        user-select: text;
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
        .app {
            grid-template-columns: 1fr;
            grid-template-rows: minmax(0, 1fr) minmax(0, 1fr);
        }

        .pane + .pane {
            border-left: 0;
            border-top: 1px solid var(--border);
        }
    }
</style>
</head>
<body>
<main class="app">
    <section class="pane">
        <div class="toolbar">
            <select id="language" aria-label="Language"></select>
        </div>
        <textarea id="input" spellcheck="false" aria-label="Input"></textarea>
    </section>
    <section class="pane">
        <div class="toolbar">
            <div class="metrics" aria-live="polite">
                <span><span id="lines-per-second" class="metric-value">0</span> lines/s</span>
                <span><span id="megabytes-per-second" class="metric-value">0.00</span> MB/s</span>
            </div>
        </div>
        <pre id="output" class="output" aria-label="Highlighted output"></pre>
    </section>
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
    const input = document.querySelector("#input");
    const output = document.querySelector("#output");
    const linesPerSecond = document.querySelector("#lines-per-second");
    const megabytesPerSecond = document.querySelector("#megabytes-per-second");
    const textEncoder = new TextEncoder();
    const kindByValue = new Map(LSH.HIGHLIGHT_KINDS.map(kind => [kind.value, kind]));

    for (const language of LSH.LANGUAGES) {
        const option = document.createElement("option");
        option.value = language.id;
        option.textContent = language.name;
        languageSelect.append(option);
    }

    function encodeLine(line) {
        let ascii = true;
        for (let index = 0; index < line.length; index += 1) {
            if (line.charCodeAt(index) > 0x7f) {
                ascii = false;
                break;
            }
        }

        if (ascii) {
            const bytes = new Uint8Array(line.length);
            for (let index = 0; index < line.length; index += 1) {
                bytes[index] = line.charCodeAt(index);
            }
            return { bytes, offsets: null };
        }

        const bytes = [];
        const offsets = [0];

        for (let index = 0; index < line.length;) {
            const codePoint = line.codePointAt(index);
            const character = String.fromCodePoint(codePoint);
            const encoded = textEncoder.encode(character);
            const nextIndex = index + character.length;

            for (const byte of encoded) {
                bytes.push(byte);
                offsets.push(nextIndex);
            }

            index = nextIndex;
        }

        return { bytes: new Uint8Array(bytes), offsets };
    }

    function stringIndexForByteOffset(offsets, offset, fallback) {
        if (offsets === null) {
            return offset;
        }
        return offsets[Math.min(offset, offsets.length - 1)] ?? fallback;
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
        const { bytes, offsets } = encodeLine(line);
        const highlights = runtime.parseNextLine(bytes);

        for (let index = 0; index + 3 < highlights.length; index += 2) {
            const start = stringIndexForByteOffset(offsets, highlights[index], line.length);
            const end = stringIndexForByteOffset(offsets, highlights[index + 2], line.length);
            const text = line.slice(start, Math.max(start, end));
            const kind = kindByValue.get(highlights[index + 1]);

            if (!kind || kind.identifier === "other") {
                html.push(escapeHtml(text));
            } else {
                html.push('<span class="token" data-kind="', kind.identifier, '">', escapeHtml(text), '</span>');
            }
        }

        return bytes.length;
    }

    let renderFrame = 0;

    function render() {
        renderFrame = 0;
        const language = LSH.LANGUAGES[languageSelect.selectedIndex] ?? LSH.LANGUAGES[0];
        output.textContent = "";

        if (!language) {
            return;
        }

        const runtime = new LSH.Runtime(LSH.ASSEMBLY, LSH.STRINGS, LSH.CHARSETS, language.entrypoint);
    const html = [];
        const lines = input.value.split("\n");
        const measuredLineCount = input.value.length === 0 ? 0 : lines.length;
        let measuredByteCount = 0;
        const startTime = performance.now();

        for (let index = 0; index < lines.length; index += 1) {
            const line = lines[index].endsWith("\r") ? lines[index].slice(0, -1) : lines[index];
            measuredByteCount += appendHighlightedLine(html, runtime, line);
            if (index + 1 < lines.length) {
                measuredByteCount += 1;
                html.push("\n");
            }
        }

        output.innerHTML = html.join("");
        const elapsedSeconds = Math.max((performance.now() - startTime) / 1000, 0.000001);
        linesPerSecond.textContent = Math.round(measuredLineCount / elapsedSeconds).toLocaleString();
        megabytesPerSecond.textContent = (measuredByteCount / (1024 * 1024) / elapsedSeconds).toFixed(2);
    }

    function scheduleRender() {
        if (renderFrame === 0) {
            renderFrame = requestAnimationFrame(render);
        }
    }

    input.addEventListener("input", scheduleRender);
    languageSelect.addEventListener("change", scheduleRender);
    render();
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
