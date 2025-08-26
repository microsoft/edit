# Contributing

## Translation improvements

You can find our translations in [`i18n/edit.toml`](./i18n/edit.toml).
Please feel free to open a pull request with your changes at any time.
If you'd like to discuss your changes first, please feel free to open an issue.

## Bug reports

If you find any bugs, we gladly accept pull requests without prior discussion.
Otherwise, you can of course always open an issue for us to look into.

## Feature requests

Please open a new issue for any feature requests you have in mind.
Keeping the binary size of the editor small is a priority for us and so we may need to discuss any new features first until we have support for plugins.

## Code changes

The project has a focus on a small binary size and sufficient (good) performance.
As such, we generally do not accept pull requests that introduce dependencies (there are always exceptions of course).
Otherwise, you can consider this project a playground for trying out any cool ideas you have.

The overall architecture of the project can be summarized as follows:
* The underlying text buffer in `src/buffer` doesn't keep track of line breaks in the document.
  This is a crucial design aspect that permeates throughout the entire codebase.

  To oversimplify, the *only* state that is kept is the current cursor position.
  When the user asks to move to another line, the editor will `O(n)` seek through the underlying document until it found the corresponding number of line breaks.
  * As a result, `src/simd` contains crucial `memchr2` functions to quickly find the next or previous line break (runs at up to >100GB/s).
  * Furthermore, `src/unicode` implements an `Utf8Chars` iterator which transparently inserts U+FFFD replacements during iteration (runs at up to 4GB/s).
  * Furthermore, `src/unicode` also implements grapheme cluster segmentation and cluster width measurement via its `MeasurementConfig` (runs at up to 600MB/s).
  * If word wrap is disabled, `memchr2` is used for all navigation across lines, allowing us to breeze through 1GB large files as if they were 1MB.
  * Even if word-wrap is enabled, it's still sufficiently smooth thanks to `MeasurementConfig`. This is only possible because these base functions are heavily optimized.
* `src/framebuffer.rs` implements a "framebuffer" like in video games.
  It allows us to draw the UI output into an intermediate buffer first, accumulating all changes and handling things like color blending.
  Then, it can compare the accumulated output with the previous frame and only send the necessary changes to the terminal.
* `src/tui.rs` implements an immediate mode UI. Its module implementation gives an overview how it works and I recommend reading it.
* `src/vt.rs` implements our VT parser.
* `src/sys` contains our platform abstractions.
* Finally, `src/bin/edit` ties everything together.
  It's roughly 90% UI code and business logic.
  It contains a little bit of VT logic in `setup_terminal`.

If you have an issue with your terminal, the places of interest are the aforementioned:
* VT parser in `src/vt.rs`
* Platform specific code in `src/sys`
* And the `setup_terminal` function in `src/bin/edit/main.rs`
