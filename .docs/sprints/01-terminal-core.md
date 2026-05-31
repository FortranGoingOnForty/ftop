# Sprint 01: Terminal Core

## Goal
A running main loop that enters raw mode, clears the screen, draws a bordered box with "ftop" centered, handles keyboard input (q to quit), responds to terminal resize, and exits cleanly restoring terminal state. Mouse click events decoded and logged. This is the skeleton everything else hangs on.

## Status: NOT STARTED

## Targets

- [ ] Terminal initialization sequence
  - [ ] Enter raw mode via fgof-termios
  - [ ] Query terminal size
  - [ ] Enable alternate screen buffer (`\e[?1049h`)
  - [ ] Hide cursor (`\e[?25l`)
  - [ ] Enable mouse tracking (`\e[?1000h` / `\e[?1006h` SGR mouse mode)
- [ ] Main event loop skeleton
  - [ ] Non-blocking input read with timeout (select/poll via C shim or fgof-termios)
  - [ ] Timer-driven refresh (configurable interval, default 1000ms)
  - [ ] SIGWINCH handler for terminal resize
  - [ ] SIGINT/SIGTERM handler for clean exit
  - [ ] Clean shutdown sequence (restore terminal, show cursor, exit alternate screen)
- [ ] Screen buffer integration (fgof-screen)
  - [ ] Initialize screen buffer to terminal dimensions
  - [ ] Resize buffer on SIGWINCH
  - [ ] Write cells to buffer
  - [ ] Flush buffer to terminal (full-frame initially, diff-based later)
- [ ] Key input integration (fgof-keys)
  - [ ] Decode key events from raw bytes
  - [ ] Handle basic keys: q (quit), arrow keys, Enter, Escape
  - [ ] Handle Ctrl+C as clean exit
- [ ] Mouse input
  - [ ] Parse SGR mouse events from terminal
  - [ ] Decode button, position, modifiers
  - [ ] Log mouse events (for debugging; wiring to widgets comes later)
- [ ] Basic ANSI rendering primitives
  - [ ] Box drawing (single-line and double-line Unicode borders)
  - [ ] Foreground/background color setting (truecolor: `\e[38;2;r;g;bm`)
  - [ ] Text positioning (`\e[row;colH`)
  - [ ] Style attributes (bold, dim, underline, italic, reverse, strikethrough)
  - [ ] Style reset (`\e[0m`)
- [ ] Signal handling via C shim
  - [ ] SIGWINCH -> set resize flag (checked in main loop)
  - [ ] SIGINT/SIGTERM -> set exit flag
  - [ ] SIGTSTP (Ctrl+Z) -> suspend properly, restore terminal on resume (SIGCONT)

## Definition of Done
- Launch ftop: screen clears, bordered box appears with "ftop" text
- Press `q`: terminal restored, program exits cleanly
- Resize terminal: box redraws to new dimensions
- Ctrl+Z: suspends to shell; `fg` resumes with correct display
- Mouse clicks: position logged to debug output
- No terminal corruption on any exit path (clean, signal, crash)

## Dependencies
- Sprint 00 (build system, fgof deps)

## Testing
- **Snapshot test**: render a 80x24 box to fgof-screen buffer, compare against golden file
- **Unit tests**: ANSI escape sequence generation, color code formatting, box dimension math
- **Manual test**: launch in terminal, verify all key/mouse/resize behaviors
- **Crash recovery test**: kill -9 the process, verify terminal state is recoverable (`reset` command works)

## Pitfalls
- **Terminal state leaks**: if the program crashes or is killed, the terminal may be left in raw mode with hidden cursor. Must handle signals and install an atexit handler via C shim.
- **SIGWINCH race**: resize signal can arrive during a draw. Don't redraw inside the signal handler — just set a flag and handle it in the main loop.
- **Mouse event parsing**: SGR mouse mode (`\e[<btn;col;row[Mm]`) is the modern standard but older terminals may not support it. Fall back to X10 mode or disable mouse if unsupported.
- **UTF-8 width**: Unicode box-drawing characters are single-width but braille characters may be reported as double-width by some terminals. Test with `wcwidth()` via C shim.
- **Alternate screen**: some terminals (screen, tmux with certain configs) may not support alternate screen buffer. Degrade gracefully.
- **select() vs poll()**: macOS doesn't support epoll. Use poll() or select() for the input timeout. C shim should abstract this.

## Deferred
(none yet)

## Notes
- The fgof-screen library already provides diff-based redraw. Leverage this early — don't write our own double-buffering.
- fgof-keys already handles CSI/SS3 decoding. Extend it if needed for mouse events rather than writing a separate parser.
- SIGTSTP/SIGCONT handling is critical for a well-behaved TUI. btop handles this; htop handles this. We must too.
