# Sprint 11: Theming + Configuration

## Goal
Loadable theme system with truecolor gradients, user-configurable keybindings, complete settings persistence, and config hot-reload. ftop should look as polished and customizable as btop.

## Status: NOT STARTED

## Targets

### Theme System
- [ ] `src/ftop_theme.f90`
  - [ ] `type :: theme` — complete color scheme for all UI elements
    - Box borders (normal, focused, active)
    - Title text, label text, value text
    - Bar meter fill, empty, and overflow colors
    - Graph line colors, area fill colors, axis colors
    - Process table: header, row, selected, tree branch
    - CPU gradient stops (idle -> max)
    - Memory gradient stops
    - Network RX/TX colors
    - Disk read/write colors
    - GPU utilization gradient
    - Alert colors (warning, critical, info)
    - Status bar background, foreground
  - [ ] Theme loading from TOML file via fgof-toml
  - [ ] Theme validation: all required colors present, valid hex codes
  - [ ] Runtime theme switching (keybinding to cycle themes)

### Default Themes
- [ ] `themes/default.toml` — balanced dark theme
- [ ] `themes/dark.toml` — deep dark background, high contrast
- [ ] `themes/light.toml` — light background
- [ ] `themes/gruvbox.toml` — gruvbox color palette
- [ ] `themes/nord.toml` — nord color palette
- [ ] `themes/solarized-dark.toml` — solarized dark
- [ ] `themes/monokai.toml` — monokai colors
- [ ] User custom themes: load from `$XDG_CONFIG_HOME/ftop/themes/`

### Color Fallback
- [ ] Truecolor detection: check `$COLORTERM` = `truecolor`/`24bit`, `$TERM` capabilities
- [ ] 256-color fallback: map RGB to nearest xterm-256 color
- [ ] 16-color fallback: map to basic ANSI colors
- [ ] Monochrome mode: bold/dim/underline/reverse only (for accessibility)

### Configuration System
- [ ] `src/ftop_config.f90`
  - [ ] Complete config structure covering all settings
  - [ ] Load from `$XDG_CONFIG_HOME/ftop/config.toml`
  - [ ] Merge: defaults -> system config -> user config -> CLI args
  - [ ] Config keys:
    - [ ] `general.update_interval` — refresh rate in ms (default: 1000)
    - [ ] `general.theme` — theme name or path
    - [ ] `general.mouse` — enable/disable mouse (default: true)
    - [ ] `general.show_gpu` — auto/true/false
    - [ ] `general.temp_unit` — celsius/fahrenheit
    - [ ] `general.byte_unit` — binary (KiB/MiB) or decimal (KB/MB)
    - [ ] `layout` — widget grid definition (from Sprint 06)
    - [ ] `process.tree_view` — default to tree or flat
    - [ ] `process.sort_by` — default sort column
    - [ ] `process.columns` — visible columns list
    - [ ] `network.show_loopback` — show lo interface
    - [ ] `network.hide_interfaces` — pattern list to hide
    - [ ] `disk.show_tmpfs` — show tmpfs mounts
    - [ ] `disk.hide_mounts` — pattern list to hide
    - [ ] `gpu.nvidia` — enable/disable
    - [ ] `gpu.amd` — enable/disable
    - [ ] `gpu.intel` — enable/disable

### Keybinding Configuration
- [ ] Default keybindings (hardcoded, overridable via config)
  - [ ] `q` / `Ctrl+C` — quit
  - [ ] `Tab` / `Shift+Tab` — cycle widget focus
  - [ ] `z` / `Enter` — zoom/expand focused widget
  - [ ] `↑↓←→` — navigate within focused widget
  - [ ] `Space` — select / toggle tree expand
  - [ ] `k` — kill (send signal)
  - [ ] `/` — search/filter
  - [ ] `t` — toggle tree view
  - [ ] `s` — cycle sort column
  - [ ] `r` — reverse sort direction
  - [ ] `h` / `?` / `F1` — help overlay
  - [ ] `Escape` — close overlay / clear filter / exit zoom
  - [ ] `1-9` — jump to widget N (or switch layout preset)
  - [ ] `F5` — toggle snapshot/diff mode
  - [ ] `T` — cycle themes
- [ ] Config-overridable:
  ```toml
  [keys]
  quit = "q"
  kill = "k"
  filter = "/"
  tree_toggle = "t"
  ```

### Config Hot-Reload
- [ ] Watch config file via fgof-watch
- [ ] On change: re-parse, validate, apply non-destructive changes (theme, colors, layout adjustments)
- [ ] Destructive changes (keybindings) require restart notification

### Help Overlay
- [ ] Full-screen overlay showing all keybindings
- [ ] Grouped by context (global, process table, navigation, etc.)
- [ ] Shows configured (possibly remapped) bindings, not defaults
- [ ] Press `h`, `?`, or `F1` to toggle

### CLI Arguments
- [ ] `--version` — version string
- [ ] `--help` — usage information
- [ ] `--config <path>` — custom config file path
- [ ] `--theme <name>` — override theme
- [ ] `--update <ms>` — override refresh interval
- [ ] `--no-mouse` — disable mouse
- [ ] `--no-gpu` — disable GPU monitoring
- [ ] `--preset <name>` — use layout preset (default, compact, process-focused, network-focused)

## Definition of Done
- Themes load from TOML files and apply to all UI elements
- All shipped themes render correctly (visual inspection on truecolor terminal)
- 256-color and 16-color fallbacks produce usable output
- Config file loads and applies all settings
- Keybindings are functional and configurable
- Help overlay shows all current keybindings
- Config hot-reload updates theme without restart
- CLI arguments override config file settings
- Config file location follows XDG spec

## Dependencies
- Sprint 05 (fgof-toml for parsing)
- Sprint 06 (layout engine for config-driven layout)
- Sprint 04 (widgets to theme)
- All monitoring sprints (for complete config coverage)

## Testing
- **Unit**: theme TOML parsing (synthetic theme file -> correct colors)
- **Unit**: RGB to 256-color mapping (known RGB -> expected color index)
- **Unit**: config merge precedence (default < file < CLI)
- **Unit**: keybinding parsing and lookup
- **Snapshot**: same screen rendered with different themes -> distinct golden files
- **Integration**: launch with `--theme gruvbox` -> correct colors applied
- **Integration**: modify config file -> colors update without restart
- **PTY**: launch, press `h` -> help overlay appears with correct bindings

## Pitfalls
- **Theme color coverage**: missing a theme color for any element leaves it as the terminal default. The theme validator must check completeness.
- **256-color approximation**: the mapping from 24-bit to 256-color is non-trivial. Use the standard 6×6×6 color cube mapping. Some RGB values map poorly — test with common terminal palettes.
- **Config migration**: if the config format changes between versions, old config files must not crash the parser. Use version field and migration logic.
- **Hot-reload race**: config file may be partially written when fgof-watch triggers. Read the file, parse it — if parsing fails, keep the current config and retry on next change event.
- **XDG_CONFIG_HOME**: may not be set. Fall back to `~/.config/ftop/`. On macOS, consider `~/Library/Application Support/ftop/` as an alternative (or stick with XDG for consistency).
- **Keybinding conflicts**: user-configured bindings may conflict with each other. Detect and warn on startup, but don't refuse to start.

## Deferred
(none yet)

## Notes
- btop ships 15+ themes. We should ship at least 7 to demonstrate the system. Community themes can be contributed later.
- Consider supporting btop theme files as an import format — many users have favorite btop themes they'd want to bring to ftop.
- The help overlay is a great place to show version info, build details, and detected hardware.
