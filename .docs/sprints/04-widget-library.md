# Sprint 04: Widget Library

## Goal
A complete set of reusable rendering widgets built on fgof-screen: boxes, text labels, bar meters, sparkline charts, braille time-series graphs, and a color gradient system with truecolor support. These are the building blocks for all dashboard panels.

## Status: NOT STARTED

## Targets

### Color System
- [ ] `src/ftop_color.f90` — color management
  - [ ] `type :: rgb_color` — (r, g, b) 0-255
  - [ ] `type :: color_gradient` — array of RGB stops with interpolation
  - [ ] Truecolor ANSI generation: `\e[38;2;r;g;bm` (fg), `\e[48;2;r;g;bm` (bg)
  - [ ] 256-color fallback: map RGB to nearest 256-color index
  - [ ] Named color constants: standard 16, common UI colors
  - [ ] Gradient presets: green-yellow-red (CPU/mem), blue-cyan (network), green-blue (disk)
  - [ ] Gradient interpolation: given value 0.0-1.0, return interpolated RGB

### Box Widget
- [ ] `src/widgets/ftop_box.f90`
  - [ ] Draw bordered rectangle with title
  - [ ] Border styles: single (`─│┌┐└┘`), double (`═║╔╗╚╝`), rounded (`╭╮╰╯`), heavy (`━┃┏┓┗┛`)
  - [ ] Title positioning: left, center, right
  - [ ] Content area calculation (inner rect minus border)
  - [ ] Padding support

### Text Widget
- [ ] `src/widgets/ftop_text.f90`
  - [ ] Render styled text at position
  - [ ] Alignment: left, center, right
  - [ ] Truncation with ellipsis for overflow
  - [ ] Printf-style formatting for numbers (e.g., "47.3%", "1.2 GiB")

### Bar Meter Widget
- [ ] `src/widgets/ftop_meter.f90`
  - [ ] Horizontal bar: `[████████░░░░░░░░]` style
  - [ ] Fill characters: block (`█`), gradient blocks (`░▒▓█`)
  - [ ] Color gradient applied to fill (green at 0% -> red at 100%)
  - [ ] Label overlay (e.g., "47%" centered on bar)
  - [ ] Compact mode: single-line, no border

### Sparkline Widget
- [ ] `src/widgets/ftop_sparkline.f90`
  - [ ] Single-row spark using block characters: `▁▂▃▄▅▆▇█`
  - [ ] Maps values array to block height (0.0-1.0 normalized)
  - [ ] Color gradient applied per character
  - [ ] Configurable width (number of recent samples shown)
  - [ ] Min/max auto-scaling or fixed range

### Braille Graph Widget
- [ ] `src/widgets/ftop_graph.f90`
  - [ ] Multi-row braille-character time series (2x4 dot pattern per character cell)
  - [ ] Unicode braille range: U+2800 to U+28FF
  - [ ] Subpixel resolution: 2 horizontal × 4 vertical dots per cell
  - [ ] Y-axis auto-scaling with labeled ticks
  - [ ] Grid lines (optional, using dim braille dots)
  - [ ] Area fill below the line (using braille dots)
  - [ ] Multiple series overlay (different colors)
  - [ ] Configurable height and width
  - [ ] Color gradient from bottom to top of graph

### Table Widget
- [ ] `src/widgets/ftop_table.f90`
  - [ ] Column definitions: name, width (fixed/auto/weight), alignment, formatter
  - [ ] Header row with column labels
  - [ ] Scrollable row display (viewport into data)
  - [ ] Row selection (highlight current row)
  - [ ] Sort indicator in header (▲/▼)
  - [ ] Column separator style (space, thin line, heavy line)
  - [ ] Row striping (alternating background, optional)

### Widget Base
- [ ] `src/ftop_widgets.f90` — abstract widget interface
  - [ ] `type, abstract :: widget`
  - [ ] Deferred: `render(screen_buffer, rect)` — draw into a rectangular region
  - [ ] Deferred: `min_size()` -> (min_width, min_height)
  - [ ] Common: position, size, visibility, focus state

## Definition of Done
- Each widget renders correctly into an fgof-screen buffer
- Widgets handle edge cases: zero-width, zero-height, single-cell, oversized content
- Color gradients produce correct truecolor ANSI sequences
- Braille graph renders a sine wave with visible subpixel resolution
- Table widget scrolls, selects, and sorts
- All widgets have snapshot tests comparing buffer output to golden files

## Dependencies
- Sprint 00 (build system, fgof-screen)
- Sprint 01 (ANSI rendering primitives — some overlap, Sprint 04 can start early)

## Testing
- **Snapshot**: each widget rendered at known sizes with known data -> compare buffer against golden
- **Unit**: color gradient interpolation (known inputs -> expected RGB)
- **Unit**: braille dot mapping (known value -> expected braille codepoint)
- **Unit**: sparkline block selection (known values -> expected block characters)
- **Unit**: table column width calculation with various content
- **Edge cases**: empty data, single sample, all-zero, all-maximum, negative values
- **Visual**: manual inspection of each widget type in a real terminal

## Pitfalls
- **Braille character encoding**: braille patterns encode dots as bit positions in the codepoint offset from U+2800. Dot 1 (top-left) = bit 0, dot 2 = bit 1, ..., dot 8 (bottom-right) = bit 7. Getting the bit mapping wrong produces garbled graphs.
- **Terminal braille support**: not all terminals render braille characters correctly. Some may show them as double-width or substitution characters. Provide a fallback to block-character graphs.
- **Color gradient interpolation**: linear RGB interpolation produces muddy midpoints. Consider interpolating in HSL or Oklab color space for perceptually uniform gradients. Start with RGB, optimize later.
- **Table column width with Unicode**: CJK characters are double-width. Use `wcwidth()` via C shim for accurate column width calculation.
- **Screen buffer coordinates**: fgof-screen uses 0-indexed or 1-indexed? Verify and be consistent. Off-by-one errors in widget positioning are extremely common.
- **Widget overflow**: a widget must never write outside its assigned rect. Clip all output to the widget's bounds.

## Deferred
(none yet)

## Notes
- The widget library should be general enough to eventually extract into its own fgof library (fgof-widgets or fgof-tui). Design accordingly — no ftop-specific logic in widget implementations.
- btop's graph rendering in `btop_draw.cpp` is a good reference for braille graph implementation. Study how it maps values to braille dot positions.
- Consider supporting both filled (area) and line (outline) graph styles. btop uses filled; line graphs can show multiple series more clearly.
