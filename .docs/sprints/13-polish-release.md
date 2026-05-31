# Sprint 13: Polish + Release

## Goal
Performance optimization, comprehensive CI, packaging for distribution, documentation, and final polish. Take ftop from "works" to "ships."

## Status: NOT STARTED

## Targets

### Performance Optimization
- [ ] Profile render path: identify and eliminate unnecessary ANSI output
- [ ] fgof-screen diff optimization: verify minimal cell updates per frame
- [ ] Collector efficiency: minimize file opens, use pre-opened descriptors where possible
- [ ] Memory audit: verify no unbounded growth, ring buffers don't leak
- [ ] CPU usage of ftop itself: should be < 1% CPU at 1Hz refresh on modern hardware
- [ ] Startup time: target < 500ms to first frame
  - [ ] Lazy GPU initialization (don't block on NVML in main thread)
  - [ ] Async first metric collection (show UI skeleton immediately, fill in data)
- [ ] Large process table performance: test with 10,000+ processes
  - [ ] Process scanning time < 100ms
  - [ ] Sort/filter time < 10ms
  - [ ] Render time < 16ms
- [ ] Terminal output optimization: batch writes, minimize flush calls
- [ ] Ring buffer memory: use compact types (int8 for percentages, int16 for temperatures)

### Stress Testing
- [ ] High process count: fork-bomb controlled test (10,000 processes)
- [ ] Rapid resize: resize terminal 100 times per second
- [ ] High I/O: stress disk and network during monitoring
- [ ] Low memory: monitor system under memory pressure
- [ ] Missing permissions: run as non-root, verify graceful degradation
- [ ] Terminal disconnect: what happens when SSH drops (SIGHUP handling)
- [ ] Long running: run for 24+ hours, check for memory growth

### CI Pipeline
- [ ] GitHub Actions workflow
  - [ ] Linux: Ubuntu latest, gfortran 12 + 13 + 14
  - [ ] FreeBSD: via vmactions/freebsd-vm or Cirrus CI
  - [ ] macOS: latest, gfortran via Homebrew
  - [ ] Build matrix: Debug + Release
- [ ] Test automation:
  - [ ] Unit tests on all platforms
  - [ ] Snapshot tests (golden file comparison)
  - [ ] Integration tests (PTY-based, may be Linux-only initially)
- [ ] Static analysis: gfortran `-fcheck=all -fsanitize=address,undefined` in Debug builds
- [ ] Release artifact building:
  - [ ] Linux x86_64 static binary
  - [ ] Linux aarch64 static binary (cross-compile or ARM runner)
  - [ ] FreeBSD x86_64 binary
  - [ ] macOS x86_64 + ARM universal binary
- [ ] Automated release workflow: tag -> build artifacts -> GitHub Release

### Packaging
- [ ] Binary releases on GitHub (per-platform tarballs)
- [ ] Package manifests:
  - [ ] Homebrew formula (`brew install ftop`)
  - [ ] AUR PKGBUILD (Arch Linux)
  - [ ] FreeBSD port Makefile
  - [ ] Debian/Ubuntu `.deb` package spec
  - [ ] RPM spec file (Fedora/RHEL)
  - [ ] Nix derivation
- [ ] man page: `ftop.1` covering usage, keybindings, config format
- [ ] Shell completions: bash, zsh, fish (for CLI arguments)

### Documentation
- [ ] README.md: overview, screenshots, installation, basic usage
- [ ] Screenshots/GIFs: capture terminal output showing all panels
- [ ] Config reference: document all TOML config keys with defaults and examples
- [ ] Theme authoring guide: how to create custom themes
- [ ] Contributing guide: build from source, run tests, code style
- [ ] Architecture doc: high-level overview for contributors (update design.md)

### Polish
- [ ] Startup experience: show splash/loading state briefly, then transition to dashboard
- [ ] Error messages: user-friendly messages for common issues (permission denied, no GPU, etc.)
- [ ] Terminal compatibility: test in major terminals
  - [ ] xterm, gnome-terminal, konsole, alacritty, kitty, wezterm, iTerm2, Terminal.app
  - [ ] tmux, screen (verify mouse, colors, alternate screen work)
  - [ ] SSH sessions (verify performance over latency)
- [ ] Edge case terminals: minimum 80x24 support, graceful degradation at smaller sizes
- [ ] Status bar: always-visible bottom bar with keybinding hints, refresh rate, alert count
- [ ] About dialog: version, build info, detected hardware, fgof library versions

### Version and Release
- [ ] Version scheme: semver (0.1.0 for initial release)
- [ ] Changelog generation
- [ ] Release checklist document

## Definition of Done
- ftop runs stable for 24+ hours without memory growth or crashes
- CPU usage of ftop itself is < 1% at default refresh rate
- Handles 10,000+ processes without perceptible lag
- CI passes on Linux (gfortran 12/13/14), FreeBSD, macOS
- Binary packages available for major platforms
- man page and README are complete
- All shipped themes render correctly in all tested terminals
- No known bugs of severity > "minor cosmetic"

## Dependencies
- All previous sprints (this is the final sprint)

## Testing
- Full regression: re-run all unit, snapshot, integration, and stress tests
- Terminal compatibility matrix: visual inspection across 10+ terminals
- Long-running soak test: 24-hour stability run with monitoring
- Installation test: verify package install on clean Ubuntu, Fedora, Arch, FreeBSD, macOS
- Upgrade test: verify config migration from older versions (if applicable)

## Pitfalls
- **Static linking**: gfortran's runtime (`libgfortran`) is not always statically linkable. May need to ship it as a shared library or use musl libc on Linux for fully static builds.
- **Terminal compatibility**: ANSI escape sequences vary between terminals. Test the actual binary, not just the escape codes. Some terminals handle truecolor differently or have bugs with braille characters.
- **Package maintenance**: shipping packages means committing to maintaining them. Start with binary releases and Homebrew, add distro packages as the project matures.
- **Screenshot automation**: terminal screenshots are hard to automate. Consider using `asciinema` or `vhs` (by Charm) for reproducible demo recordings.
- **man page formatting**: write in mandoc or groff format. Test rendering with `man -l ftop.1`.

## Deferred
(none yet — this is the shipping sprint)

## Notes
- The initial release should be 0.1.0, signaling "usable but evolving." Save 1.0.0 for when the feature set is stable and all three platforms are well-tested.
- Consider a `--benchmark` flag that runs the collector and renderer N times and reports average cycle time. Useful for performance regression testing in CI.
- A pre-release (0.1.0-rc1) to gather early feedback before the official release is strongly recommended.
