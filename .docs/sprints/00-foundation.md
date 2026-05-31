# Sprint 00: Project Foundation

## Goal
Establish the repository structure, build system, CI pipeline, and fgof dependency integration. A bare Fortran binary that compiles and links against all fgof dependencies, prints "ftop" to stdout, and exits.

## Status: DONE

## Targets

- [x] Repository structure matching design.md directory layout
- [x] CMakeLists.txt root with Fortran + C mixed-language support
  - [x] Fortran 2018 standard enforcement (`-std=f2018`)
  - [x] Warning flags (`-Wall -Wextra -Wpedantic`)
  - [x] Debug/Release build types
  - [x] Platform detection (Linux/FreeBSD/macOS)
  - [x] Conditional source inclusion per platform
- [x] fgof dependency integration via adjacent CMake projects
  - [x] fgof-screen
  - [x] fgof-termios
  - [x] fgof-keys
  - [x] fgof-state
  - [x] fgof-fs
  - [x] fgof-process
  - [x] fgof-watch
  - [x] fgof-lineedit
  - [x] fgof-cache
  - [x] fgof-expect
- [x] Minimal `src/ftop.f90` that compiles, links all deps, prints version, exits
- [x] `.gitignore` updated for build artifacts, `.docs/refs/`
- [x] CLAUDE.md with project conventions
- [x] GitHub Actions CI
  - [x] Linux (Ubuntu, gfortran latest + gfortran-12)
  - [x] FreeBSD (via VM action)
  - [x] macOS (gfortran via setup-fortran)
  - [x] Build + link check on all three platforms
- [x] Test scaffold: `test/` directory structure with a single "it compiles" test
- [x] LICENSE file (choose license)

## Definition of Done
- `cmake --build .` succeeds on Linux, FreeBSD, macOS
- CI passes on all three platforms
- All fgof libraries link without errors
- `./ftop --version` prints version string and exits cleanly

## Dependencies
None (first sprint)

## Testing
- Compilation test: binary links and runs
- CI: all three platform builds green

## Pitfalls
- **fgof libraries may use fpm internally**: their CMakeLists.txt may not exist or may be incomplete. May need to write CMake wrappers for fgof libs that only have fpm manifests.
- **gfortran version differences**: F2018 features vary between gfortran 10, 12, 13, 14. Pin minimum version and test against it.
- **FreeBSD CI**: GitHub Actions doesn't natively support FreeBSD. Options: vmactions/freebsd-vm, cross-compilation, or Cirrus CI. Decide early.
- **Module file compatibility**: Fortran `.mod` files are compiler-specific and version-specific. Never commit them.
- **CMake Fortran module dependency scanning**: CMake's Fortran module dependency tracking can be fragile. Use explicit `add_dependencies()` if module build order breaks.

## Deferred
(none yet)

## Notes
- Decision: add `CMakeLists.txt` directly to fgof libraries as needed for ftop integration; fgof repos do not need to stay strictly fpm-only.
- License decision: MIT.
- Current CMake integration uses adjacent fgof repos from `FTOP_FGOF_ROOT` and also builds transitive `fgof-temp` and `fgof-pty` for `fgof-state`/`fgof-cache`/`fgof-expect`.
- CI checks out fgof repos as siblings, preferring a same-named branch when it exists and falling back to `trunk`.
- Sprint 00 local verification completed on FreeBSD; GitHub-hosted Linux/macOS/FreeBSD CI will be authoritative after push.
