# ftop
btop/htop but written in fortran

## Vision
- Beautiful, Modern-designed TUI interface with sections for mem analysis, network, disks, gpus, process table, and any other onboard devices that would be useful for users to be able to monitor.
- As bespoke as possible
    - no shelling out to external libraries unless necessary
    - favor `fgof` libraries. (see the series of `fgof-` repos in ~/GithubOrgs/fgof-*)
    - try to opt for as little C source as possible, though if C is needed we will happily dive into interop implementations, especially if C is needed to get stable rendering and nice glyphs.
- Some kind of Cores visualization for CPU corest that shows relative load. I was thinking one larger blackbox with subboxes that are filled in to varying degrees/colors depending on their core usage.
    - see btop and htop for their approach/ideas.
        - we want to be novel, so steal, but invent new ui/ux that btop/htop don't have
- Signal sending, per-core like btop process table features

# Stages
- The first stage of implementation is planning, whereby we scaffold an entire written plan that covers all of implementation at a granular level.
- You clone both btop, htop, top, and any others that may be useful to .docs/refs/
- We have a back and forth about stack, design choices, libraries/deps, C, features from btop/htop to include, UI/UX from btop/htop to emulate, areas for improvement on htop and btop
- Once a formal plan has been established by the back and forth, we generate a series of sprints files in .docs/sprints/.
    - These files should
        - enumerate all targets from empty repo to finished product
        - establish DoD for the feature
        - Be living documents, updated as targets are hit or deferred
        - Include notes about pitfalls to avoid, common traps, etc.
- Once a sprint scaffold has been made, we start working through implementation by starting at sprint 00/01 and hitting all the targets, implementing cleanly, robustly, and efficiently the first pass through.

## Guidelines
- Commit often
    - keep messages terse, imperative, <250 chars unless elaboration on a decision/change is needed
    - avoid coauthoring commits
    - avoid writing "Generated with ..." in messages
- Tests are first class.
    - Unit, Integration, PTY Interactive harness, Stress, Sanity benches
- CI is first class
- Hardware monitoring accuracy is a primary goal.
- binary speed and responsiveness is a primary goal
- All deferred-during-implementation items MUST be explicitly punted to a later sprint file. do not let targets get lost in context/memory.
