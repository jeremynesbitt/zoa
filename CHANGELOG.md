# Changelog

All notable changes to Zoa are recorded here, newest first. Zoa is currently
beta (0.x) software under active development.

## 0.1.9 (beta)

The first Zoa release in over a year — and by far the largest. Roughly **600
commits** since 0.1.7 (Aug 2024), spanning a near-complete modernization of the
command language, the optimizer, the analysis tools, and the build/installer
pipeline on both macOS and Windows.

> Zoa remains **beta** software under active development. Please back up your
> work and report issues at https://github.com/jeremynesbitt/zoa/issues.

### Highlights

- **The optimizer actually works now.** A long-standing bug froze the lens
  during optimization (curvature/thickness variables never really took effect);
  that's fixed, along with the AUTUI crash.
- A modern **CODE V–style command language** dispatched ahead of the legacy KDP
  parser — dozens of new commands, faster and with real error reporting.
- **Multi-configuration (zoom)**, **undo/redo**, **per-field reference rays and
  vignetting**, typed **clear + edge apertures**, and a typed **solve manager**.
- Much-improved **lens-drawing (VIE)** and analysis plots, with data tables you
  can read alongside every plot.
- **Windows** builds and installer, and a **notarized, signed macOS installer**.

### Optimizer

- Fixed the frozen-lens bug: curvature and thickness variables now drive the
  merit function correctly. This is the big one — optimization previously never
  converged properly.
- **Unified merit model**: operands and constraints share one table, each row
  carrying a Role and a Weight. The AUTUI window shows Role, Weight, Value, and
  a **Contribution %** column so you can see each term's share of the objective.
- New general constraints: **MXT / MNT** (max/min thickness), **MNE** (min edge
  thickness), **MNA / MAE** (aperture/edge bounds), applied at `AUT ; … ; GO`.
- CLI merit entry: `NAME target [weight]` adds an objective; **LCON** lists the
  active merit rows and their roles; **UPD** / **CHA** update constraints from
  the command line.
- AUTUI usability: wider target/weight entry fields, working "Delete selected
  row", trimmed numeric formatting.

### Command language & CLI

- A CODE V–style front door dispatches registered commands before the legacy
  parser, so new commands are faster, report real errors, and no longer hit the
  old 140-character input limit.
- Notable new/expanded commands: **ZRN** (OPD→Zernike fit table), **RAYREF**
  (reference-ray report), **ZOO / POS** (zoom), **SET** (clear-aperture and
  other system settings), **THO / PLOTTHO** (third-order aberrations),
  **RESAUTO**, **SLB** (save surface labels), **IND** (refractive indices),
  **ETH** (edge thickness), **ASP / SPH** (surface type), **CUY / CUX / THI**
  solves, and coupled **VIE ORIENT**.

### Analysis & plotting

- Every analysis plot now populates a readable **Data tab** alongside the graph
  (OPD, Zernike-vs-field, RMS-vs-field, and more), with properly centered column
  headers.
- **Zernike vs. field**: correctly collapses to the actual number of defined
  fields (no more duplicate on-axis rows), and supports both dot-range (`5..9`)
  and comma-list (`9,16,25`) term selection.
- **Lens drawing (VIE)**: Zemax-style surface highlighting, cursor-hover global
  coordinate readout, autoscaling that accounts for aperture extent, and
  ray-footprint sizing for surfaces with no defined aperture.
- Plots refresh automatically when the lens, apertures, EPD, fields, or edge
  factor change.

### Lens editor & UI (GTK4)

- Variable / pickup / solve menus directly on lens-editor columns, including
  special-surface parameters and a **glass modifier** dropdown.
- Surface-type dropdown wired to the new ASP/SPH commands.
- Lens editor stays in sync with command-line edits (RDY/CUY/THI and friends no
  longer go stale).
- **Undo/redo** for the whole lens system (Edit menu / Cmd+Z), backed by a
  snapshot ring.
- Fixed several window use-after-free crashes when reopening the macro and
  optimizer windows.

### Apertures, solves, variables

- Typed **clear apertures** and per-surface **edge (physical) apertures**
  (`CIR EDG`), drawn in the ORTHO view, replacing the old ALENS-backed storage.
- Typed **solve manager** with CODE V solves (CUY/CUX/THI) that round-trip
  through save/load, plus `DEL SOL`.
- **Aspheric** parameters gained variable/pickup support (per-coefficient),
  and reference rays (R1–R5) with per-field vignetting (`SET VIG`,
  SysConfig GUI columns).

### Scripting: Jupyter & MATLAB

- **zoa_server** now emits structured JSON, enabling MATLAB and other clients to
  drive Zoa and parse results reliably; MATLAB examples included.
- Jupyter kernel (`zoa_kernel`) for notebook-driven sessions; it locates
  `zoa_server` inside an installed `Zoa.app` so notebooks work against the
  packaged app.
- See "Known limitations" for the current status of these bridges in the
  packaged app.

### Platform & installer

- **Windows**: gfortran and Intel compiler builds, plus an MSI / winget
  installer that reads the version from `version.txt`.
- **macOS**: fully scripted signed + **notarized** `.pkg` build; the installer
  version is now also driven from `version.txt` (previously a manual edit).

### Under the hood

- Modernized numerics: `REAL*8` → `real(real64)` throughout.
- Ongoing migration off the monolithic `ALENS` array to typed objects, and a
  strangler-fig migration off the legacy KDP text parser (typed, text-free
  `kdp_api` entry points that eliminate a class of numeric round-trip bugs).
- Removed ~45k lines of dead legacy optimizer code and the old RPN calculator.
- A golden-file regression suite (37 tests) now guards output across the app.
- Help/command reference is generated from in-source docblocks, with rendered
  math in the HTML docs.

### Known limitations

- **Multi-configuration (zoom)** support is minimal: it stores the commands that
  differentiate a configuration from the base config. Broader zoom support is
  planned.
- The **Jupyter / MATLAB bridges** require separate client-side setup and have
  not been fully verified against the notarized/packaged build.
- Zoa is beta; expect rough edges.
