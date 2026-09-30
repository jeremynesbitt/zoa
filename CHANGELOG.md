# Changelog

All notable changes to Zoa are recorded here, newest first. Zoa is currently
beta (0.x) software under active development.

## 0.1.10 (beta)

A focused follow-up to 0.1.9, about **70 commits**. Two themes: **your open
plots now survive closing the program**, via a new `.zin` companion file, and a
long list of smaller plotting improvements and fixes.

> Zoa remains **beta** software under active development. Please back up your
> work and report issues at https://github.com/jeremynesbitt/zoa/issues.

### Plot state that survives a restart (`.zin`)

- Saving a lens now also writes a **`.zin` companion file** beside the `.zoa`
  (the analog of Zemax's `.ZDA`). It captures every open plot tab — its
  settings, the command that made it, the plotted numbers, and the Data-tab
  table — and `RES` brings them all back, drawn from the stored data.
- **Plots come back at startup**, restored from the auto-saved lens Zoa already
  reloads, and on `RESAUTO`. The lens drawing (VIE) is restored by replaying its
  command.
- Loading a lens or starting a new one now **offers to discard open plots**
  rather than leaving stale windows behind, and File → Open behaves like `RES`.
- Fixed a **memory leak**: a plot's data arrays are now freed when its tab
  closes or replots, where previously every plot leaked.

### Plotting

- **Export a plot as a PNG**: File → *Export Current Plot to PNG*, or the
  `EXPORTPNG <file>` command. Works for the lens drawing as well as the
  analysis plots.
- **Spot diagram**
  - The ring pattern now has **one definition** for the whole program (it was
    duplicated, with different values, in two places). The default is 5 rings
    tracing 1 + 6 + 12 + 18 + 24 rays, at evenly spaced radii, each ring
    staggered by half its own angular step and sampled at zone centres — which
    removes the artificial ring that used to show up at the pupil edge.
    `NUMRINGS` sets the count.
  - **`AIRY ON`** overlays the Airy disk (radius 1.22·λ·F/#) on every field.
  - **Field Point** is now a dropdown listing the actual fields plus *All*, and
    a single field draws one correctly proportioned panel.
- **RMS vs Field (`PLTRMS`)**
  - **Reference** (`RSPH`: Chief / No Tilt / Best Focus) is now a per-plot
    setting, bracketed so it no longer leaks into other plots through the
    global it reads.
  - Pupil sampling is pinned to the plot's own **Density** setting, which is now
    a **16/32/64/128 dropdown** instead of a spin button that errored on any
    value that was not a power of two.
- **PMA / image plots** draw every row and column of the grid — the surface map
  was losing its outer edge, making it visibly asymmetric.
- The **Zernike vs Field** command is renamed **`ZRNFLD`** (was the temporary
  `ZERN_TST`).
- Plot settings carry **tooltips naming the CLI command** behind them, and
  labels are left-justified.
- New **Optimization menu** with the optimization UI.
- Readable tick labels on manually scaled axes; `FIE` gained `AST`/`DST` x-axis
  scales (the legacy `AST` command is now `ASTK`).

### First-order data

- **Entrance pupil diameter is correct again.** A system specified with a
  1013.2 mm entrance pupil was reported by `FIR` as 5291.07. The paraxial
  marginal ray was using the *sine* of the aperture angle where the slope is a
  *tangent*; the XZ chief ray is now iterated like the YZ one, and pupil data is
  refreshed after the retrace.
- `FIR` reports a real **working F-number at used conjugates** instead of the
  infinite-conjugate value.
- `IND` prints refractive indices to **6 decimals** rather than truncating.

### Fixes

- The OPD map stored pupil **X and Y swapped** (`DSPOT`).
- The **Data tab** no longer cuts off at 65536 characters, so a dense pupil map
  shows all of its rows.
- The **lens editor** table now grows with the window (the button gaps used to
  absorb the space), and the last surface's name no longer picks up garbage
  characters after an edit.
- Asking a plot for **more than 9 curves** no longer crashes; the extra curves
  are dropped with a message.
- Fixed crashes when **closing all plots** with an empty tab slot, and when
  **restoring plots at startup** before the drawing area was ready.
- Replotting after a lens change no longer spews errors, and a numbered plot
  command no longer appends a second `P<n>`.
- Quieter and cleaner: roughly **60 debug prints** removed, along with the
  GLib `GValue` criticals, the `pllsty: Invalid line style` abort, and a
  duplicate drag-and-drop controller — all three appeared on every startup.

## 0.1.9 (beta)

The first Zoa release in over a year — and by far the largest. Roughly **600
commits** since 0.1.7 (Aug 2024), spanning a modernization of the command language, the optimizer, and analysis tools, along with some under the hood improvements to the macOS and Windows building process.

> Zoa remains EXTREMELY **beta** software under active development. Please back up your
> work and report issues at https://github.com/jeremynesbitt/zoa/issues.

### Highlights

- **The optimizer actually works now.** A long-standing bug froze the lens
  during optimization (curvature/thickness variables never really took effect);
  that's fixed, along with the AUTUI crash.
- A modern **CODE V–style command language** dispatched ahead of the legacy KDP
  parser — dozens of new commands, faster and with real error reporting.
- **Multi-configuration (zoom)**, **undo/redo**, and some infrastructure changes to better support different surface types in the future.
- Many improvements in **lens-drawing (VIE)** and analysis plots, with data tables tabs for most plots to view the raw data.


### Optimizer

- **Unified merit model**: operands and constraints share one table in a new UI (AUTUI), each row carrying a type and a Weight. The AUTUI window shows type, Weight, Value, and
  a **Contribution %** column so you can see each term's share of the objective.
- New general constraints: **MXT / MNT** (max/min thickness), **MNE** (min edge
  thickness), **MNA / MAE** (aperture/edge bounds), applied at `AUT ; … ; GO`.
- CLI merit entry: `NAME target [weight]` adds an objective; **LCON** lists the
  active merit rows and their roles; **UPD** / **CHA** update constraints from
  the command line.
- AUTUI usability: wider target/weight entry fields, working "Delete selected
  row", trimmed numeric formatting.

### Command language & CLI

- A CODE V–style minimal command list is supported before the legacy parser.  Long term goal is to eliminate the old parser to better support new features.


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

### Interface with other Lens Design Programs
-Can import Code V and Zemax files
-Exporting to Code V and Zemax still unsupported

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
- Documentation is TERRIBLE.  There is some minimal help files, but woefully insufficient.  
