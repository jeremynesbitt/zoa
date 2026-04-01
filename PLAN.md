# Zoa: Testing, Refactoring, and Jupyter Interface Plan

## Context

Zoa is a Fortran optical design program where all actions are possible via CLI commands. The program currently requires full GTK4 initialization to run — all output flows through `SHOWIT()` → `updateTerminalLog()` → GTK text view. This means you can't run commands without the GUI, which blocks both automated testing and a Jupyter interface.

The key insight: **both testing and Jupyter require the same prerequisite — decoupling command execution and output from GTK.** This is Phase 0.

---

## Phase 0: Decouple Output from GTK (Shared Prerequisite)

**Goal:** Make it possible to run `PROCESKDP()` commands and capture output without GTK.

### 0.1 — Create `src/zoa-output.f90` (NEW)
An output abstraction module with **zero GTK dependencies**:
- Defines an abstract `output_handler_iface(text, color)` interface
- A module-level procedure pointer `output_handler` defaulting to `zoa_output_to_stdout`
- `zoa_emit(text, color)` — calls whatever handler is registered
- `zoa_set_output_handler(handler)` — swaps the handler
- `zoa_output_to_stdout` — simple `write(*,*)` fallback

### 0.2 — Modify `SHOWIT()` in `HARDWAR1.FOR:2983`
- Replace `call updateTerminalLog(trim(OUTLYNE), "black")` with `call zoa_emit(trim(OUTLYNE), "black")`
- Replace `use handlers` with `use zoa_output` (SHOWIT only uses handlers for updateTerminalLog)
- All ~19,560 calls to SHOWIT are handled by this single change

### 0.3 — Fix direct `updateTerminalLog` callers (~16 files, ~143 calls)
- Replace direct `call updateTerminalLog(...)` with `call zoa_emit(...)` in all non-GUI files
- Files like `codeV-commands.f90`, `codeV-sur.f90`, `CMDER.FOR`, etc.

### 0.4 — Register GTK handler at startup in `zzhandlers.f90`
- In `activate()`, after GTK init: `call zoa_set_output_handler(updateTerminalLog)`
- GUI works exactly as before; headless mode gets stdout output

### 0.5 — Create `src/zoa-headless-init.f90` (NEW)
- Calls `INITKDP()` to set up COMMON blocks, glass catalogs, defaults
- Initializes `curr_lens_data`, `sysConfig`, `ioConfig` (with null pointers)
- Sets `basePath` for data file access
- No GTK initialization

### 0.6 — Guard GUI-only commands
- In `CMDER.FOR`, commands like `EDITOR`, `MACROUI`, `SYSCONFIGUI` need a headless guard
- Check a `HEADLESS_MODE` flag and print "Command requires GUI" instead of crashing

**Files modified:** `HARDWAR1.FOR`, `zzhandlers.f90`, `codeV-commands.f90`, `codeV-sur.f90`, `CMDER.FOR`, + ~12 other files with direct updateTerminalLog calls
**Files created:** `src/zoa-output.f90`, `src/zoa-headless-init.f90`

---

## Phase 1: Testing Infrastructure

**Goal:** CLI-based test framework — feed commands in, capture output, compare to reference.

### 1.1 — Create `src/zoa-test-capture.f90` (NEW)
- In-memory output buffer (array of captured lines)
- `capture_handler(text, color)` — stores output lines
- `clear_capture_buffer()` / `get_captured_output()` / `write_captured_to_file(filename)`
- `compare_with_reference(ref_file, tolerance, passed)` — line-by-line comparison with floating-point tolerance (default 1e-6 relative)

### 1.2 — Create `src/zoa-test-runner.f90` (NEW) — test runner executable
- Reads `.zoa` test script files (one command per line)
- Calls `zoa_headless_init()`, registers `capture_handler`
- Executes each line via `PROCESKDP()`
- Compares captured output to `.ref` reference file
- Returns exit code 0 (pass) or 1 (fail) with diff on failure
- Usage: `zoa_test test/paraxial.zoa test/paraxial.ref`

### 1.3 — Build system changes in `meson.build`
```meson
test_runner = executable('zoa_test',
  sources: ['src/zoa-test-runner.f90'],
  link_with: zoa_lib,
  dependencies: [zoa_deps],
  install: false)

test('fieldflattener', test_runner,
     args: ['test/fieldflattener.zoa', 'test/fieldflattener.ref'])
```

### 1.4 — Create initial test cases
Generate reference output by running tests against the current (known-good) build:

| Test file | Purpose |
|-----------|---------|
| `test/basic_lens_entry.zoa` | Lens data entry: RD, TH, GLA commands |
| `test/paraxial.zoa` | Paraxial calculations: EFL, BFL, FNO |
| `test/fieldflattener.zoa` | Optimization (already exists) |
| `test/raytrace.zoa` | Ray tracing: RAY, RTG |
| `test/glass_catalog.zoa` | Glass catalog lookups |

Each `.zoa` file gets a corresponding `.ref` file with expected output.

### 1.5 — Run with `meson test` or `ninja test`

**Files created:** `src/zoa-test-capture.f90`, `src/zoa-test-runner.f90`, `test/*.zoa`, `test/*.ref`
**Files modified:** `meson.build`, `src/meson.build`

---

## Phase 2: Refactoring (Ongoing, Protected by Tests)

**Goal:** Incrementally reduce global state and improve code structure. Each change verified by the test suite.

### 2.1 — Library split: `zoa_core_lib` (no GTK) vs `zoa_gui_lib`
- Separates computational engine from UI
- Test runner links only `zoa_core_lib` (no GTK dependency needed)
- Requires resolving which files `use handlers` for GUI vs just for output (output already decoupled by Phase 0)

### 2.2 — COMMON block → module migration
- Pattern already established: `mod_DATLEN.f90`, `mod_DATMAI.f90` wrap COMMON blocks in modules
- Extend to remaining `.INC` files one at a time
- Each migration: convert, run tests, commit

**Current state (as of 2026-04-01):**

Modules already created: `mod_DATLEN`, `mod_DATMAI`, `mod_DATSUB`, `mod_DATCFG`, `mod_DATSPD`, `mod_DATSP1`, `mod_DATHGR`

Remaining `.INC` files fall into three categories:

*Actual COMMON block files — still need modules:*
- `DATPTS.INC` — part drawing tolerances; 3 `.FOR` users (`PLOTCAD6`, `PLOTCAD8`, `USER1`), no `.f90` users yet
- `DATNSS.INC` — **dead file**: defines `NSSCOMMON1` block but is never `INCLUDE`d anywhere; NSS files declare variables locally. Safe to delete.

*Inline code fragments (not COMMON blocks — not candidates for module wrapping):*
- `LENO.INC`, `GLASSP.INC`, `SPSRF.INC` — GUI dialog handling code included inline inside subroutines
- `FRESHCLAP.INC`, `GLASSPFRESH.INC` — aperture/glass dialog widget-setting code
- `NOTCAT.INC`, `NOTMODEL.INC`, `NOTMYGLASS.INC` — WDialog field-state snippets
- `CATNAMER.INC` — inline IF chain mapping filenames to catalog names
- `codeV-interfaces.INC` — interface block declarations (not COMMON blocks)

*The bigger work:* All existing modules are used only by `.f90` files. The `.FOR` files (137 for DATMAI, 121 for DATLEN, etc.) still `INCLUDE` the `.INC` directly. The real Phase 2.2 payoff is migrating `.FOR` subroutines to `USE modname` instead of `INCLUDE '*.INC'` — enabling eventual deletion of the `.INC` files. Suggested starting point: `DATSUB` (38 users) or `DATCFG` (33 users) as they're smaller scope than DATMAI/DATLEN.

### 2.3 — Reduce global mutable state
- Start with `OUTLYNE` (output string) — pass as argument instead of COMMON
- Then `INPUT` (command string) — already partially passed in PROCESKDP
- Create a `parsed_command` type to replace `WC`, `WQ`, `W1`–`W5` globals

### 2.4 — Migrate CMDER commands to CodeV-style infrastructure
- The goal is to **migrate or obsolete CMDER.FOR entirely**, moving commands into the `zoaCmds` registry in `codeV-commands.f90`
- CodeV-style commands are the industry standard and the target command syntax
- Migration strategy: one command (or group) at a time — add to `zoaCmds`, remove from CMDER.FOR, run tests
- Legacy commands that have no CodeV equivalent can be registered with their original names
- Once all commands are migrated, CMDER.FOR can be deleted

---

## Phase 3: Jupyter / ZeroMQ Interface

**Goal:** Type Zoa commands in Jupyter notebook cells, get output back.

### 3.1 — Add fzmq dependency
- [fzmq](https://github.com/richsnyder/fzmq) wraps libzmq for Fortran
- Add as meson subproject or system dependency

### 3.2 — Create `src/zoa-server.f90` (NEW) — ZeroMQ server executable
- Third executable alongside `Zoa` (GUI) and `zoa_test` (tests)
- REQ/REP pattern: receive command string, execute via `PROCESKDP`, return captured output
- Uses same `zoa_headless_init()` and `capture_handler` from Phase 1

### 3.3 — Create `jupyter/zoa_kernel.py` (NEW) — Python Jupyter kernel
- Starts `zoa_server` as subprocess
- Connects via ZeroMQ
- Sends cell contents as commands, displays returned text
- Installable via `pip install -e jupyter/` + `jupyter kernelspec install`

### 3.4 — Enhanced output for rich Jupyter display
- Structured JSON responses for lens tables, aberration data (displayed as formatted tables in notebooks)
- **Inline plot display:** PlPlot renders to PNG file, server sends PNG data back via ZeroMQ, Jupyter kernel displays as `IPython.display.Image` — enables spot diagrams, ray fans, MTF plots, etc. directly in notebook cells
- Protocol: message type header (`TEXT`, `JSON`, `IMAGE`) followed by payload, so the kernel knows how to render each response
- Future: interactive plot widgets using ipywidgets for things like lens layout manipulation

---

## Execution Order

```
Phase 0  ──→  Phase 1  ──→  Phase 2 (ongoing)
                  │
                  └──→  Phase 3 (can start after Phase 1)
```

**Start with:** Phase 0 + Phase 1 (the output decoupling and test infrastructure).
Phase 2 and 3 can proceed in parallel once tests are in place.

## Verification

- After Phase 0: Rebuild GUI (`ninja -C buildHB`), verify it works identically
- After Phase 1: `meson test -C buildHB` runs all tests, all pass
- After each Phase 2 change: `meson test` confirms no regressions
- After Phase 3: Open Jupyter notebook, type a command like `RES doublet; LIS`, see lens listing output
