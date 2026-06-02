# Zoa MATLAB Client

A pure-JeroMQ MATLAB client for the `zoa_server` ZeroMQ analysis server.
No Python required — all communication goes through a Java ZeroMQ binding.

---

## Prerequisites

### 1. Build the Zoa server

From the repository root:

```bash
ninja -C buildHB
```

The server binary will be at `buildHB/zoa_server`.

### 2. Get a JeroMQ jar

JeroMQ is a pure-Java ZeroMQ implementation — no native library needed.

Download the latest jar from the GitHub releases page:
  https://github.com/zeromq/jeromq/releases

Look for `jeromq-<version>.jar` (e.g. `jeromq-0.6.0.jar`).
Drop the jar into this `matlab/` directory.

Alternatively, if you use Maven:
```xml
<dependency>
  <groupId>org.zeromq</groupId>
  <artifactId>jeromq</artifactId>
  <version>0.6.0</version>
</dependency>
```

### 3. Add to MATLAB Java path (one-time per session)

```matlab
javaaddpath('/path/to/zoa/matlab/jeromq-0.6.0.jar')
```

`ZoaClient` will do this automatically if the jar is in the `matlab/`
directory.  To make it permanent, add the path to your `javaclasspath.txt`
(see `edit(fullfile(prefdir,'javaclasspath.txt'))`).

---

## Usage

```matlab
% Add the matlab/ folder to the MATLAB path once:
addpath('/path/to/zoa/matlab')

% Start the server and connect
zoa = ZoaClient();

% Load a lens
zoa.cmd("RES osdtriplet");

% Run Seidel analysis, get structured result
r = zoa.analyze("SEI");

% Access the Seidel table (surfaces x 7 terms double matrix)
r.tables.seidel.data

% Column order: SA3, CMA3, AST3, DIS3, PTZ3, AC3, LC3
% Row order:    S0, S1, ..., S{n-1}, SUM

% Convenience scalars
r.scalars.sum_spherical
r.scalars.sum_coma

% Metadata
r.meta.title
r.meta.wavelength_nm

% Run any plain command and get the text output
txt = zoa.cmd("FIR");

% Shut down cleanly
zoa.close();
```

---

## JSON schema

The server returns JSON conforming to `zoa.result/1`:

```json
{
  "schema": "zoa.result/1",
  "type":   "seidel",
  "meta":   {"title": "OSDtriplet", "wavelength_nm": "587.5618", "units": "lens units"},
  "scalars": {"sum_spherical": -0.0209, ...},
  "tables":  {
    "seidel": {
      "row_labels": ["S0","S1",...,"SUM"],
      "col_labels": ["SA3","CMA3","AST3","DIS3","PTZ3","AC3","LC3"],
      "data": [[...], [...], ...]
    }
  }
}
```

`jsondecode` turns `tables.seidel.data` into a cell array; `zoaDecode`
returns it as-is (jsondecode with uniform arrays will produce a double
matrix automatically when all rows have the same length).

For grids (future analyses), `zoaDecode` decodes the base64 binary data
field into a `double` matrix of size `[nx, ny]`.

---

## Notes

- `ZoaClient` spawns `../buildHB/zoa_server` as a subprocess on a random
  free TCP port.  The server process is killed when `close()` is called.
- The server is **stateful**: lens data persists between `cmd()` calls.
- All `analyze()` calls go via `__JSON__ <command>` — the server runs the
  analysis headlessly and returns the structured JSON result.
- The `SEI` and `THO` commands both trigger `MMAB3_NEW` directly in
  headless mode (the GUI Seidel plot path is bypassed).
