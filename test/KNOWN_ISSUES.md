# Zoa Test Known Issues

## Environment Requirement

Tests that use `RES <lensname>` (e.g., `osdtriplet`) require
`~/Library/Application Support/Zoa/Projects/` to be synced with
`Library/Projects/` in the repo.

Tests that use `CV2PRG`, `ZMX2PRG`, `ZOA2CV`, or `ZOA2ZMX` require
`~/Library/Application Support/Zoa/CodeV/` to be synced with
`Library/CodeV/` in the repo (including the `zoa_singlet.zmx` and
`zoa_doublet.zmx` fixtures).

On a fresh clone, run:
```
cp Library/Projects/* "$HOME/Library/Application Support/Zoa/Projects/"
cp Library/CodeV/* "$HOME/Library/Application Support/Zoa/CodeV/"
```

---

## GUI-Only Plot Commands (no headless output)

The following command computes results headlessly but skips the plot step.
It remains in the test script for its text output only.

| Command | Behaviour | Affected test script |
|---------|-----------|---------------------|
| `ZERN_TST;GO` | Computes Zernike coefficients; `zern_go` returns before plotting when `HEADLESS_MODE` is true (the GTK drawing-area path is not yet bridged to plplot). No PNG is produced and no segfault occurs. | plots_wave.zoa |

---

## No Absolute Paths in Captured Output (rule)

Golden `.ref` files must never contain machine-specific paths. Commands that
report file locations emit only the filename via `zoa_emit`; the full path
goes to the terminal via `print *` (suppressed in the test runner). This was
fixed for `open_file_to_sav_lens` ("Full Path is"), `SAV` ("File name to save
is"), and `RESAUTO` ("Restoring from:"). Follow the same pattern for any new
file-handling command.

---

## Pre-existing Test Failure

`macro_ex1` uses the binary macro system which has a known unrelated failure.
It currently passes (the ref captures the error output), but any change to
macro infrastructure may break it.
