#!/usr/bin/env bash
# genTestCases.sh — generate or validate test references (text + plots)
#
# First run (no .ref file):  runs the test and saves output as the reference.
# Subsequent runs:            compares output against the saved reference.
#
# Text output → test/NAME.ref
# Plot PNGs   → test/NAME.ref.p1.png, test/NAME.ref.p2.png, …
#
# Cairo rendering is deterministic so plots are compared byte-for-byte.
# Set ZOA_PLOT_TOLERANCE=1 to use ImageMagick fuzzy comparison instead
# (requires: magick or compare on PATH).

BINARY=./buildHB/zoa_test
TESTDIR=test
PASS=0; FAIL=0; SKIP=0

# --- image comparison helper ------------------------------------------------
compare_images() {   # args: ref_png actual_png label
  local ref="$1" actual="$2" label="$3"
  if [ ! -f "$actual" ]; then
    echo "  FAIL (image missing): $label"
    FAIL=$((FAIL+1))
    return
  fi
  if [ "${ZOA_PLOT_TOLERANCE:-0}" = "1" ]; then
    # Fuzzy comparison via ImageMagick (counts differing pixels)
    local magick_bin=""
    command -v magick   >/dev/null 2>&1 && magick_bin="magick compare"
    command -v compare  >/dev/null 2>&1 && magick_bin="compare"
    if [ -z "$magick_bin" ]; then
      echo "  SKIP (ImageMagick not found): $label"
      SKIP=$((SKIP+1))
      return
    fi
    local diff
    diff=$($magick_bin -metric AE "$ref" "$actual" /dev/null 2>&1 | head -1)
    if [ "$diff" = "0" ]; then
      echo "  PASS (image): $label"
      PASS=$((PASS+1))
    else
      echo "  FAIL (image, $diff differing pixels): $label"
      FAIL=$((FAIL+1))
    fi
  else
    # Exact byte comparison (default — deterministic Cairo rendering)
    if cmp -s "$ref" "$actual"; then
      echo "  PASS (image): $label"
      PASS=$((PASS+1))
    else
      echo "  FAIL (image differs): $label"
      FAIL=$((FAIL+1))
    fi
  fi
}

# --- main loop --------------------------------------------------------------
for f in "$TESTDIR"/*.zoa; do
  base="${f%.zoa}"
  ref="${base}.ref"
  name=$(basename "$base")

  # Clear any plot files left from a previous run
  rm -f /tmp/zoa_plot_*.png

  if [ ! -f "$ref" ]; then
    # ── Generate mode ──────────────────────────────────────────────────────
    "$BINARY" "$f" 2>/dev/null > "$ref"
    echo "GEN (text): $name"

    # Save any generated plots as reference images
    n=1
    for png in /tmp/zoa_plot_*.png; do
      [ -f "$png" ] || break
      dest="${base}.ref.p${n}.png"
      cp "$png" "$dest"
      echo "GEN (image $n): $name → $(basename "$dest")"
      n=$((n+1))
    done

  else
    # ── Compare mode ───────────────────────────────────────────────────────
    actual=$("$BINARY" "$f" 2>/dev/null)
    expected=$(cat "$ref")

    if [ "$actual" = "$expected" ]; then
      echo "PASS (text): $name"
      PASS=$((PASS+1))
    else
      echo "FAIL (text): $name"
      FAIL=$((FAIL+1))
      # Show a compact diff
      diff <(echo "$expected") <(echo "$actual") | head -20
    fi

    # Compare each reference plot image against what this run produced
    n=1
    for ref_png in "${base}.ref.p"*.png; do
      [ -f "$ref_png" ] || break
      compare_images "$ref_png" "/tmp/zoa_plot_${n}.png" "$name (plot $n)"
      n=$((n+1))
    done
  fi
done

# --- summary ----------------------------------------------------------------
echo ""
echo "Results: $PASS passed, $FAIL failed, $SKIP skipped"
[ "$FAIL" -eq 0 ]   # exit 1 if any failures
