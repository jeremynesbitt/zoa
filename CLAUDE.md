# Zoa Developer Notes for Claude

## How to Add a New CLI Command

New-style Zoa commands live in the `codeV_commands` module and its submodules.
Four files must be touched, always in this order:

### 1. Declare the interface — `src/codeV-interfaces.INC`

Add a `module subroutine` declaration inside the existing `interface` block (before `end interface`):

```fortran
   module subroutine execFOO(iptStr)
    character(len=*) :: iptStr
   end subroutine execFOO
```

Use `module subroutine` for commands that take the full input string, or
`module subroutine execFOO()` (no argument) for commands that read globals directly.

### 2. Register the command — `src/codeV-commands.f90`

In the `initCodeVCommands` subroutine, add a new slot after the last used slot
(currently `zoaCmds(682)`):

```fortran
        zoaCmds(683)%cmd = "FOO"
        zoaCmds(683)%execFunc => execFOO
```

`%cmd` is the keyword the user types (case-sensitive, uppercase).
`%execFunc` is a procedure pointer that matches the `(iptStr)` signature.
`iptStr` will contain the **full** input line (e.g. `"FOO BAR 3.14"`).

### 3. Implement the procedure — appropriate submodule

Pick the submodule that best fits:

| Submodule file | Purpose |
|---|---|
| `src/codeV-commands-lensops.f90` | Lens loading, saving, surface modifiers |
| `src/codeV-commands-editops.f90` | Variable codes, constraints, optimization |
| `src/codeV-commands-plots.f90`   | Plot commands |
| `src/codeV-commands-utils.f90`   | Utilities, CLI helpers |

Add the implementation **inside** the existing `submodule … contains … end submodule` block:

```fortran
    module procedure execFOO
        use command_utils, only : parseCommandIntoTokens
        implicit none

        character(len=80) :: tokens(40)
        integer :: numTokens

        call parseCommandIntoTokens(trim(iptStr), tokens, numTokens, ' ')
        ! tokens(1) == "FOO", tokens(2) is the first argument, etc.

        ! ... implementation ...
    end procedure execFOO
```

### 4. For GUI-only commands — headless guard

If the command opens a GTK window, guard it so headless/test mode gets a clean error:

```fortran
        use globals, only: HEADLESS_MODE
        if (HEADLESS_MODE) then
            call zoa_emit("FOO requires GUI", "red")
            return
        end if
```

### 5. Verify

```bash
ninja -C buildHB          # must compile cleanly
meson test -C buildHB     # 5/6 must still pass (macro_ex1 is a pre-existing failure)
```

---

## Notes on the Command Lookup

- `startCodeVLensUpdateCmd(iptCmd)` in `codeV-commands.f90` receives only the **first token**
  as `iptCmd` (uppercase). It loops over `zoaCmds` looking for an exact match.
- When found, it calls `zoaCmds(ii)%execFunc(currentCommand)` where `currentCommand` is the
  **full** original input line — so your procedure gets all tokens.
- Commands that need subcommands (e.g. `EDI PREF`) parse `iptStr` themselves using
  `parseCommandIntoTokens`.

---

## Pre-existing Test Failure

`macro_ex1` (binary macro system) is a known failing test — unrelated to command additions.
All other 5 tests must pass after any change.
