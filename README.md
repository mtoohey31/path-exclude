# Path Exclude

Run commands excluding individual binaries from `$PATH`. Useful for testing how a given program fails when a binary it requires is missing.

Inspired by [Nix's profiles](https://nixos.org/manual/nix/unstable/package-management/profiles.html).

## Usage

```bash
px whoami jq python3 -- my-program
```

## Installation

Path exclude should work on most Unix platforms. Windows is currently unsupported. If you run into problems, feel free to open an issue!

### Nix

Use the flake's `overlays.default` or `packages.default` outputs.

### Elsewhere

Follow [Idris2's installation guide](https://github.com/idris-lang/Idris2/blob/main/INSTALL.md), then run `make` from the root of this project. The resulting binary and support directory will be located in `build/exec`.
