# Path Exclude

Run commands excluding individual binaries from `$PATH`. Useful for testing how a given program fails when a binary it requires is missing.

## Usage

```bash
px whoami jq python3 -- my-program
```
