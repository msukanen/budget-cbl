# `BUDGET`, a simple budget keeper.
A ***very*** simple CSV-file based "budget tracker". Nothing fancy, just
insert your initial budget and credit/debit into `DATA/BUDGET.CSV` and run.

## Files
* `bin` - the `budget` executable will appear here.
* `CBL` - the source files.
* `CPY` - "copybook" files, if there is/will be any.
* `DATA` - a placce for your `BUDGET.DTA`, see [DATA/README.md](DATA/README.md)

# Requirements
* **CMake** - ditto.
* (Gnu)**COBOL** — I've been using GnuCOBOL, but the app probably (or rather,
maybe) compiles just fine with almost any COBOL variant out there.

# Notes
Linux/Raspberry Pi/WSL — the trio I've compiled this in. Your mileage varies
with other systems, but e.g. with MS-Windows the process *should* work almost
out of the box as-is.
