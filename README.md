# fpclock 

**fpclock** – a program measuring process execution time

Author: Paul Lipkowski

## Installation

- Required: FreePascal Compiler `fpc` (version 3.0.4 or newer)
- So far tested only on Linux amd64. Not designed for 32-bit computers.
- Compile using `compile.sh`
- Install to `$PATH` using `installBash.sh`

## Usage 
- Syntax: `fpclock 'process' [flags]`
- Available flags:
    *  (no flag) – Show execution time of COMMAND in seconds with precision of 4 digits and feed the line afterwards
    * `-h`, `--help` – Print help
    * `-n`, `--no-feed-line` – Do not feed the line after having shown output
    * `-p N`, `--prec=N` – Set precision to N digits (default N=4)
    * `-u U`, `--units=U` – Set measurement unit to U' (see more Us below)
- Available units with their flag values (`U`):
    * days – `d` or `days`
    * minutes – `m`, `mins` or `minutes`
    * seconds – `s`, `secs` or `seconds`
    * milliseconds – `ms`, `milli` or `milliseconds`
    * microseconds – `μs`, `mus`, `micro` or `microseconds`
    * ticks – `ticks`
    * nanoseconds – `ns`, `nano` or `nanoseconds` (stopwatch is accurate to 1 tick = 100 ns though).
- Example: 
    * `fpclock 'ls -l'`
    * `fpclock 'cp ./foo/ ./bar -r' -n`
    * `fpclock 'cp ./foo/ ./bar -r' -n -p 6 -u ms`
    * `fpclock 'cp ./foo/ ./bar -r' -n --prec=6 --units=milliseconds`
