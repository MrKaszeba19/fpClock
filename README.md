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
    *  (*no flag*) - Show execution time of COMMAND in milliseconds
    * `-h, --help` - Print help
    * `-n, --no-feed-line` - Do not feed line after having shown output
- Example: `fpclock 'ls -l'`
