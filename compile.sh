#!/bin/bash

units=(
	StopWatch.pas
)

fpc ${units[*]} main.pas -o"fpclock"
