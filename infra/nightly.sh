#!/bin/bash

function setup {
  raco pkg install --auto biginterval
  raco pkg install --auto fpbench
  rm -rf ./herbie
  git clone https://github.com/uwplse/herbie
}

function generate-points {
  racket "infra/generate-points.rkt" "./herbie/bench" "./infra/all-points.txt"
}

REPORTDIR="report"
MPFI_DATA="$REPORTDIR/mpfi-results.txt"
MATH_DATA="$REPORTDIR/mathematica-output.txt"
RIVAL_DATA="$REPORTDIR/rival-output.txt"

function clean {
  if [ -d "$REPORTDIR" ]; then
    rm -r "$REPORTDIR"
  fi
  mkdir -p "$REPORTDIR"
}

function run-mpfi {
  echo "running mpfi on generated points"
  racket "infra/run-mpfi.rkt" "infra/all-points.txt" "$1"
}

function run-mathematica {
  echo "Converting points to mathematica script"
  racket "infra/run-mathematica.rkt" "infra/all-points.txt" "$REPORTDIR/mathematica-input.txt" "$1" "$2"
}

function format-data {
  echo "Formatting the mpfi and mathematica data into latex table"
  racket "infra/report.rkt" "$MPFI_DATA" "$MATH_DATA" "$RIVAL_DATA" "$REPORTDIR/index.html" "$REPORTDIR/examples.txt" "$REPORTDIR/macros.txt"
  cp "infra/index.css" "$REPORTDIR"
}

function rerun {
  clean
  generate-points
  run-mpfi "$MPFI_DATA"
  run-mathematica "$MATH_DATA" "$RIVAL_DATA"
  format-data
  gzip -9 "$MPFI_DATA" "$MATH_DATA" "$RIVAL_DATA"
}

function all {
  setup
  rerun
}

for cmd in $@; do
    echo "Running $cmd"
    $cmd
done
