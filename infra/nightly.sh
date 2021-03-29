#!/bin/bash
set -e

function setup {
  raco pkg install --auto biginterval || true
  raco pkg install --auto fpbench || true
  rm -rf ./herbie || true
  git clone https://github.com/uwplse/herbie
}

function generate-points {
  racket "infra/generate-points.rkt" "./herbie/bench" "./infra/all-points.txt"
}

REPORTDIR="report"
MPFI_DATA="$REPORTDIR/mpfi-results.txt"
MATH_DATA="$REPORTDIR/mathematica-output.txt"
MATH_HEADERS="infra/headers.wls"
RIVAL_DATA="$REPORTDIR/rival-output.txt"
POINTS="infra/all-points.txt"

function clean {
  if [ -d "$REPORTDIR" ]; then
    rm -r "$REPORTDIR"
  fi
  mkdir -p "$REPORTDIR"
}

function run-mpfi {
  echo "running mpfi on generated points"
  racket "infra/run-mpfi.rkt" "infra/all-points.txt" "$MPFI_DATA"
}

function run-mathematica {
  echo "Converting points to mathematica script"
  rm "report/mathematica-output.txt" || true
  racket "infra/run-mathematica.rkt" "$POINTS" "$MATH_HEADERS" "$MATH_DATA"
}

function run-rival {
  echo "Running Rival"
  racket "infra/run-rival.rkt" "$POINTS" "$RIVAL_DATA"
}

function format-data {
  echo "Formatting the mpfi and mathematica data into latex table"
  racket "infra/report.rkt" "$MPFI_DATA" "$MATH_DATA" "$RIVAL_DATA" "$REPORTDIR/index.html" \
         "$REPORTDIR/examples.txt" "$REPORTDIR/macros.txt" "$REPORTDIR/sampled-plot.png" \
         "$REPORTDIR/bad-result-plot.png"
  cp "infra/index.css" "$REPORTDIR"
}

function rerun {
  clean
  generate-points
  run-mpfi
  run-rival
  run-mathematica
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
