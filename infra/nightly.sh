#!/bin/bash

function run {
    bench="$1"; shift
    name="$1"; shift
    
    echo "Running herbie sampling on $name"
    seed=$(date "+%Y%j")
    racket "src/herbie.rkt" report  \
	   --num-iters 0 \
	   --note "mpfi" \
	   --debug \
	   --seed "$seed" \
     --suite "$name" \
     --timeout 60000 \
	   "$bench" "mpfi-reports/$name"
}

function generate-points {
  echo "clearing reports"
  report=$(git rev-parse --abbrev-ref HEAD)-$(date "+%Y-%m-%d")
  rm -rf mpfi-reports
  mkdir -p mpfi-reports

  for bench in bench/*; do
    name=$(basename "$bench" .fpcore)
    run "$bench" "$name" "$@"
  done
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
  racket "infra/report.rkt" "$1" "$2" "$3" "$4"
  cp "infra/index.css" "$REPORTDIR"
}

function all {
  clean
  run-mpfi "$MPFI_DATA"
  run-mathematica "$MATH_DATA" "$RIVAL_DATA"
  format-data "$MPFI_DATA" "$MATH_DATA" "$RIVAL_DATA" "$REPORTDIR/index.html"
}


for cmd in $@; do
    echo "Running $cmd"
    $cmd
done
