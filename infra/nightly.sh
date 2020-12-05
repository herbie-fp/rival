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

function run-mpfi {
  echo "running mpfi on generated points"
  racket "infra/run-mpfi.rkt" "infra/all-points.txt" "$REPORTDIR/mpfi-results.txt"
}

function format-data {
  echo "Formatting the mpfi and mathematica data into latex table"
  racket "infra/format-mpfi.rkt" "$REPORTDIR/mathematica-output.txt" "$REPORTDIR/rival-output.txt" "$REPORTDIR/mpfi-results.txt" "$REPORTDIR/index.html"
  cp "infra/index.css" "$REPORTDIR"
}

function run-mathematica {
  echo "Converting points to mathematica script"
  racket "infra/run-mathematica.rkt" "infra/all-points.txt" "$REPORTDIR/mathematica-input.txt" "$REPORTDIR/mathematica-output.txt" "$REPORTDIR/rival-output.txt"
}

if [ -d "$REPORTDIR" ]
then
  rm -r "$REPORTDIR"
fi
mkdir "$REPORTDIR"

run-mpfi
run-mathematica
format-data
