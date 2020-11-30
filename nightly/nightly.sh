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


function run-mpfi {
  echo "running mpfi on generated points"
  racket "infra/run-mpfi.rkt" "mpfi-reports/all-points.txt" "mpfi-reports/mpfi-results.txt"
}

function format-data {
  echo "Formatting the mpfi and mathematica data into latex table"
  racket "infra/format-mpfi.rkt" "mpfi-reports/mathematica-output.txt" "mpfi-reports/rival-output.txt" "mpfi-reports/mpfi-results.txt" "mpfi-reports/table.txt"
}

function run-mathematica {
  echo "Converting points to mathematica script"
  racket "infra/run-mathematica.rkt" "mpfi-reports/all-points.txt" "mpfi-reports/mathematica-input.txt" "mpfi-reports/mathematica-output.txt" "mpfi-reports/rival-output.txt"
}



for cmd in $@; do
    echo "Running $cmd"
    $cmd
done
