#!/bin/bash
set -e -x

RHOST="uwplse.org"
RHOSTDIR="/var/www/rival"

upload () {
    B=$(git rev-parse --abbrev-ref HEAD)
    C=$(git rev-parse HEAD | sed 's/\(..........\).*/\1/')
    RDIR="$(date +%s):$(hostname):$B:$C"
    rsync --perms --chmod 755 --recursive report/ "$RHOST:$RHOSTDIR/$RDIR"
    if command -v nightly-results &>/dev/null; then
        nightly-results url "http://rival.uwplse.org/$RDIR"
    fi
}

upload