#!/bin/bash
set -e -x

RHOST="uwplse.org"
RHOSTDIR="/var/www/rival"

upload () {
    B=$(git rev-parse --abbrev-ref HEAD)
    C=$(git rev-parse HEAD | sed 's/\(..........\).*/\1/')
    RDIR="$(date +%s):$(hostname):$B:$C"
    rsync --perms --chmod 755 --recursive report/ "$RHOST:$RHOSTDIR/$RDIR"
}

upload