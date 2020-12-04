#!/bin/bash
set -e -x

RHOST="uwplse.org"
RHOSTDIR="/var/www/rival"

upload () {
    DIR="$(date +%s)"
    rsync --perms --chmod 755 --recursive report/ "$RHOST:$RHOSTDIR/$DIR"
}

upload "$1"