#!/usr/bin/env bash
WISP="${1}"

function die () {
    echo $1
    exit 1
}

${WISP} -c 'display 1' | grep -q 1 || die 'failed to display output'
