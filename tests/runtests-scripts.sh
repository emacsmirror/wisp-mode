#!/usr/bin/env bash
set -x
WISP="${1}"
BUILDDIR="${2}"
SRCDIR="${3}"

function die () {
    echo $1
    exit 1
}

${WISP} -L "${SRCDIR}" -C "${BUILDDIR}" -c 'display 1' | grep -q 1 || die 'failed to display output'
