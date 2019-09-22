#!/bin/sh
set -eu
cd "$(dirname "$0")"
echo "Entering directory '$PWD'"
set -x
${EMACS:-emacs} -Q -batch -L . -l srfi-data-convert -f srfi-data-convert
