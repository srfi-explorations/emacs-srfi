#!/bin/sh
set -eu
cd "$(dirname "$0")"
echo "Entering directory '$PWD'"
set -x
curl --fail --silent --show-error -o srfi-data.scm https://raw.githubusercontent.com/scheme-requests-for-implementation/srfi-common/master/admin/srfi-data.scm
