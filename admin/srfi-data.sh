#!/bin/sh
set -eu
cd "$(dirname "$0")"
echo "Entering directory '$PWD'"
set -x
curl --fail --silent --show-error -o srfi-data.scm https://raw.githubusercontent.com/scheme-requests-for-implementation/srfi-common/master/admin/srfi-data.scm
${EMACS:-emacs} -Q -batch -L . -l srfi-data-convert -f srfi-data-convert
mv -f srfi-data.el.new ../srfi-data.el
git commit --message "Update SRFI data" ../srfi-data.el
git --no-pager show
