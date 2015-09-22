#!/usr/bin/env sh

if [ -z "$EMACS" ] ; then
    EMACS="emacs"
fi

$EMACS -batch -l ob-http.el -l tests.el -f ert-run-tests-batch-and-exit
