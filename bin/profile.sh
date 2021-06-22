#!/usr/bin/env sh

EMACS_BIN="emacs"
PROFILER="~/.emacs.d/local/packages/profile-dotemacs/profile-dotemacs.el"
FUNCTION="profile-dotemacs"

$EMACS_BIN -Q -l $PROFILER -f $FUNCTION
