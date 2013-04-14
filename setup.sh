#! /bin/bash

plugins_dir="$HOME/.emacs.d/plugins"

mkdir -p "$plugins_dir"

if [ ! -d "$plugins_dir/cperl-mode" ] ; then
    git clone git://github.com/jrockway/cperl-mode.git \
        "$plugins_dir/cperl-mode"
fi
