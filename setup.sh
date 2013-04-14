#! /bin/bash

plugins_dir="$HOME/.emacs.d/plugins"

mkdir -p "$plugins_dir"

if [ ! -d "$plugins_dir/cperl-mode" ] ; then
    git clone git://github.com/jrockway/cperl-mode.git \
        "$plugins_dir/cperl-mode"
fi

if ! grep --quiet 'eterm-set-cwd' "$HOME/.bash_profile" ; then
    cat <<EOF >> "$HOME/.bash_profile"

# Emacs ansi-term directory tracking
# track directory, username, and cwd for remote logons
# From
# http://www.enigmacurry.com/2008/12/26/emacs-ansi-term-tricks/
if [ \$TERM = eterm-color ]; then
    function eterm-set-cwd {
        \$@
        echo -e "\033AnSiTc" \$(pwd)
    }
    
    # set hostname, user, and cwd
    function eterm-reset {
        echo -e "\033AnSiTu" \$(whoami)
        echo -e "\033AnSiTc" \$(pwd)
        echo -e "\033AnSiTh" \$(hostname)
    }
    
    for temp in cd pushd popd; do
        alias \$temp="eterm-set-cwd \$temp"
    done
    
    # set hostname, user, and cwd now
    eterm-reset
fi
EOF
fi
