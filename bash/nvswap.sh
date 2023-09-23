#!/bin/bash

# rfr: 20230328-Neovim-switch-scripti <url:file:///~/prj/study/logseq-study/pages/20230328-Neovim-switch-scripti.md#r=g14154>

rm -rf ~/.local/share/nvim/lazy
rm -rf "$HOME/.config/nvim"
ln -s "$1" "$HOME/.config/nvim"

