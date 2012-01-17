#!/bin/sh


emacs --batch \
      --eval "(add-to-list 'load-path (expand-file-name \"~/.emacs.d\"))" \
    --eval "(add-to-list 'load-path (expand-file-name \"~/.emacs.d/extensions\"))" \
    --eval "(add-to-list 'load-path (expand-file-name \"~/.emacs.d/extensions/yasnippet\"))" \
    --eval "(add-to-list 'load-path (expand-file-name \"~/.emacs.d/extensions/auto-complete\"))" \
    --eval "(add-to-list 'load-path (expand-file-name \"~/.emacs.d/extensions/eproject\"))" \
    --eval "(batch-byte-compile-if-not-done)" *.el extensions/*.el extensions/*/*.el