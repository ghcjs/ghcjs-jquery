#!/bin/bash

stack build
echo "Copy all.js..."
cp $(stack path --dist-dir)/build/ghcjs-jquery-example/ghcjs-jquery-example.jsexe/all.js example/
echo "All done."
