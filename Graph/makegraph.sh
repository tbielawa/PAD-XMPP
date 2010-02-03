#!/bin/bash

# Requires Graphviz. Get it at www.graphviz.org or from your local
# package tree.

echo "Generating ConnectionStates.png... this may take a while"

dot -Tpng -o ConnectionStates.png Login.gv
