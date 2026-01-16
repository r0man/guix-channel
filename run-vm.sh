#!/bin/sh
# Run the Cortex XDR VM Test Environment
#
# Usage:
#   ./run-vm.sh              # Run with GUI
#   ./run-vm.sh -nographic   # Run headless (Ctrl-A X to exit)
#   ./run-vm.sh -m 2048      # Run with 2GB RAM
#   ./run-vm.sh -nic user,hostfwd=tcp::10022-:22  # With SSH forwarding

/gnu/store/f40ir4k2m4q234vxzlp9hbbq1xim9y22-run-vm.sh "$@"
