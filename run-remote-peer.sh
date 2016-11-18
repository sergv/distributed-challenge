#! /usr/bin/env bash
#
# File: run-remote-peer.sh
#
# Created: Thursday, 24 November 2016
#

# treat undefined variable substitutions as errors
set -u
# propagate errors from all parts of pipes
set -o pipefail

port="${1:-7003}"
tick_interval=2000000
shift 1
ssh fileserver /tmp/challenge node --with-seed 0 --send-for 10 --wait-for 5 --host 10.10.10.3 --port "$port" --nodes /tmp/nodes.txt --tick-interval "$tick_interval" "${@}"

exit 0

