#! /usr/bin/env bash
#
# File: run-peer.sh
#
# Created: Thursday, 24 November 2016
#

# treat undefined variable substitutions as errors
set -u
# propagate errors from all parts of pipes
set -o pipefail

#tick_interval=250000
tick_interval=1
peer_sync_interval=2
#host="10.10.10.2"
host="127.0.0.1"
port="${1:-8888}"
shift 1

/tmp/dist/build/challenge/challenge \
    node \
    --with-seed 0 \
    --send-for 10 \
    --wait-for 5 \
    --host "$host" \
    --port "$port" \
    --nodes nodes.txt \
    --debug-messages=off \
    --tick-interval "$tick_interval" \
    --peer-synchronization-interval "$peer_sync_interval" "${@}" | tee "/tmp/node-$port"

exit 0

