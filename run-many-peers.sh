#! /usr/bin/env bash
#
# File: run-many-peers.sh
#
# Created:  1 December 2016
#

# treat undefined variable substitutions as errors
set -u
# propagate errors from all parts of pipes
set -o pipefail

host="127.0.0.1"
port="${1:-7000}"
number_of_peers=30
tick_interval=0.1
peer_sync_interval=3

nodes_file=/tmp/many_nodes.txt
rm -f "$nodes_file"
for i in $(seq 1 $number_of_peers); do
    echo "$host:$(($port + $i)):0" >>"$nodes_file"
done

for i in $(seq 1 $number_of_peers); do
    peer_port="$(($port + $i))"
    echo "Starting peer $host:$peer_port"
    {
        coproc "MY_PROC" {
            /tmp/dist/build/challenge/challenge \
                node \
                --with-seed 0 \
                --send-for 10 \
                --wait-for 5 \
                --host "$host" \
                --port "$peer_port" \
                --nodes "$nodes_file" \
                --debug-messages=off \
                --tick-interval "$tick_interval" \
                --peer-synchronization-interval "$peer_sync_interval" "${@}" >&1
        } >&3;
    } 3>&1;
done

wait
# for i in $(seq 1 $number_of_peers); do
#     wait "${COPROC[$i]}"
# done

exit 0

