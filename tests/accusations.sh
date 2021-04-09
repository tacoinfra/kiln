#!/usr/bin/env bash

########################
# REPLACE ALL OF THESE #
########################
sandbox_binary="/home/eod/work/tezos/tezos-sandbox"
tezos_node_binary="/home/eod/work/tezos/tezos-node"
tezos_accuser_alpha_binary="/home/eod/work/tezos/tezos-accuser-alpha"
tezos_client_binary="/home/eod/work/tezos/tezos-client"

kiln_config_dir="${1:?Specify path to directory where Kiln\'s \'config\' directory should be written}/config"

echo 'Starting tezos-sandbox accusations test...'

root_path=/tmp/accusing-test
rm -rf "$root_path"

test="simple-double-baking"
if [ $# -eq 2 ]
  then
    test="${2}"
fi

mkdir -p "$kiln_config_dir"
"$sandbox_binary" accusations $test \
  --generate-kiln "$kiln_config_dir",10000 \
  --clean-kiln-config \
  --pause-on-error true \
  --interactive true \
  --pause-at-end true \
  --starting-level 50 \
  --root-path "$root_path" \
  --tezos-node-binary "$tezos_node_binary" \
  --tezos-accuser-alpha-binary "$tezos_accuser_alpha_binary" \
  --tezos-client-binary "$tezos_client_binary"
