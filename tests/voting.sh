#!/usr/bin/env bash

echo 'Starting flextesa voting test... monitor a node via kiln at http://127.0.0.1:20000'
rm -rf /tmp/kiln_voting_test

export PATH=/home/eod/work/tezos:$PATH

TEZOSPATH="/home/eod/work/tezos"

cp -r $TEZOSPATH/src/bin_client/test/proto_test_injection /tmp/kiln_voting_test

sandbox_binary="$TEZOSPATH/tezos-sandbox"

chmod -R +w /tmp/kiln_voting_test

"$sandbox_binary" voting \
      /tmp/kiln_voting_test/TEZOS_PROTOCOL /tmp/kiln_voting_test/TEZOS_PROTOCOL \
      --base-port=20000 \
      --interactive=true \
      --winning-client-is-clueless \
      --pause-on-error=true
