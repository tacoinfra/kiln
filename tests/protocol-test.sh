#!/usr/bin/env bash

kiln_config_dir="${1:?Specify path to directory where Kiln\'s \'config\' directory should be written}/config_test"
    : "${size:=3}"
    : "${speed:=10}"
    : "${blocks_per_voting_period:=24}"

fail() { "''${___fail:?$1}"; }
contains_re_group() { [[ $1 =~ $2 ]] && echo "${BASH_REMATCH[1]}"; }

# if [ -z "''${ledger_uri:-}" ]; then
#           connected_ledgers=''$(/home/eod/work/tezos/tezos-client list connected ledgers 2>/dev/null)
#           ledger_uri=$(contains_re_group "$connected_ledgers" '(ledger://[^\"]+)' || fail "Unable to find a connected ledger")
# fi

ledger_uri="ledger://buttery-quail-oblong-seal/bip25519/0h/0h"

echo "> Ledger: $ledger_uri"

show_ledger=$(/home/eod/Downloads/latest/tezos-client show ledger "$ledger_uri" 2>/dev/null)
pk=$(contains_re_group "$show_ledger" '\* Public Key: ([A-Za-z0-9]+)' || fail "Unable to determine public key for $ledger_uri")
echo "> PK: $pk"
pkh=$(contains_re_group "$show_ledger" '\* Public Key Hash: ([A-Za-z0-9]+)' || fail "Unable to determine public key hash for $ledger_uri")
echo "> PKH: $pkh"

echo 'Starting tezos-sandbox protocol test...'

root_path=/tmp/kiln-protocol-test
rm -rf "$root_path"

mkdir -p "$kiln_config_dir"

oldProtoHash="PtEdo2ZkT9oKpimTah6x2embF25oss54njMuPzkJTEi5RqfdZFA"
oldSuffix="008-PtEdo2Zk"
newSuffix="009-PsFLoren"

proposalProtocolLib="/home/eod/work/tezos/src/proto_009_PsFLoren/lib_protocol/TEZOS_PROTOCOL"

########################
# REPLACE ALL OF THESE #
########################
first_baker_alpha_binary="/home/eod/Downloads/latest/tezos-baker-$oldSuffix"
first_endorser_alpha_binary="/home/eod/Downloads/latest/tezos-endorser-$oldSuffix"
first_accuser_alpha_binary="/home/eod/Downloads/latest/tezos-accuser-$oldSuffix"
second_baker_alpha_binary="/home/eod/Downloads/latest/tezos-baker-$newSuffix"
second_endorser_alpha_binary="/home/eod/Downloads/latest/tezos-endorser-$newSuffix"
second_accuser_alpha_binary="/home/eod/Downloads/latest/tezos-accuser-$newSuffix"
tezos_client_binary="/home/eod/Downloads/latest/tezos-client"
tezos_admin_client_binary="/home/eod/Downloads/latest/tezos-admin-client"

/home/eod/Downloads/latest/tezos-sandbox daemons-upgrade $proposalProtocolLib \
   --interactive true \
   --add-bootstrap "LBK,$pk,$pkh,$ledger_uri@200_000_000_000" \
   --no-daemons-for LBK \
   --add-external 10000 \
   --generate-kiln "$kiln_config_dir",10000 \
   --clean-kiln-config \
   --time-between-blocks "$speed" \
   --size "$size" \
   --blocks-per-vot "$blocks_per_voting_period" \
   --pause-on-error true \
   --root-path "$root_path" \
   --waiting-attempts 2000 \
   --tezos-node-binary /home/eod/Downloads/latest/tezos-node \
   --protocol-hash ${oldProtoHash} \
   --first-baker-alpha-binary     "$first_baker_alpha_binary" \
   --first-endorser-alpha-binary  "$first_endorser_alpha_binary" \
   --first-accuser-alpha-binary   "$first_accuser_alpha_binary" \
   --second-baker-alpha-binary    "$second_baker_alpha_binary" \
   --second-endorser-alpha-binary "$second_endorser_alpha_binary" \
   --second-accuser-alpha-binary  "$second_accuser_alpha_binary" \
   --tezos-client-binary "$tezos_client_binary" \
   --tezos-admin-client-binary "$tezos_admin_client_binary"
