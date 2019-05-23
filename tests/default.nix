{ obelisk ? (import ../tezos-bake-central/.obelisk/impl {})
, pkgs ? obelisk.reflex-platform.nixpkgs
}: let
  tbp-flextesa = import dep/tbp-flextesa {};
in {
  voting = pkgs.writeScriptBin "voting-test" ''
    #!/usr/bin/env bash
    set -e
    echo 'Starting flextesa voting test... monitor a node via kiln at http://127.0.0.1:20000'
    rm -rf /tmp/kiln_voting_test
    cp -r ${tbp-flextesa.tezos.master.tezos-src}/src/bin_client/test/proto_test_injection /tmp/kiln_voting_test
    chmod -R +w /tmp/kiln_voting_test

    export PATH="${tbp-flextesa.tezos.master.kit + /bin}:$PATH"
    ${tbp-flextesa.tezos.master.kit}/bin/tezos-sandbox voting \
      /tmp/kiln_voting_test/TEZOS_PROTOCOL /tmp/kiln_voting_test/TEZOS_PROTOCOL \
      --base-port=20000 \
      --interactive=true \
      --pause-on-error=true
  '';

  protocol = let
    tzFlextesa = tbp-flextesa.tezos.master;
    tzMultiProto = (import dep/tbp-multi-protocol-mainnet {}).tezos.mainnet;
    oldSuffix = "003-PsddFKi3";
    newSuffix = "004-Pt24m4xi";
    propto = tzMultiProto.tezos-src + /src/proto_004_Pt24m4xi/lib_protocol;
  in pkgs.writeScriptBin "protocol-test" ''
    #!/usr/bin/env bash
    set -Eeuo pipefail

    export PATH="${pkgs.jq + /bin}:$PATH"

    kiln_config_dir="''${1:?Specify path to directory where Kiln\'s \'config\' directory should be written}/config"
    : "''${size:=3}"
    : "''${speed:=10}"
    : "''${block_per_voting_preiod:=40}"

    fail() { "''${___fail:?$1}"; }
    contains_re_group() { [[ $1 =~ $2 ]] && echo "''${BASH_REMATCH[1]}"; }

    if [ -z "''${ledger_uri:-}" ]; then
      connected_ledgers=''$(${tzFlextesa.kit + /bin/tezos-client} -P 0 list connected ledgers 2>/dev/null)
      ledger_uri=$(contains_re_group "$connected_ledgers" '(ledger://[^\"]+)' || fail "Unable to find a connected ledger")
    fi
    echo "> Ledger: $ledger_uri"

    show_ledger=$(${tzFlextesa.kit + /bin/tezos-client} -P 0 show ledger "$ledger_uri" 2>/dev/null)
    pk=$(contains_re_group "$show_ledger" '\* Public Key: ([A-Za-z0-9]+)' || fail "Unable to determine public key for $ledger_uri")
    echo "> PK: $pk"
    pkh=$(contains_re_group "$show_ledger" '\* Public Key Hash: ([A-Za-z0-9]+)' || fail "Unable to determine public key hash for $ledger_uri")
    echo "> PKH: $pkh"

    echo 'Starting tezos-sandbox protocol test...'

    root_path=/tmp/kiln-protocol-test
    rm -rf "$root_path"

    mkdir -p "$kiln_config_dir"
    ${tzFlextesa.kit + /bin/tezos-sandbox} daemons-upgrade ${propto} \
      --add-bootstrap "LBK,$pk,$pkh,$ledger_uri@200_000_000_000" \
      --no-daemons-for LBK \
      --add-external 10000 \
      --generate-kiln "$kiln_config_dir",10000 \
      --clean-kiln-config \
      --time "$speed,$speed" \
      --size "$size" \
      --blocks-per-vot "$block_per_voting_preiod" \
      --pause-on-error true \
      --root-path "$root_path" \
      --waiting-attempts 2000 \
      --tezos-node-binary ${tzMultiProto.kit + /bin/tezos-node} \
      --protocol-hash PsddFKi32cMJ2qPjf43Qv5GDWLDPZb3T3bF6fLKiF5HtvHNU7aP \
      --first-baker-alpha-binary     ${tzMultiProto.kit + /bin/tezos-baker- + oldSuffix} \
      --first-endorser-alpha-binary  ${tzMultiProto.kit + /bin/tezos-endorser- + oldSuffix} \
      --first-accuser-alpha-binary   ${tzMultiProto.kit + /bin/tezos-accuser- + oldSuffix} \
      --second-baker-alpha-binary    ${tzMultiProto.kit + /bin/tezos-baker- + newSuffix} \
      --second-endorser-alpha-binary ${tzMultiProto.kit + /bin/tezos-endorser- + newSuffix} \
      --second-accuser-alpha-binary  ${tzMultiProto.kit + /bin/tezos-accuser- + newSuffix} \
      --tezos-client-binary ${tzMultiProto.kit + /bin/tezos-client} \
      --tezos-admin-client-binary ${tzMultiProto.kit + /bin/tezos-admin-client}
  '';
}
