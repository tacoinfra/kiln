{ pkgs }:
with pkgs;
let
  outer-version = "v12.0-3";
  macos_version = "catalina";
    tezos-admin-client = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-admin-client-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "0kz9adcv2dcw57rhl59idcv9dhfz5532knjy9khh4xnrpkb57dzb";
    };
    tezos-baker-011-PtHangz2 = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-baker-011-PtHangz2-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "1qb3w9p42k2nkvb022q3ssnydh3sag4qg593zq1sgn0p2gca581n";
    };
    tezos-baker-012-Psithaca = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-baker-012-Psithaca-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "1bbvpbv4xj4mvi1i5yhfwmz9a1grhpfd13jgmxs7y9vsh1i5b8hi";
    };
    tezos-client = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-client-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "04mb3mhmpffd2k2n3gl78s5xrlsxrs4fy2py5n5dfzgx6jvkfvv9";
    };
    tezos-endorser-011-PtHangz2 = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-endorser-011-PtHangz2-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "14qbx26v8x3gnjba1jf5nc1lw3by8mssg3vzmnhmq5cz8b5vj1yx";
    };
    tezos-node = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-node-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "1nmp3b5wfjw0wqagnv8qpjxicsi6mhz7wwd0qfi0ggrwqm6bf194";
    };

in stdenv.mkDerivation rec {
  name = "tezos-${outer-version}";

  phases = [ "installPhase" ];

  installPhase = ''
  mkdir -p $out/bin

  cp ${tezos-admin-client}/${outer-version}/bin/tezos-admin-client $out/bin/tezos-admin-client
  chmod +x $out/bin/tezos-admin-client

  cp ${tezos-baker-011-PtHangz2}/${outer-version}/bin/tezos-baker-011-PtHangz2 $out/bin/tezos-baker-011-PtHangz2
  chmod +x $out/bin/tezos-baker-011-PtHangz2

  cp ${tezos-baker-012-Psithaca}/${outer-version}/bin/tezos-baker-012-Psithaca $out/bin/tezos-baker-012-Psithaca
  chmod +x $out/bin/tezos-baker-012-Psithaca

  cp ${tezos-client}/${outer-version}/bin/tezos-client $out/bin/tezos-client
  chmod +x $out/bin/tezos-client

  cp ${tezos-endorser-011-PtHangz2}/${outer-version}/bin/tezos-endorser-011-PtHangz2 $out/bin/tezos-endorser-011-PtHangz2
  chmod +x $out/bin/tezos-endorser-011-PtHangz2

  cp ${tezos-node}/${outer-version}/bin/tezos-node $out/bin/tezos-node
  chmod +x $out/bin/tezos-node
  '';
}
