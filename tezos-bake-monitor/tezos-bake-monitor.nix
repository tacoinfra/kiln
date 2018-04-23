{ mkDerivation, stdenv, text, aeson, snap, lens, time, mtl, safe, optparse-applicative }:
mkDerivation {
  pname = "tezos-bake-monitor";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ text aeson snap lens time mtl safe optparse-applicative ];
  description = "A wrapper for the Tezos baking client which monitors the output and provides statistics via a webserver";
  license = stdenv.lib.licenses.bsd3;
  hydraPlatforms = stdenv.lib.platforms.none;
}
