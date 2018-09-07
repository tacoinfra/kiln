{ pkgs, ... }:
let
  origExe = (import ./. { supportGargoyle = false; }).exe;
in {
  releaseExe = pkgs.runCommand "releaseExe" {} ''
    mkdir "$out"
    cp '${origExe}/backend' "$out/backend"
    cp -r '${origExe}/static' "$out/static"
    cp -r '${origExe}/config' "$out/config"

    mkdir "$out/frontend.jsexe"
    cp '${origExe}/frontend.jsexe/all.js' "$out/frontend.jsexe/all.js"
  '';
}
