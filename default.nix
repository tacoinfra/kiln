{ system ? if builtins.currentSystem == "aarch64-darwin" then "x86_64-darwin" else builtins.currentSystem
, obelisk ? (import tezos-bake-central/.obelisk/impl { inherit system; })
, pkgs ? obelisk.reflex-platform.nixpkgs
  # can be set to "SIMPLE", "ADVANCED" or null
, closure-compiler-setting ? null
}:
let
  inherit (obelisk.reflex-platform) hackGet;

  obApp = source: distMethod: system_: import ./tezos-bake-central {
    inherit distMethod;
    system = system_;
    tezosScopedKit = tezosScopedKit source system_;
    supportGargoyle = false;
    inherit closure-compiler-setting source;
  };
  obAppGargoyle = source: distMethod: system_: import ./tezos-bake-central {
    inherit distMethod;
    system = system_;
    tezosScopedKit = tezosScopedKit source system_;
    supportGargoyle = true;
    inherit closure-compiler-setting source;
  };

  zcash = import dep/zcash { inherit pkgs; };

  distroMethods = {
    source = null;
    docker = "docker";
    linuxPackage = "linux-package";
  };

  tezosScopedKit = source: system_:  import dep/platform-specific-binaries.nix { system = system_ ;inherit pkgs source;};

  dockerExe = let exe = (obApp false distroMethods.docker "x86_64-linux").linuxExe; in pkgs.runCommand "dockerExe" {} ''
    mkdir "$out"

    cp '${exe}/backend' "$out/backend"
    cp -r '${exe}/static.assets' "$out/static.assets"

    mkdir "$out/frontend.jsexe.assets"
    cp -r '${exe}/frontend.jsexe.assets'/*all.js "$out/frontend.jsexe.assets"

    # mkdir -p /.zcash-params
    # ln -sf '${zcash}/zcash-params/'* /.zcash-params

  '';
  dockerImage = let
    bakeCentralSetupScript = pkgs.dockerTools.shellScript "dockersetup.sh" ''
      set -ex

      ${pkgs.dockerTools.shadowSetup}
      echo 'nobody:x:99:99:Nobody:/:/sbin/nologin' >> /etc/passwd
      echo 'nobody:*:17416:0:99999:7:::'           >> /etc/shadow
      echo 'nobody:x:99:'                          >> /etc/group
      echo 'nobody:::'                             >> /etc/gshadow

      mkdir -p    /var/run/bake-monitor
      chown 99:99 /var/run/bake-monitor

      mkdir -p /.zcash-params
      ln -sf '${zcash}/zcash-params/'* /.zcash-params
      ln -s ${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt /etc/ssl/certs/ca-certificates.crt
    '';
    bakeCentralEntrypoint = pkgs.dockerTools.shellScript "entrypoint.sh" ''
      set -ex

      mkdir -p /var/run/bake-monitor
      ln -sft /var/run/bake-monitor '${dockerExe}'/*

      cd /var/run/bake-monitor
      exec ./backend "$@"
    '';
  in pkgs.dockerTools.buildImage {
    name = "kiln";
    contents = [ pkgs.iana-etc pkgs.cacert ];
    runAsRoot = bakeCentralSetupScript;
    keepContentsDirlinks = true;
    config = {
     Env = [
        ("PATH=" + builtins.concatStringsSep(":")([
          "${pkgs.stdenv.shellPackage}/bin"
          "${pkgs.coreutils}/bin"
        ]))
      ];
      Expose = 8000;
      Entrypoint = [bakeCentralEntrypoint];
      User = "99:99";
    };
  };

  installKiln = pkgs.writeScriptBin "install-kiln" ''
    #!/usr/bin/env bash
    set -e
    KILN_INSTALL_PATH="''${1:-app}"
    echo "Installing Kiln in directory: $KILN_INSTALL_PATH"
    mkdir -p "$KILN_INSTALL_PATH"
    ln -sf '${(obAppGargoyle true distroMethods.source system).exe}'/* "$KILN_INSTALL_PATH"
    mkdir -p $HOME/.zcash-params
    ln -sf '${zcash}'/zcash-params/* $HOME/.zcash-params
    echo "Install Complete!"
    echo "'cd \"$KILN_INSTALL_PATH\"' and run './backend' to run kiln with default settings."
  '';

in (obApp false distroMethods.source system) // {
  tests = import ./tests { inherit obelisk pkgs; };

  inherit pkgs dockerExe dockerImage installKiln;

  kiln-debian = (import ./linux-distros.nix {
    inherit pkgs;
    obApp = obAppGargoyle false distroMethods.linuxPackage "x86_64-linux";
    nodeKit = tezosScopedKit false "x86_64-linux";
    pkgName = "kiln";
    version = "0.16.1"; # TODO: Calculate this
    inherit zcash;
  }).kiln-debian;

  tezosKit = tezosScopedKit false system;

}
