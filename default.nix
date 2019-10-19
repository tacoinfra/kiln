{ system ? builtins.currentSystem
, obelisk ? (import tezos-bake-central/.obelisk/impl { inherit system; })
, pkgs ? obelisk.reflex-platform.nixpkgs
}:
let
  inherit (obelisk.reflex-platform) hackGet;
  obApp = distMethod: import ./tezos-bake-central { inherit system distMethod tezosScopedKit; supportGargoyle = false; };
  obAppGargoyle = distMethod: import ./tezos-bake-central { inherit system distMethod tezosScopedKit; supportGargoyle = true; };

  distroMethods = {
    source = null;
    docker = "docker";
    linuxPackage = "linux-package";
  };

  tezos-baking-platform = import dep/tezos-baking-platform {};

  tezosScopedKit = import ./tezos-bake-central/scoped-tzkits.nix {
    inherit pkgs tezos-baking-platform;
  };

  dockerExe = let exe = (obApp distroMethods.docker).linuxExe; in pkgs.runCommand "dockerExe" {} ''
    mkdir "$out"

    cp '${exe}/backend' "$out/backend"
    cp -r '${exe}/static.assets' "$out/static.assets"

    mkdir "$out/frontend.jsexe.assets"
    cp -r '${exe}/frontend.jsexe.assets'/*all.js "$out/frontend.jsexe.assets"
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
    '';
    bakeCentralEntrypoint = pkgs.dockerTools.shellScript "entrypoint.sh" ''
      set -ex

      mkdir -p /var/run/bake-monitor
      ln -sft /var/run/bake-monitor '${dockerExe}'/*

      cd /var/run/bake-monitor
      exec ./backend "$@"
    '';
  in pkgs.dockerTools.buildImage {
    name = "tezos-bake-monitor";
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

  upgradeKilnVM =
    let
      resultStorePathFile = "https://s3.eu-west-3.amazonaws.com/tezos-kiln/vm/master-store-path";
    in pkgs.writeScriptBin "upgrade-kiln" ''
        #!/usr/bin/env bash
        set -e
        if [[ $# -eq 0 ]] ; then
           echo "Downloading latest Kiln path"
           echo "Fetching ${resultStorePathFile}"
           export KILN_VM_STORE_PATH=`curl '${resultStorePathFile}'`
        else
           echo "Using the user supplied store path: $1"
           export KILN_VM_STORE_PATH=$1
        fi
        echo "Downloading Kiln"
        nix copy --from 's3://tezos-nix-cache?region=eu-west-3' $KILN_VM_STORE_PATH
        echo "Installing Kiln"
        sudo nix-env -p /nix/var/nix/profiles/system --set $KILN_VM_STORE_PATH
        sudo /nix/var/nix/profiles/system/bin/switch-to-configuration switch
      '';

  kilnVMConfig = (import (pkgs.path + /nixos) {
    configuration = {
      imports = [
        ./virtualbox-image.nix
      ];
      users.users.kiln = {
        isNormalUser = true;
        description = "Kiln account";
        extraGroups = [ "wheel" ];
        password = "";
        uid = 1000;
      };
      services.xserver = {
        enable = true;
        displayManager.sddm.enable = true;
        displayManager.sddm.autoLogin = {
          enable = true;
          relogin = true;
          user = "kiln";
        };
        desktopManager.plasma5.enable = true;
        libinput.enable = true; # for touchpad support on many laptops
      };

      security.sudo.wheelNeedsPassword = false;
      networking.firewall.enable = false;
      environment.systemPackages = [ upgradeKilnVM pkgs.firefox tezos-baking-platform.tezos.mainnet.kit ];
      services.udev.extraRules = ''
        SUBSYSTEMS=="usb", ATTRS{idVendor}=="2581", ATTRS{idProduct}=="1b7c", MODE="0660", GROUP="users"
        SUBSYSTEMS=="usb", ATTRS{idVendor}=="2581", ATTRS{idProduct}=="2b7c", MODE="0660", GROUP="users"
        SUBSYSTEMS=="usb", ATTRS{idVendor}=="2581", ATTRS{idProduct}=="3b7c", MODE="0660", GROUP="users"
        SUBSYSTEMS=="usb", ATTRS{idVendor}=="2581", ATTRS{idProduct}=="4b7c", MODE="0660", GROUP="users"
        SUBSYSTEMS=="usb", ATTRS{idVendor}=="2581", ATTRS{idProduct}=="1807", MODE="0660", GROUP="users"
        SUBSYSTEMS=="usb", ATTRS{idVendor}=="2581", ATTRS{idProduct}=="1808", MODE="0660", GROUP="users"
        SUBSYSTEMS=="usb", ATTRS{idVendor}=="2c97", ATTRS{idProduct}=="0000", MODE="0660", GROUP="users"
        SUBSYSTEMS=="usb", ATTRS{idVendor}=="2c97", ATTRS{idProduct}=="0001", MODE="0660", GROUP="users"
        SUBSYSTEMS=="usb", ATTRS{idVendor}=="2c97", ATTRS{idProduct}=="0004", MODE="0660", GROUP="users"
      '';
      nix.binaryCaches = [ "https://cache.nixos.org/" "https://nixcache.reflex-frp.org" "https://s3.eu-west-3.amazonaws.com/tezos-nix-cache" ];
      nix.binaryCachePublicKeys = [ "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=" "obsidian-tezos-kiln:WlSLNxlnEAdYvrwzxmNMTMrheSniCg6O4EhqCHsMvvo=" ];

      nixpkgs = { localSystem.system = "x86_64-linux"; };
      virtualbox = {
        baseImageSize = 64 * 1024; # in MiB
        memorySize = 12 * 1024; # in MiB
        vmDerivationName = "kiln-vm";
        vmName = "Kiln VM";
        vmFileName = "kiln-vm.ova";
        extraDisk = {
          label = "kiln-data";
          mountPoint = "/home/kiln/app";
          size = 500 * 1024;
        };
      };
      systemd.services.setupkiln = {
        wantedBy = [ "multi-user.target" ];
        after = [ "home-kiln-app.mount" "network-online.target" ];
        wants = [ "network-online.target" ];
        # Change the ownership of the kiln folder (root of the other disk)
        script = ''
          chown -R kiln:users /home/kiln/app
        '';
        serviceConfig = {
          User = "root";
          Type = "oneshot";
        };
      };
      systemd.services.kiln = {
        wantedBy = [ "multi-user.target" ];
        after = [ "setupkiln.service" ];
        restartIfChanged = true;
        preStart = ''
          ln -sft . '${(obAppGargoyle distroMethods.source).exe}'/*
          mkdir -p log
        '';
        script = ''
          exec ./backend
        '';
        serviceConfig = {
          User = "kiln";
          WorkingDirectory = "/home/kiln/app";
          Restart = "always";
          RestartSec = 5;
        };
      };
    };
  });

  installKiln = pkgs.writeScriptBin "install-kiln" ''
    #!/usr/bin/env bash
    set -e
    KILN_INSTALL_PATH="''${1:-app}"
    echo "Installing Kiln in directory: $KILN_INSTALL_PATH"
    mkdir -p "$KILN_INSTALL_PATH"
    ln -sf '${(obAppGargoyle distroMethods.source).exe}'/* "$KILN_INSTALL_PATH"
    echo "Install Complete!"
    echo "'cd \"$KILN_INSTALL_PATH\"' and run './backend' to run kiln with default settings."
  '';

in (obApp distroMethods.source) // {
  tests = import ./tests { inherit obelisk pkgs; };

  inherit pkgs dockerExe kilnVMConfig dockerImage installKiln;

  server = import ./public-nodes {
    inherit pkgs obelisk;
    app = obApp distroMethods.source;
  };

  kilnVM = kilnVMConfig.config.system.build.virtualBoxOVA;
  kilnVMSystem = kilnVMConfig.system;

  kiln-debian = (import ./linux-distros.nix {
    inherit pkgs;
    obApp = obAppGargoyle distroMethods.linuxPackage;
    nodeKit = tezosScopedKit;
    pkgName = "kiln";
    version = "0.7.2"; # TODO: Calculate this
  }).kiln-debian;
}
