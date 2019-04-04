{ system ? builtins.currentSystem
, obelisk ? (import tezos-bake-central/.obelisk/impl { inherit system; })
, pkgs ? obelisk.reflex-platform.nixpkgs
}:
let
  obApp = distMethod: import ./tezos-bake-central { inherit system distMethod; supportGargoyle = false; };

  tezos-bake-platform = import dep/public-nodes/tezos-baking-platform {};
  tezos = tezos-bake-platform.tezos;

  nodeConfigOptions = {
    zeronet = {
      network = "zeronet";
      p2pPort = 29732;
      rpcPort = 28732;
      tzKit = tezos.zeronet.kit;
      monitorPort = 8002;
      histMode = "archive";
    };
    alphanet = {
      network = "alphanet";
      p2pPort = 19732;
      rpcPort = 18732;
      tzKit = tezos.alphanet.kit;
      monitorPort = 8001;
    };
    mainnet = {
      network = "mainnet";
      p2pPort = 9732;
      rpcPort = 8732;
      tzKit = tezos.mainnet.kit;
      monitorPort = 8000;
    };
  };

  mkTezosNodeServiceModule = { p2pPort, rpcPort, network, tzKit, histMode ? null, ... }: {...}:
    let serviceName = "${network}-node"; user = serviceName; group = user;
    in {
      networking.firewall.allowedTCPPorts = [p2pPort];
      systemd.services.${serviceName} = {
        wantedBy = [ "multi-user.target" ];
        after = [ "network.target" ];
        restartIfChanged = true;
        script = let dataDir = "$HOME/.tezos-node"; in ''
          if [ ! -f "${dataDir}/identity.json" ]; then
            ${tzKit}/bin/tezos-node identity generate --data-dir "${dataDir}"
          fi
          exec ${tzKit}/bin/tezos-node run --rpc-addr '127.0.0.1:${toString rpcPort}' --net-addr ':${toString p2pPort}' --data-dir "${dataDir}" ${if histMode == null then "" else "--history-mode ${histMode}"}
        '';
        serviceConfig = {
          User = user;
          KillMode = "process";
          WorkingDirectory = "~";
          Restart = "always";
          RestartSec = 5;
          MemoryHigh = "7G";
          MemoryMax = "12G";
        };
      };
      users = {
        users.${user} = {
          description = "${user} service";
          home = "/var/lib/${user}";
          createHome = true;
          isSystemUser = true;
          group = group;
        };
        groups.${group} = {};
      };
  };

  mkMonitorModule =
    { enableHttps
    , routeHost
    , network
    , monitorName ? "${network}-monitor"
    , dbname ? monitorName
    , user ? monitorName
    , rpcPort
    , monitorPort
    , appConfig
    , version
    , ...}@args: {config, ...}: {
      imports = [
        (obelisk.serverModules.mkObeliskApp (args // {
          exe = (obApp null).linuxExeConfigurable appConfig version;
          name = monitorName;
          user = user;
          internalPort = monitorPort;
          baseUrl = null;
          backendArgs = pkgs.lib.concatStringsSep " " [
            "--network='${network}'"
            "--serve-node-cache=yes"
            "--pg-connection='dbname=${dbname}'"
            "--check-for-upgrade=no"
            "--nodes='http://127.0.0.1:${toString rpcPort}'"
            "--email-from='${monitorName}@obsidian.systems'"
            "--network-gitlab-project-id='${pkgs.lib.fileContents ./tezos-bake-central/config/network-gitlab-project-id}'"
            "--"
            "--port=${toString monitorPort}"
          ];
        }))
      ];

      systemd.services.${monitorName} = {
        serviceConfig = {
          MemoryHigh = "2G";
          MemoryMax = "12G";
        };
      };

      services.nginx = {
        virtualHosts.${routeHost} = {
          locations = {
            "/api" = {
              proxyPass = "http://127.0.0.1:${toString monitorPort}/api";
            };
          };
        };
      };

      environment.systemPackages = [ config.services.postgresql.package ];
      services.postgresql = {
        enable         = true;
        authentication = ''
          #      #db          #user     #auth-method  #auth-options
          local  "${dbname}"  "${user}" peer
        '';
      };
    }
  ;

  opsEmail = "elliot.cameron@obsidian.systems";

  syslog-ngModule = { opsEmail ? null }: {...}: {
    services.openssh.extraConfig = ''
      MaxAuthTries 3
    '';

    services.journald.rateLimitBurst = 0;

    services.syslog-ng.enable = opsEmail != null && opsEmail != "";
    services.syslog-ng.extraConfig = ''
      source s_journald {
        systemd-journal(prefix(".SDATA.journald."));
      };

      filter f_errors { "$LEVEL_NUM" lt "4" };
      filter f_sshd_attacks_liberal {
        not (
          # and abuse-looking errors
          message("PAM service\(sshd\) ignoring max retries")
        )
      };
      filter f_sshd_attacks {
        not (
          # match program
              (
                "''${.SDATA.journald.SYSLOG_IDENTIFIER}" eq "sshd"
              or "''${PROGRAM}" eq "sshd"
          ) and (
              # and abuse-looking errors
              message("^PAM service\(sshd\) ignoring max retries")
            or message("^error: maximum authentication attempts exceeded for")
            or message("^error: PAM: Authentication failure for illegal user")
            or message("^error: Received disconnect from")
          )
        )
      };
      template ops_friendlyname "$HOST Admin" ;

      destination d_smtp {
        smtp(
          host("mail.obsidian.systems")
          port(2525)
          from("syslog-ng alert service" "noreply@obsidian.systems")
          to(ops_friendlyname "${opsEmail}")
          subject("[ALERT] $LEVEL $HOST $PROGRAM $MSG")
          body("$MSG\\n$SDATA\n")
        );
      };

      log {
        source(s_journald);
        filter(f_sshd_attacks_liberal);
        filter(f_sshd_attacks);
        filter(f_errors);
        destination(d_smtp);
      };
    '';
  };

  usersModule = {config, pkgs, ...}: {
    users.users = {
      "elliot.cameron" = {
        description = "Elliot Cameron";
        isNormalUser = true;
        openssh.authorizedKeys.keys = [
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPsrDJrZRXpa6f5g+dfysfU4R/YSqOKRzu2zR99k9izE elliot@nixos"
        ];
        extraGroups = ["wheel"];
      };
      dbornside = {
        description = "Dan Bornside";
        isNormalUser = true;
        openssh.authorizedKeys.keys = [
          "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQD0ijHT/18Dbjq26bnh2KYndp5vMQXkdD66064xLvpqOVMaPDm9I2QYsEAwGdatnriAFLUhPVkTWTga7KIA37Z9XaTMhKRJb4koT4osIz1ikbVvbUsrLquRC1gulrMRKHjaA3QlPOnOy7pvIW6DYyl9vDhl143X8/7riW9O+pw5OJM8HBKxwIzNZ1XstE3E6VOXnhskU18EBDEqJBE+6+36RBOiGfeDfsV45O1ov4fEAwspV7qIbVirrLnqOyvNfPOCBAnhL5vK6C5Horci1u7hyHHCnV57UoF/fJzYTRKSCeObUNHrhyAlhMstqPhb9qCrtFRDKyBkvmGzntwi/eSv dbornside@localhost.localdomain"
        ];
        extraGroups = ["wheel"];
      };
    };
  };

  dockerExe = let exe = (obApp "docker").linuxExe; in pkgs.runCommand "dockerExe" {} ''
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


  runKilnExe = app:
    let
      # Somehow these desktop icons dont work
      runKilnDesktopItem = pkgs.makeDesktopItem {
        name = "run-kiln-desktop-item";
        desktopName = "Run Kiln";
        genericName = "Initiate Kiln directory and run in terminal";
        icon = "utilities-terminal";
        terminal = "true";
        exec = "bash run-kiln";
        categories = "Application";
      };
      openKilnDesktopItem = pkgs.makeDesktopItem {
        name = "open-kiln-desktop-item";
        desktopName = "Open Kiln";
        genericName = "Open Kiln in Firefox";
        icon = "firefox";
        exec = "firefox http://127.0.0.1:8000/";
        categories = "Application;WebBrowser";
      };
      script = pkgs.writeScriptBin "run-kiln" ''
        #!/run/current-system/sw/bin/bash
        if [ ! -d "/home/demo/kiln" ]
        then
            mkdir "/home/demo/kiln"
            ln -s ${app.exe}/* "/home/demo/kiln/"
        fi
        cd "/home/demo/kiln"
        /home/demo/kiln/backend --pg-connection='dbname=kiln-db'
      '';
    in pkgs.stdenv.mkDerivation {
      name = "run-kiln";
      buildInputs = [ app.exe ];
      src = script;

      # This was adapted from some other derivation in nixpkgs
      # but the icons dont show on Desktop/start menu
      installPhase = ''
        mkdir -p $out/share/applications
        mv * $out/
        cp ${runKilnDesktopItem}/share/applications/* $out/share/applications
        cp ${openKilnDesktopItem}/share/applications/* $out/share/applications
      '';

    };

in (obApp null) // {
  inherit pkgs dockerExe runKilnExe dockerImage;
  server = args@{ hostName, adminEmail, routeHost, enableHttps, config, version, ... }:
    let
      network =
        if pkgs.lib.strings.hasPrefix "zeronet" hostName then "zeronet" else
        if pkgs.lib.strings.hasPrefix "alphanet" hostName then "alphanet" else
        "mainnet";
      nodeConfig = nodeConfigOptions.${network};
      nixos = import (pkgs.path + /nixos);
    in nixos {
      system = "x86_64-linux";
      configuration = {
        imports = [
          (obelisk.serverModules.mkBaseEc2 args)
          (mkTezosNodeServiceModule nodeConfig)
          (mkMonitorModule (args // nodeConfig // {
              appConfig = config;
              version = version;
            })
          )
          (syslog-ngModule {
            opsEmail = if pkgs.lib.strings.hasPrefix "zeronet" hostName then null else opsEmail;
          })
          usersModule
        ];

        services.postgresql.initialScript = pkgs.writeText "init-pg.sql" ''
          CREATE USER "${network}-monitor";
          CREATE DATABASE "${network}-monitor" OWNER "${network}-monitor";
        '';
      };
    };

  kilnVM = (import (pkgs.path + /nixos) {
    configuration = {
      imports = [
        "${pkgs.path}/nixos/modules/virtualisation/virtualbox-image.nix"
        "${pkgs.path}/nixos/modules/profiles/demo.nix"
      ];
      environment.systemPackages = [ (runKilnExe (obApp null)) pkgs.firefox];
      services.postgresql = {
        enable         = true;
        authentication = ''
          #      #db              #user  #auth-method  #auth-options
          local  "kiln-db"  "demo" peer
        '';
      };
      services.postgresql.initialScript = pkgs.writeText "init-pg.sql" ''
        CREATE USER "demo";
        CREATE DATABASE "kiln-db" OWNER "demo";
      '';
      services.udev.extraRules = ''
        SUBSYSTEMS=="usb", ATTRS{idVendor}=="2581", ATTRS{idProduct}=="1b7c", MODE="0660", GROUP="users"
        SUBSYSTEMS=="usb", ATTRS{idVendor}=="2581", ATTRS{idProduct}=="2b7c", MODE="0660", GROUP="users"
        SUBSYSTEMS=="usb", ATTRS{idVendor}=="2581", ATTRS{idProduct}=="3b7c", MODE="0660", GROUP="users"
        SUBSYSTEMS=="usb", ATTRS{idVendor}=="2581", ATTRS{idProduct}=="4b7c", MODE="0660", GROUP="users"
        SUBSYSTEMS=="usb", ATTRS{idVendor}=="2581", ATTRS{idProduct}=="1807", MODE="0660", GROUP="users"
        SUBSYSTEMS=="usb", ATTRS{idVendor}=="2581", ATTRS{idProduct}=="1808", MODE="0660", GROUP="users"
        SUBSYSTEMS=="usb", ATTRS{idVendor}=="2c97", ATTRS{idProduct}=="0000", MODE="0660", GROUP="users"
        SUBSYSTEMS=="usb", ATTRS{idVendor}=="2c97", ATTRS{idProduct}=="0001", MODE="0660", GROUP="users"
      '';
      nix.binaryCaches = [ "https://cache.nixos.org/" "https://nixcache.reflex-frp.org" ];
      nix.binaryCachePublicKeys = [ "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=" ];

      nixpkgs = { localSystem.system = "x86_64-linux"; };
      virtualbox = {
        baseImageSize = 20 * 1024; # in MiB
        memorySize = 16 * 1024; # in MiB
        vmDerivationName = "kiln-baker-vm";
        vmName = "Kiln Baker VM";
        vmFileName = "kiln-baker-vm.ova";
      };
      systemd.services.kiln = {
        wantedBy = [ "multi-user.target" ];
        after = [ "network.target" ];
        restartIfChanged = true;
        script = ''
          mkdir -p kiln
          cd kiln
          ln -sft . '${(obApp null).exe}'/*
          mkdir -p log
          exec ./backend --pg-connection='dbname=kiln-db' >>backend.out 2>>backend.err </dev/null
        '';
        serviceConfig = {
          User = "demo";
          KillMode = "process";
          WorkingDirectory = "~";
          Restart = "always";
          RestartSec = 5;
        };
      };
    };
  }).config.system.build.virtualBoxOVA;
}
