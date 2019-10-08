{ pkgs
, obelisk
, app
, opsEmail ? "divam.narula@obsidian.systems"
, ...
}:
let

  tezos = (import dep/tezos-baking-platform {}).tezos;

  kilnApiV1 = import (pkgs.fetchFromGitHub {
    owner = "obsidian.systems";
    repo = "tezos-bake-monitor";
    rev = "788d9b53f166f7801addb36c778bfc36bc833c15"; # 0.5.3
    sha256 = "0g1fijywb7afqy056v9rfl293fa9hzrz1q1p949blzcxv3iqp0jn";
  }) { system = "x86_64-linux"; };

  kilnApiV2 = import (pkgs.fetchFromGitHub {
    owner = "obsidian.systems";
    repo = "tezos-bake-monitor";
    rev = "dbc3775f2c9c28a39ecec1044c0efe6bb6c08a9d"; # 0.6.2
    sha256 = "0kgbbfs75ql9vd514nsspapgr3l80vwwwfyslaixj1cfmlzahdxr";
  }) { system = "x86_64-linux"; };

  networkConfigOptions = {
    zeronet = {
      network = "zeronet";
      p2pPort = 29732;
      rpcPort = 28732;
      tzKit = tezos.zeronet.kit;
      monitorPort = 8002;
      histMode = "archive";
      kilns = [
        {
          app = kilnApiV1;
          apiVersion = 1;
          apiPort = 8001;
        }
        {
          app = kilnApiV2;
          apiVersion = 2;
          apiPort = 8002;
          extraArgs = [
            "--enable-obsidian-node=false"
          ];
        }
        {
          inherit app;
          apiVersion = 3;
          apiPort = 8000;
          extraArgs = [
            "--enable-obsidian-node=false"
          ];
        }
      ];
    };
    alphanet = {
      network = "alphanet";
      p2pPort = 19732;
      rpcPort = 18732;
      tzKit = tezos.alphanet.kit;
      histMode = "archive";
      kilns = [
        {
          app = kilnApiV1;
          apiVersion = 1;
          apiPort = 8001;
        }
        {
          app = kilnApiV2;
          apiVersion = 2;
          apiPort = 8002;
          extraArgs = [
            "--enable-obsidian-node=false"
          ];
        }
        {
          inherit app;
          apiVersion = 3;
          apiPort = 8000;
          extraArgs = [
            "--enable-obsidian-node=false"
          ];
        }
      ];
    };
    mainnet = {
      network = "mainnet";
      p2pPort = 9732;
      rpcPort = 8732;
      tzKit = tezos.mainnet.kit;
      histMode = "archive";
      kilns = [
        {
          app = kilnApiV1;
          apiVersion = 1;
          apiPort = 8001;
        }
        {
          app = kilnApiV2;
          apiVersion = 2;
          apiPort = 8002;
          extraArgs = [
            "--enable-obsidian-node=false"
          ];
        }
        {
          inherit app;
          apiVersion = 3;
          apiPort = 8000;
          extraArgs = [
            "--enable-obsidian-node=false"
          ];
        }
      ];
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
          exec ${tzKit}/bin/tezos-node run --rpc-addr '127.0.0.1:${toString rpcPort}' --net-addr '0.0.0.0:${toString p2pPort}' --data-dir "${dataDir}" ${if histMode == null then "" else "--history-mode ${histMode}"}
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
    , kiln ? { app = null; apiVersion = null; apiPort = null; }
    , monitorName ? "kiln-${network}-v${toString kiln.apiVersion}"
    , dbname ? monitorName
    , user ? monitorName
    , rpcPort
    , version
    , ...}@args: {config, ...}: {
      imports = [
        ./pg-init-module.nix
        (obelisk.serverModules.mkObeliskApp (args // {
          exe = kiln.app.linuxExeConfigurable version;
          name = monitorName;
          user = user;
          internalPort = kiln.apiPort;
          baseUrl = null;
          backendArgs = pkgs.lib.concatStringsSep " " ([
            "--network='${network}'"
            "--serve-node-cache=yes"
            "--pg-connection='dbname=${dbname}'"
            "--check-for-upgrade=no"
            "--nodes='http://127.0.0.1:${toString rpcPort}'"
            "--email-from='${monitorName}@obsidian.systems'"
            "--network-gitlab-project-id='${pkgs.lib.fileContents ../tezos-bake-central/config/network-gitlab-project-id}'"
          ] ++ (kiln.extraArgs or []) ++ [
            "--"
            "--port=${toString kiln.apiPort}"
          ]);
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
            "/api/v${toString kiln.apiVersion}" = {
              proxyPass = "http://127.0.0.1:${toString kiln.apiPort}/api/v${toString kiln.apiVersion}";
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

      services.pgAggregatedInitScript = ''
        CREATE USER "${user}";
        CREATE DATABASE "${dbname}" OWNER "${user}";
      '';
    }
  ;

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
      "divam.narula" = {
        description = "Divam";
        isNormalUser = true;
        openssh.authorizedKeys.keys = [
          "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQC6vZkH8FD5grlE/qVpYiViVeUwzUzpC1BZmIYrsrAsTbJSDK3h9R9WQx6J9jiyVwcSLV4RKBh6GZzzMmPzdtkaXaZsKYTVNgPq5txiHal62jV4RD+qWQpE0Z0vzd/OMeq9y0FpldITCkrcwCXnM7Tvt4BCwD+mHM2AVJsvIga0mDgc0r7dxRA+yocCbv2qA1rgqXXhFud2Y2DZkjJPK2env46aZzN0VDrdqCRFsDkXMCU/MGWpJUtQCMLjbEa0k0VrOBUw96c3kiboQJDDnEGFE/X3znc2VCff6tmWEVcJZP52U40GpR/4+ROMpu5ISu3dJRVTsDpdAsPZFGzg/6rkJXi6mCPj6WoiEnOKOH88IdpoLn1tD7ErEYdekv/+bwMEWixPtRzvYxBFQ6uqJYjJAS7sf/8IM8k7YAB1WUtuqIyHZ2V8UCpMr4A/ujtABXwH+SW7+UEPsU3+aC8cKPE8QZJ2m8nKq5hbKJXeS3B4gCBx+e3LA5DHi5s/cN4iWCbclm0IMPg9AUbtquz3lRKy8Gl09gPsfjLnboZ8FbwOSKWlV+PqnQBbUSkRhjxWg3On18CEvsai4egI++GK2qOKJSKstgw0pNSQkU1rzGgx/9kYskhKA0kBKwAgCGf9zoPsUJ00mcuyEoYBZuOsMirwR5lZue3qvq5W2noLM+Gofw== dfordivam@gmail.com"
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

  server = args@{ hostName, adminEmail, routeHost, enableHttps, version, ... }:
    let
      network =
        if pkgs.lib.strings.hasPrefix "zeronet" hostName then "zeronet" else
        if pkgs.lib.strings.hasPrefix "alphanet" hostName then "alphanet" else
        "mainnet";
      networkConfig = networkConfigOptions.${network};
      nixos = import (pkgs.path + /nixos);
      kilnModules = map
        (kiln: mkMonitorModule (args // networkConfig // { inherit kiln version; }))
        networkConfig.kilns;

    in nixos {
      system = "x86_64-linux";
      configuration = {
        imports = [
          (obelisk.serverModules.mkBaseEc2 args)
          (mkTezosNodeServiceModule networkConfig)
          (syslog-ngModule {
            opsEmail = if pkgs.lib.strings.hasPrefix "zeronet" hostName then null else opsEmail;
          })
          usersModule
        ] ++ kilnModules;
      };
    };
in server
