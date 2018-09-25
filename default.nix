{ system ? builtins.currentSystem
, obelisk ? (import tezos-bake-central/.obelisk/impl { inherit system; })
, pkgs ? obelisk.reflex-platform.nixpkgs
}:
let
  obApp = import ./tezos-bake-central { inherit system; supportGargoyle = false; };

  tezos-bake-platform = import (pkgs.fetchgit {
    url = "https://gitlab.com/obsidian.systems/tezos-baking-platform.git";
    rev = "82593a2148f8bb9895aaffe6208d852e2ef7ff2e";
    sha256 = "1nsw5v67pjr74j65rhsnvri10hphgfnfimvrg0wf484kaj1kqj2j";
    fetchSubmodules = false;
  }) {};

  tezos = tezos-bake-platform.tezos;

  nodeConfig = {
    zeronet = rec {
      network = "zeronet";
      name = network;
      p2pPort = 29732;
      rpcPort = 28732;
      tzKit = tezos.zeronet.kit;
      monitorPort = 8002;
      monitorPath = "/admin/${name}";
    };
    alphanet = rec {
      network = "alphanet";
      name = network;
      p2pPort = 19732;
      rpcPort = 18732;
      tzKit = tezos.alphanet.kit;
      monitorPort = 8001;
      monitorPath = "/admin/${name}";
    };
    mainnet = rec {
      network = "mainnet";
      name = network;
      p2pPort = 9732;
      rpcPort = 8732;
      tzKit = tezos.betanet.kit;
      monitorPort = 8000;
      monitorPath = "/admin/${name}";
    };
  };

  mkTezosNodeServiceModule = { p2pPort, rpcPort, name, tzKit, ... }: {...}:
    let serviceName = "${name}-node"; user = serviceName; group = user;
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
          exec ${tzKit}/bin/tezos-node run --rpc-addr '127.0.0.1:${toString rpcPort}' --net-addr ':${toString p2pPort}' --data-dir "${dataDir}"
        '';
        serviceConfig = {
          User = user;
          KillMode = "process";
          WorkingDirectory = "~";
          Restart = "always";
          RestartSec = 5;
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
    , name
    , monitorName ? "${name}-monitor"
    , dbname ? monitorName
    , rpcPort
    , monitorPort
    , monitorPath
    , ...}@args:
    obelisk.serverModules.mkObeliskApp (args // {
      exe = obApp.linuxExe;
      name = monitorName;
      internalPort = monitorPort;
      baseUrl = null;
      backendArgs = pkgs.lib.concatStringsSep " " (pkgs.lib.mapAttrsToList (name: value: "--${name}='${value}'") {
        network = if name == "mainnet" then "betanet" else name;
        serve-node-cache = "yes";
        pg-connection = "dbname=${dbname}";
        #route = ''http${if enableHttps then "s" else ""}://${routeHost}${monitorPath}'';
        nodes = "http://127.0.0.1:${toString rpcPort}";
      });
    })
  ;

  monitoringModule = {config, ...}: {
    environment.systemPackages = [ config.services.postgresql.package ];
    services.postgresql = {
      enable         = true;
      authentication = ''
        #      #db                #user               #auth-method  #auth-options
        local  "zeronet-monitor"  "zeronet-monitor"   peer
        local  "alphanet-monitor" "alphanet-monitor"  peer
        local  "mainnet-monitor"  "mainnet-monitor"   peer
      '';
    };

    services.nginx = {
      virtualHosts."tezos-api.obsidian.systems" = {
        locations = {
          "/zeronet/api" = {
            proxyPass = "http://127.0.0.1:${toString nodeConfig.zeronet.monitorPort}/api";
          };
          "/alphanet/api" = {
            proxyPass = "http://127.0.0.1:${toString nodeConfig.alphanet.monitorPort}/api";
          };
          "/mainnet/api" = {
            proxyPass = "http://127.0.0.1:${toString nodeConfig.mainnet.monitorPort}/api";
          };
          "/api" = {
            proxyPass = "http://127.0.0.1:${toString nodeConfig.mainnet.monitorPort}/api";
          };
        };
      };
    };
  };

  usersModule = {config, pkgs, ...}: {
    users.users = let
      monitorGroups = pkgs.lib.mapAttrsToList (key: v: "${v.name}-monitor") nodeConfig;
      nodeGroups = pkgs.lib.mapAttrsToList (key: v: "${v.name}-node") nodeConfig;
    in {
      "elliot.cameron" = {
        description = "Elliot Cameron";
        isNormalUser = true;
        openssh.authorizedKeys.keys = [
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPsrDJrZRXpa6f5g+dfysfU4R/YSqOKRzu2zR99k9izE elliot@nixos"
        ];
        extraGroups = ["wheel"] ++ monitorGroups ++ nodeGroups;
      };
      dbornside = {
        description = "Dan Bornside";
        isNormalUser = true;
        openssh.authorizedKeys.keys = [
          "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQD0ijHT/18Dbjq26bnh2KYndp5vMQXkdD66064xLvpqOVMaPDm9I2QYsEAwGdatnriAFLUhPVkTWTga7KIA37Z9XaTMhKRJb4koT4osIz1ikbVvbUsrLquRC1gulrMRKHjaA3QlPOnOy7pvIW6DYyl9vDhl143X8/7riW9O+pw5OJM8HBKxwIzNZ1XstE3E6VOXnhskU18EBDEqJBE+6+36RBOiGfeDfsV45O1ov4fEAwspV7qIbVirrLnqOyvNfPOCBAnhL5vK6C5Horci1u7hyHHCnV57UoF/fJzYTRKSCeObUNHrhyAlhMstqPhb9qCrtFRDKyBkvmGzntwi/eSv dbornside@localhost.localdomain"
        ];
        extraGroups = ["wheel"] ++ monitorGroups ++ nodeGroups;
      };
    };
  };

in obApp // {
  server = args@{ hostName, adminEmail, routeHost, enableHttps }:
    let
      nixos = import (pkgs.path + /nixos);
    in nixos {
      system = "x86_64-linux";
      configuration = {
        imports = [
          (obelisk.serverModules.mkBaseEc2 args)
          (mkTezosNodeServiceModule nodeConfig.zeronet)
          (mkTezosNodeServiceModule nodeConfig.alphanet)
          (mkTezosNodeServiceModule nodeConfig.mainnet)
          (mkMonitorModule (args // nodeConfig.zeronet))
          (mkMonitorModule (args // nodeConfig.alphanet))
          (mkMonitorModule (args // nodeConfig.mainnet))
          monitoringModule
          usersModule
        ];
      };
    };
}
