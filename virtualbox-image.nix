{ config, lib, pkgs, ... }:

with lib;

let

  cfg = config.virtualbox;

in {

  options = {
    virtualbox = {
      baseImageSize = mkOption {
        type = types.int;
        default = 10 * 1024;
        description = ''
          The size of the VirtualBox base image in MiB.
        '';
      };
      memorySize = mkOption {
        type = types.int;
        default = 1536;
        description = ''
          The amount of RAM the VirtualBox appliance can use in MiB.
        '';
      };
      vmDerivationName = mkOption {
        type = types.str;
        default = "nixos-ova-${config.system.nixos.label}-${pkgs.stdenv.hostPlatform.system}";
        description = ''
          The name of the derivation for the VirtualBox appliance.
        '';
      };
      vmName = mkOption {
        type = types.str;
        default = "NixOS ${config.system.nixos.label} (${pkgs.stdenv.hostPlatform.system})";
        description = ''
          The name of the VirtualBox appliance.
        '';
      };
      vmFileName = mkOption {
        type = types.str;
        default = "nixos-${config.system.nixos.label}-${pkgs.stdenv.hostPlatform.system}.ova";
        description = ''
          The file name of the VirtualBox appliance.
        '';
      };
      extraDisk = mkOption {
        description = ''
          Optional extra disk/hdd configuration.
          The disk will be an 'ext4' partition on a separate VMDK file.
        '';
        default = null;
        example = {
          label = "storage";
          mountPoint = "/home/demo/storage";
          size = 100 * 1024;
        };
        type = types.nullOr (types.submodule {
          options = {
            size = mkOption {
              type = types.int;
              description = "Size in MiB";
            };
            label = mkOption {
              type = types.str;
              default = "vm-extra-storage";
              description = "Label for the disk partition";
            };
            mountPoint = mkOption {
              type = types.str;
              description = "Path where to mount this disk.";
            };
          };
        });
      };
    };
  };

  config = {
    system.build.virtualBoxOVA = import (pkgs.path + /nixos/lib/make-disk-image.nix) {
      name = cfg.vmDerivationName;

      inherit pkgs lib config;
      partitionTableType = "legacy";
      diskSize = cfg.baseImageSize;

      postVM =
        ''
          export HOME=$PWD
          export PATH=${pkgs.virtualbox}/bin:$PATH

          echo "creating VirtualBox pass-through disk wrapper (no copying invovled)..."
          VBoxManage internalcommands createrawvmdk -filename disk.vmdk -rawdisk $diskImage

          ${optionalString (cfg.extraDisk != null) ''
            echo "creating extra disk: data-disk.raw"
            dataDiskImage=data-disk.raw
            truncate -s ${toString cfg.extraDisk.size}M $dataDiskImage

            parted --script $dataDiskImage -- \
              mklabel msdos \
              mkpart primary ext4 1MiB -1
            eval $(partx $dataDiskImage -o START,SECTORS --nr 1 --pairs)
            mkfs.ext4 -F -L ${cfg.extraDisk.label} $dataDiskImage -E offset=$(sectorsToBytes $START) $(sectorsToKilobytes $SECTORS)K
            echo "creating extra disk: data-disk.vmdk"
            ${pkgs.qemu}/bin/qemu-img convert -f raw -O vmdk $dataDiskImage data-disk.vmdk
          ''}

          echo "creating VirtualBox VM..."
          vmName="${cfg.vmName}";
          VBoxManage createvm --name "$vmName" --register \
            --ostype ${if pkgs.stdenv.hostPlatform.system == "x86_64-linux" then "Linux26_64" else "Linux26"}
          VBoxManage modifyvm "$vmName" \
            --memory ${toString cfg.memorySize} --acpi on --vram 32 \
            ${optionalString (pkgs.stdenv.hostPlatform.system == "i686-linux") "--pae on"} \
            --nictype1 virtio --nic1 nat \
            --audiocontroller ac97 --audio alsa \
            --rtcuseutc on \
            --usb on --mouse usbtablet
          VBoxManage storagectl "$vmName" --name SATA --add sata --portcount 4 --bootable on --hostiocache on
          VBoxManage storageattach "$vmName" --storagectl SATA --port 0 --device 0 --type hdd \
            --medium disk.vmdk
          ${optionalString (cfg.extraDisk != null) ''
            VBoxManage storageattach "$vmName" --storagectl SATA --port 1 --device 0 --type hdd \
            --medium data-disk.vmdk
          ''}

          echo "exporting VirtualBox VM..."
          mkdir -p $out
          fn="$out/${cfg.vmFileName}"
          VBoxManage export "$vmName" --output "$fn"

          rm -v $diskImage

          mkdir -p $out/nix-support
          echo "file ova $fn" >> $out/nix-support/hydra-build-products
        '';
    };

    fileSystems = { "/" = {
         device = "/dev/disk/by-label/nixos";
         autoResize = true;
         fsType = "ext4";
       };
    } // (if cfg.extraDisk == null then {} else {
      ${cfg.extraDisk.mountPoint} = {
        device = "/dev/disk/by-label/" + cfg.extraDisk.label;
        autoResize = true;
        fsType = "ext4";
      };
    });

    boot.growPartition = true;
    boot.loader.grub.device = "/dev/sda";

    virtualisation.virtualbox.guest.enable = true;

  };
}
