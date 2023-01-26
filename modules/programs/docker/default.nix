# Adapted from https://unix.stackexchange.com/a/670716 and
# https://pilsniak.com/how-to-install-docker-on-mac-os-using-brew

{ config, pkgs, lib, ... }:

let cfg = config.modules.programs.docker;
in {
  options.modules.programs.docker = { enable = lib.mkEnableOption "docker"; };
  config = lib.mkIf cfg.enable {
    home = {
      packages = with pkgs; [
        docker
        docker-compose
        docker-machine
        xhyve # for linux virtualization
        docker-machine-xhyve # docker driver for xhyve
      ];
      activation = {
        createDockerMachine = lib.hm.dag.entryAnywhere ''
          docker-machine create default --driver xhyve --xhyve-experimental-nfs-share=true --xhyve-disk-size "40000"
        '';
      };
    };
  };
}
