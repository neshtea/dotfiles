---
title: My system configurations for NixOS and Darwin (macOS)
author: Marco Schneider
---

This repository contains my personal nixos and
[home-manager](https://github.com/nix-community/home-manager)
configuration.

It manages the configuration for multiple hosts:

- [wayfarer](./hosts/wayfarer): My work MacBook Pro, running macOS.
- [anarres](./hosts/anarres): My personal stationary PC, running NixOS.

# Home Configuration

Shared configuration between hosts can be found in
[home.nix](./hosts/home.nix).  Each host refines that configuration in
their respective `home.nix` declarations.

# Systen Configuration

Each NixOS host defines its system configuration in

- [`configuration.nix`](./hosts/anarres/configuration.nix): System
  configuration.
- [`hardware-configuration.nix`](./hosts/anarres/hardware-configuration.nix):
  Hardware specific configuration configuration.

It is somewhat modularized (see [`modules`](./modules)).  I wouldn't
recommend anyone trying to copy anything in here (there are better,
more well thought out configurations like this [this
one](https://github.com/kenranunderscore/dotfiles)), but it gets the
job done.

Modules can be included in the respecfive home configurations via an
enable option (and possible more).  If, for example, you want your
host to use the configured [kitty terminal
emulator](https://sw.kovidgoyal.net/kitty/), just enable it in the
home configuration like this:

``` nix
modules.programs.kitty.enable = true;
```

# Install

##  macOS

Assuming you have =nix= installed: Clone this repository and then,
from the root of the project run:

``` shell
nix build .#home-configuration.wayfarer.system --extra-experimental-features nix-command --extra-experimental-features flakes
```

This is only required the first time around.

# Updates

## Switch to a new generation

### NixOS

Using `nixos-rebuild`:
```
nixos-rebuild switch --flake <point to dotfiles> --use-remote-sudo
```

### macOS

To switch to the next generation of your system/user config, run:

```
home-manager switch --flake .
```

## Update packages

To update the packages (i.e. upgrade), run

```
nix flake update
```

Or, if you only want to update the `nixpkgs` input, run

```
run flake lock --update-input nixpkgs
```
