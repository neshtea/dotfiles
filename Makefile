.PHONY: build
build:
	home-manager switch --flake .

update:
	nix flake update
