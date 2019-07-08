{ pkgs ? import <nixpkgs> {} }:
	pkgs.mkShell {
		buildInputs = [ pkgs.pandoc pkgs.texlive.combined.scheme-small ];
	}
