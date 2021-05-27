{ pkgs ? import <nixpkgs> {} }:
let
  compilers = with pkgs;
    [ nasm
      gcc
      ghc
    ];

  libs = with pkgs;
    [ ];

  debuggingTools = with pkgs;
    [ gdb
      strace
      ltrace
      binutils
      elfutils
      bpftools
      xxd
      qemu
      file
    ];
in
pkgs.mkShell {
  buildInputs = compilers ++ libs ++ debuggingTools;
}
