{ pkgs ? import <nixpkgs> {} }:
let
  compilers = with pkgs;
    [ nasm
      gcc
      ghc
    ];

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
  buildInputs = compilers ++ debuggingTools;
}
