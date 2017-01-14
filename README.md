# Verdi Runtime

Verdi Runtime is an OCaml library providing the functionality necessary to run distributed systems developed in the [Coq](https://coq.inria.fr) based [Verdi framework](https://github.com/uwplse/verdi) on real hardware. In particular, it provides several shims that handle the lower-level details of network communication.

## Requirements

- [`OCaml 4.02`](https://ocaml.org)
- [`ocamlfind`](http://projects.camlcity.org/projects/findlib.html)
- [`ocamlbuild`](https://github.com/ocaml/ocamlbuild)
- [`topkg`](http://erratique.ch/software/topkg)

## Installation

The easiest way to install the library (and its dependencies) is via [OPAM](https://opam.ocaml.org).

```
opam repo add distributedcomponents http://opam.distributedcomponents.net
opam install verdi-runtime
```

If you don't use OPAM, consult the [`opam`](opam) file for build instructions.

## Files

- `Shim.ml`: shim for extracted systems verified against a network semantics with _unordered_ message passing, implemented using UDP
- `OrderedShim.ml`: shim for extracted systems verified against a network semantics with _ordered_ message passing, implemented using TCP
- `Daemon.ml`: fair task-processing event loop based on the Unix `select` system call, used in `OrderedShim.ml`
- `Opts.ml`: basic Verdi cluster command line argument processing based on OCaml's `Arg` module
- `Util.ml`: miscellaneous functions, e.g., timestamps and conversion between `char list` and `string`

## Usage

The best way to learn how to use Verdi Runtime is to examine its use in Verdi-based verification projects to link with OCaml code extracted by Coq:

- [Verdi Raft](https://github.com/uwplse/verdi-raft)
- [Verdi Aggregation](https://github.com/DistributedComponents/verdi-aggregation)
