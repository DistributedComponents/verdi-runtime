Verdi Runtime
=============

Verdi Runtime is an OCaml library providing the functionality necessary to run verified distributed systems developed in the [Coq](https://coq.inria.fr) based [Verdi framework](https://github.com/uwplse/verdi) on real hardware. In particular, it provides several shims that handle the lower-level details of network communication.

Requirements
------------

- [`OCaml 4.02.3`](https://ocaml.org) (or later)
- [`OCamlbuild`](https://github.com/ocaml/ocamlbuild)
- [`ocamlfind`](http://projects.camlcity.org/projects/findlib.html)
- [`topkg`](http://erratique.ch/software/topkg)
- [`cheerios-runtime`](https://github.com/uwplse/cheerios)
- [`base64`](https://github.com/mirage/ocaml-base64) (3.0.0 or later)
- [`yojson`](https://github.com/ocaml-community/yojson) (1.7.0 or later)

Installation
------------

The easiest way to install the library (and its dependencies) is via [opam](https://opam.ocaml.org):
```shell
opam pin add cheerios-runtime -n -y -k git https://github.com/uwplse/cheerios.git
opam pin add verdi-runtime -k git https://github.com/DistributedComponents/verdi-runtime.git
```

If you don't use opam, consult the [opam file](opam) for build instructions.

Files
-----

- `Shim.ml`: shim for extracted systems verified against a network semantics with _unordered_ message passing and node reboots, implemented using UDP and state checkpointing
- `UnorderedShim.ml`: shim for extracted systems verified against a network semantics with _unordered_ message passing *without* node reboots, implemented using UDP
- `OrderedShim.ml`: shim for extracted systems verified against a network semantics with _ordered_ message passing, implemented using TCP
- `Daemon.ml`: fair task-processing event loop based on the Unix `select` system call, used in `OrderedShim.ml` and `UnorderedShim.ml`
- `Opts.ml`: basic Verdi cluster command line argument processing based on OCaml's `Arg` module
- `Util.ml`: miscellaneous functions, e.g., timestamps and conversion between `char list` and `string`

Usage
-----

In order to run Verdi systems, the proper shim from Verdi Runtime must be linked to the OCaml event handler code extracted by Coq. Examples of this use can be found in Verdi-based verification projects:

- [Verdi Raft](https://github.com/uwplse/verdi-raft)
- [Verdi LockServ](https://github.com/DistributedComponents/verdi-lockserv)
