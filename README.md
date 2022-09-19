# QuantumGoose

ICFP non-sense about quantum languages 

## Instruction

Compiling the project requires `opam`, `opal`, `dune`, and `ppx_inline_test` to be installed

**To install ocaml/opam**

[ocaml website](https://ocaml.org/docs/up-and-running)

**To setup the project**

```
cd QuantumGoose
opam install dune opal ppx_inline_test
```
**To compile**

```
dune build

```

**To test**

```
dune runtest
```

**To execute the QuantumGoose compiler (qgc)**

```
dune exec qgc
```
