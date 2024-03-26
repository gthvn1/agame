# OCRay

Try to write a "game" in OCaml using Raylib...

# Installation
- we created a new switch locally for the project
    - `opam switch create . 5.1.1`
    - `eval $(opam env)`
- and we install `dune` to manage the project
    - you can install others tools like `utop` or `ocaml-lsp-server`.
- you also need to install the [raylib ocaml bindings](https://ocaml.org/p/raylib/0.5.1)
    - automatically install dependencies: `opam depext raylib`
    - and install raylib: `opam install raylib`
- we can run the project using `dune exec ocray`

## Links

- [OCaml](https://ocaml.org/)
- [Raylib](https://www.raylib.com/)
