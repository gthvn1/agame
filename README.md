# OPong

Try to write a "pong game" in OCaml using Raylib...

# Installation
- we created a new switch locally for the project
    - `opam switch create . 5.1.1`
    - `eval $(opam env)`
- and we install `dune` to manage the project
    - you can install others tools like `utop` or `ocaml-lsp-server`.
- you also need to install the [raylib ocaml bindings](https://ocaml.org/p/raylib/0.5.1)
    - automatically install dependencies: `opam depext raylib`
    - and install raylib: `opam install raylib`
 
# Running
- we can run the project using `dune exec opong`

# Todo
- [x] drawing the window
- [x] drawing the players, the ball and the let
- [x] managing players movements
- [x] managing ball movement
- [x] reset the position when one player miss the ball
- [x] counting score
- [ ] be more acurate when ball hitting players
- [ ] improve the ball movement (manage different angles, ...)
- [ ] add effects on the ball (slice, ...)

# Screenshots

<img align="center" src="https://github.com/gthvn1/opong/blob/master/screenshots/first_screenshot.png">

# Links

- [OCaml](https://ocaml.org/)
- [Raylib](https://www.raylib.com/)
