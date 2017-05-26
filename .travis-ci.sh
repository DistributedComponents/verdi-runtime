set -ev

opam init --yes --no-setup
eval $(opam config env)

opam pin add verdi-runtime . --yes --verbose
