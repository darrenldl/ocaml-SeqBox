export OPAM_VERSION=1.2.2

export OPAM_PACKAGES='jbuilder stdint nocrypto digestif angstrom hex cmdliner'

sudo apt-get update -qq

sudo apt-get install -qq ocaml

# install opam
curl -L https://github.com/OCamlPro/opam/archive/${OPAM_VERSION}.tar.gz | tar xz -C /tmp
pushd /tmp/opam-${OPAM_VERSION}
./configure
make
sudo make install
opam init
eval `opam config -env`
popd

opam switch 4.04.2
eval `opam config -env`

opam install -q -y $(OPAM_PACKAGES)

./tests/dev_tests.sh
