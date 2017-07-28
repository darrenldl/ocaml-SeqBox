export OPAM_VERSION=1.2.2

sudo apt-get update -qq

# install opam
curl -L https://github.com/OCamlPro/opam/archive/${OPAM_VERSION}.tar.gz | tar xz -C /tmp
pushd /tmp/opam-${OPAM_VERSION}
./configure
make
sudo make install
opam init
eval `opam config -env`
popd

opam pin add osbx .

./tests/dev_tests.sh
