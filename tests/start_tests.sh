#!/bin/bash

cd ..

echo "Building osbx"
opam install osbx
echo ""

echo "Copying osbx binary over"
cp ~/.opam/system/bin/osbx ./tests/osbx
echo ""

cd tests

echo "Generating test data"
dd if=/dev/urandom of=dummy bs=1024 count=1024
echo ""

# version tests
echo "Starting version tests"
echo "========================================"
./version_tests.sh
echo "========================================"

echo ""

# rescue tests
echo "Starting rescue tests"
echo "========================================"
./rescue_tests.sh
echo "========================================"
