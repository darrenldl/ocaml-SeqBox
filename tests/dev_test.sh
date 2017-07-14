#!/bin/bash

cd ..

echo "Building osbx"
jbuilder build @install
echo ""

echo "Copying osbx binary over"
cp _build/default/src/osbx.exe ./tests/osbx
echo ""

cd tests

echo "Generating test data"
# dd if=/dev/zero of=dummy bs=$[1024 * 1024 * 10] count=1
truncate -s $[10 * 496] dummy
echo ""

# version tests
echo "Starting version tests"
echo "========================================"
./version_tests.sh
echo "========================================"

echo ""

# nometa tests
echo "Starting nometa tests"
echo "========================================"
./nometa_tests.sh
echo "========================================"

echo ""

# rescue tests
echo "Starting rescue tests"
echo "========================================"
./rescue_tests.sh
echo "========================================"
