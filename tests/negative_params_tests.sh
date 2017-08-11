#!/bin/bash

truncate -s 1M dummy

echo "decode using 0 show-fail-max"
echo "========================================"

./osbx decode --show-fail-max=0 dummy

echo ""
echo "decode using -1 show-fail-max"
echo "========================================"

./osbx decode --show-fail-max=-1 dummy

echo ""

echo "show using 0 find-max"
echo "========================================"

./osbx show --find-max=0 dummy

echo ""
echo "show using -1 find-max"
echo "========================================"

./osbx show --find-max=-1 dummy

echo ""

echo "show using 0 skip-to"
echo "========================================"

./osbx show --skip-to=0 dummy

echo ""
echo "show using -1 skip-to"
echo "========================================"

./osbx show --skip-to=-1 dummy

echo ""
