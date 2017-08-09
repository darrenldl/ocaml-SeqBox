#!/bin/bash

echo "Generating UID"

uid=$(cat /dev/urandom | tr -dc 0-9a-f | fold -w 12 | head -n 1)

echo ""

dd if=/dev/urandom of=dummy bs=2096 count=1

echo "Generating sbx container using official SeqBox"

python3 SeqBox/sbxenc.py -uid $uid dummy dummy_official.sbx -o

echo ""

echo "Generating sbx container using osbx"

./osbx encode dummy dummy_osbx.sbx -f --uid $uid

echo ""

echo "Comparing the two containers"

cmp -i 512 dummy_official.sbx dummy_osbx.sbx
