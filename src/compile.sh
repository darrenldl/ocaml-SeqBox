#!/bin/bash
# corebuild -cflags -safe-string -pkgs stdint,nocrypto,core,hex input_file.byte
corebuild -pkgs stdint,nocrypto,nocrypto.unix,core,hex,angstrom encode.native
