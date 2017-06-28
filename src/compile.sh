#!/bin/bash
# corebuild -cflags -safe-string -pkgs stdint,nocrypto,core,hex input_file.byte
corebuild -pkgs stdint,nocrypto,core,hex in_file.byte
