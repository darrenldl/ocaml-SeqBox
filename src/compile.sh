#!/bin/bash
# corebuild -cflags -safe-string -pkgs stdint,nocrypto,core,hex input_file.byte
corebuild -pkgs stdint,nocrypto,core,hex misc_utils.byte
