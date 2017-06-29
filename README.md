# ocaml-SeqBox
Implementation of SeqBox in OCaml

The original pharsing was "Port of SeqBox to OCaml", but since no direct porting was actually done due to differences between Python 3(implementation language of SeqBox) and OCaml, this is only an implementation(not a port) of SeqBox according to its technical specifications.
This is mainly to address the different licenses being used(SeqBox uses AGPL 3.0 while this project uses 3-Clause BSD license).

SeqBox - https://github.com/MarcoPon/SeqBox

## Notes
CRC-CCITT implementation is translated from libcrc (https://github.com/lammertb/libcrc) using a copy retrieved on 2017-06-27

See License section for details on licensing

## License
The following files directly from libcrc(with syntax modification) are under the MIT license as used by libcrc
  - crcccitt.c
  - checksum.h
  
The following files translated/ported from libcrc are under the MIT license as used by libcrc as well
  - crcccitt.ml
  - crcccitt.mli

All remaining files are distributed under the 3-Clause BSD license as stated in the LICENSE file
