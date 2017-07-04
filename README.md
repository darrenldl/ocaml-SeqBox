# ocaml-SeqBox
Implementation of SeqBox in OCaml

The original pharsing was "Port of SeqBox to OCaml", but since no direct porting/translation was actually done due to the differences between Python 3(implementation language of SeqBox) and OCaml, which also force the architecture and design of this software(ocaml-SeqBox) to be independently developed, thus this is only an implementation(not a port or translation) of SeqBox according to its technical specifications.
This is mainly to address the different licenses being used(SeqBox was using AGPL 3.0 at the time of writing while this project uses 3-Clause BSD license).

Official SeqBox Repo - https://github.com/MarcoPon/SeqBox

## Notes
CRC-CCITT implementation is translated from libcrc (https://github.com/lammertb/libcrc) using a copy retrieved on 2017-06-27

See License section for details on licensing

## Technical Specification
The following specification is copied directly from the official specification with slight modification(version 2, 3 are stated excplicitly as not implemented).

Byte order: Big Endian
### Common blocks header:

| pos | to pos | size | desc                                |
|---- | ---    | ---- | ----------------------------------- |
|  0  |      2 |   3  | Recoverable Block signature = 'SBx' |
|  3  |      3 |   1  | Version byte |
|  4  |      5 |   2  | CRC-16-CCITT of the rest of the block (Version is used as starting value) |
|  6  |     11 |   6  | file UID                            |
| 12  |     15 |   4  | Block sequence number               |

### Block 0

| pos | to pos   | size | desc             |
|---- | -------- | ---- | ---------------- |
| 16  | n        | var  | encoded metadata |
|  n+1| blockend | var  | padding (0x1a)   |

### Blocks > 0 & < last:

| pos | to pos   | size | desc             |
|---- | -------- | ---- | ---------------- |
| 16  | blockend | var  | data             |

### Blocks == last:

| pos | to pos   | size | desc             |
|---- | -------- | ---- | ---------------- |
| 16  | n        | var  | data             |
| n+1 | blockend | var  | padding (0x1a)   |

### Versions:
N.B. Current versions differs only by blocksize.

| ver | blocksize | note    |
|---- | --------- | ------- |
|  1  | 512       | default |
|  2  | 128       | NOT IMPLEMENTED |
|  3  | 4096      | NOT IMPLEMENTED |

### Metadata encoding:

| Bytes | Field | 
| ----- | ----- |
|    3  | ID    |
|    1  | Len   |
|    n  | Data  |

#### IDs

| ID | Desc |
| --- | --- |
| FNM | filename (utf-8) |
| SNM | sbx filename (utf-8) |
| FSZ | filesize (8 bytes) |
| FDT | date & time (8 bytes, seconds since epoch) |
| SDT | sbx date & time (8 bytes) |
| HSH | crypto hash (SHA256, using [Multihash](http://multiformats.io) protocol) |
| PID | parent UID (*not used at the moment*)|

## Index of source code
```
In_file module (in_file.ml, in_file.mli)
  - Hashing and loading entire file data in memory
  - No longer used in favour of Stream_file module
Sbx_block module (sbx_block.ml, sbx_block.mli)
  - Single SBX block construction/access, encoding to bytes and decoding from bytes
  - Submodules
    - Header
    - Metadata
    - Block
Stream_file (stream_file.ml, stream_file.mli)
  - Provides framework for streamed processing of files
  - Abstracts away low-level file interaction, allows other modules to only construct "processor" to be run by the framework
    - Processor is a function that only deals with input and/or output channel
Encode module (encode.ml, encode.mli)
  - Encode file (and directly output into another file)
  - Relies on processor framework in Stream_file
Decode module (decode.ml, decode.mli)
  - Decode file (and directly output into another file)
  - Relies on processor framework in Stream_file
```

## Progress Report
  - Single SBX block encoding - done (Sbx_block module)
  - Single SBX block decoding - done (Sbx_block module)
  - Streamed processing framework - done (Streamed_file module)
  - Streamed file encoding - done (Encode module)
  - Streamed file decoding - in progress (Decode module)
  - Commandline interface for encoding and decoding - not started
  - Scan mode - not started
  - Recovery mode - not started
  - Commandline options for scanning and recovery - not started

## License
The following files directly from libcrc(with syntax modification) are under the MIT license as used by libcrc
  - crcccitt.c
  - checksum.h
  
The following files translated/ported from libcrc are under the MIT license as used by libcrc as well
  - crcccitt.ml
  - crcccitt.mli

All remaining files are distributed under the 3-Clause BSD license as stated in the LICENSE file
