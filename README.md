# ocaml-SeqBox
Implementation of [SeqBox](https://github.com/MarcoPon/SeqBox) in OCaml

The original pharsing was "Port of SeqBox to OCaml", but since no direct porting/translation was actually done due to the differences between Python 3(implementation language of SeqBox) and OCaml, which also force the architecture and design of this software(ocaml-SeqBox) to be independently developed, thus this is only an implementation(not a port or translation) of SeqBox according to its technical specifications.
This is mainly to address the different licenses being used(SeqBox was using AGPL 3.0 at the time of writing while this project uses 3-Clause BSD license).

Official SeqBox Repo - https://github.com/MarcoPon/SeqBox

Table of Contents
=================

   * [ocaml-SeqBox](#ocaml-seqbox)
      * [Notes](#notes)
      * [Possibly useful additional features of ocaml-SeqBox(possibly not yet in official SeqBox)](#possibly-useful-additional-features-of-ocaml-seqboxpossibly-not-yet-in-official-seqbox)
      * [Technical Specification](#technical-specification)
         * [Common blocks header:](#common-blocks-header)
         * [Block 0](#block-0)
         * [Blocks &gt; 0 &amp; &lt; last:](#blocks--0---last)
         * [Blocks == last:](#blocks--last)
         * [Versions:](#versions)
         * [Metadata encoding:](#metadata-encoding)
            * [IDs](#ids)
            * [Features currently NOT planned to be implemented](#features-currently-not-planned-to-be-implemented)
      * [Index of source code](#index-of-source-code)
      * [Progress Report](#progress-report)
      * [License](#license)

Created by [gh-md-toc](https://github.com/ekalinin/github-markdown-toc)
                                                           
## Notes
CRC-CCITT is currently implemented via FFI and links to the static object files compiled from libcrc (https://github.com/lammertb/libcrc)
  - See crcccitt_wrap.ml, crcccitt_wrap.mli for the FFI bindings
  - See crcccitt.c, checksum.h for libcrc source used(crcccitt.c is slightly modified, modification is under same license used by libcrc)

Exact behaviours in non-standard cases are not specified in official SeqBox technical specification
  - See [specification](SPECS.md) of ocaml-SeqBox for details on how ocaml-SeqBox behaves(if you care about undefined behaviour those sort of things)

## Possibly useful additional features of ocaml-SeqBox(possibly not yet in official SeqBox)
  - Allows random ordering in sbx container
    - This also means block corruption will not stop the decoding process
  - Allows duplicate metadata/data blocks to exist within one sbx container
    - This means you can concatenate multiple copies of sbx container together directly to increase chance of recovery in case of corruption

## Technical Specification
The following specification is copied directly from the official specification (with possible slight modification).

Also see section "Features currently NOT planned to be implemented" for features ocaml-SeqBox is probably not going to have

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
|  2  | 128       |         |
|  3  | 4096      |         |

All versions are implemented in ocaml-SeqBox

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

#### Features currently NOT planned to be implemented
  - Data hiding (XOR encoding/decoding in official seqbox)
    - Provides neither sufficiently strong encryption nor sufficient stealth for any serious attempt to hide/secure data
    - You should use the appropriate tools for encryption

## Index of source code
 
**Sbx_block (sbx_block.ml, sbx_block.mli)**
  - Single SBX block construction/access, encoding to bytes and decoding from bytes
  - Submodules
    - Header
    - Metadata
    - Block
    
**Stream_file (stream_file.ml, stream_file.mli)**
  - Provides framework for streamed processing of files
  - Abstracts away low-level file interaction, allows other modules to only construct "processor" to be run by the framework
    - Processor is a function that only deals with input and/or output channel
    
**Encode (encode.ml, encode.mli)**
  - Encode file (and directly output into another file)
  - Relies on processor framework in Stream_file
  
**Decode (decode.ml, decode.mli)**
  - Decode file (and directly output into another file)
  - Relies on processor framework in Stream_file
  
**Crcccitt_wrap (crcccitt_wrap.ml, crccitt.mli)**
  - FFI binding to crcccitt.c

**Conv_utils (conv_utils.ml, conv_utils.mli)**
  - Utility functions for converting between different data types/format

**File_utils (file_utils.ml, file_utils.mli)**
  - Utility functions for file access(namely for getting metadata)

**Misc_utils (misc_utils.ml, misc_utils.mli)**
  - Small utility functions used across different modules

**Random_utils (random_utils.ml, random_utils.mli)**
  - Utility functions for generating random bytes etc

**Time_utils (time_utils.ml, time_utils.mli)**
  - Utilitiy functions to get time, etc

**Multihash (multihash.ml, multihash.mli)**
  - Partial multihash implementation (only supporting SHA256 hash)

**Osbx (osbx.ml)**
  - Main program file/entry point

**Osbx_encode (osbx_encode.ml)**
  - Functions for encode command

**Osbx_decode (osbx_decode.ml)**
  - Functions for decode command

**Param (param.ml, param.mli)**
  - Central place to configure parameters for other modules

**Sbx_specs (sbx_specs.ml, sbx_specs.mli)**
  - Sbx container specification related parameters, functions

## Progress Report
  - Single SBX block encoding - done (Sbx_block module)
  - Single SBX block decoding - done (Sbx_block module)
  - Streamed processing framework - done (Streamed_file module)
  - Streamed file encoding - done (Encode module)
  - Streamed file decoding - done (Decode module)
  - Profiling and optimization of encoding - done
    - Result : CRC-CCITT implementation causes major slowdown
  - Profiling and optimization of decoding - done
    - Result : CRC-CCITT implementation causes major slowdown
  - Replace CRC-CCITT implementation with a more efficient one(or create FFI to libcrc) - done
    - CRC-CCITT is implemented via FFI to libcrc
  - Commandline interface for encoding and decoding - done
  - Further profiling and optimization of encoding - skipped for now
  - Further profiling and optimization of decoding - skipped for now
  - ~~Scan mode - not started~~
    - Replaced by rescue mode
  - ~~Recovery mode - not started~~
    - Replaced by rescue mode
  - Rescue mode - in progress
  - ~~Commandline options for scanning and recovery - not started~~
    - Replaced by rescue mode
  - Commandline interface for rescue mode - not started
  - Package and publish to opam - not started

## License
The following files directly from libcrc(with slight modification) are under the same MIT license used by libcrc
  - crcccitt.c
  - checksum.h

All remaining files are distributed under the 3-Clause BSD license as stated in the LICENSE file
