# ocaml-SeqBox

## No longer maintained, use rsbx instead
This project has been superseded by [rsbx](https://github.com/darrenldl/rust-SeqBox) and is no longer actively developed or maintained due to lack of active users(I no longer use this program as well).

Implementation of [SeqBox](https://github.com/MarcoPon/SeqBox) in OCaml

The original pharsing was "Port of SeqBox to OCaml", but since no direct porting/translation was actually done due to the differences between Python 3(implementation language of SeqBox) and OCaml, which also force the architecture and design of this software(ocaml-SeqBox) to be independently developed, thus this is only an implementation(not a port or translation) of SeqBox according to its technical specifications.
This is mainly to address the different licenses being used(SeqBox was using AGPL 3.0 at the time of writing while this project uses 3-Clause BSD license).

Official SeqBox Repo - https://github.com/MarcoPon/SeqBox

Table of Contents
=================

   * [ocaml-SeqBox](#ocaml-seqbox)
      * [Acknowledgement](#acknowledgement)
      * [Getting started](#getting-started)
         * [Recordings](#recordings)
         * [Installation](#installation)
      * [Notes](#notes)
      * [Tips](#tips)
      * [Gotchas](#gotchas)
      * [Helpers](#helpers)
      * [Links](#links)
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
      * [License](#license)

Created by [gh-md-toc](https://github.com/ekalinin/github-markdown-toc)

## Acknowledgement
I would like to thank [Marco](https://github.com/MarcoPon/) (author of the official SeqBox) for discussing and clarifying several aspects of his project, and providing me with test data.

I would like to thank [Ming](https://github.com/mdchia/) for his feedback on the documentation, UX design, and several other general aspects of this project. And also his help on testing the building and installation of osbx on macOS.

## Getting started
### Recordings
You can view the recordings [here](https://github.com/darrenldl/ocaml-SeqBox/wiki/Recordings)

### Installation
Osbx 1.2.4 is available through OPAM.
```
opam install osbx
```
There are four modes/commands for osbx currently : encode, decode, rescue, show

You can consult the man pages of osbx itself and all the four commands via
```
osbx        --help
osbx encode --help
osbx decode --help
osbx rescue --help
osbx show   --help
```

## Notes
Version 1.2.4 is considered to be feature complete and mature enough for production use, but any bug reports or suggestions are very welcome - just open an issue!

Contributions are welcome as well, but note that by submitting a contribution, you agree that your code will be licensed under the 3-Clause BSD license.

No major active development will occur, but since 1.2.4 was designed to be quite scriptable via the options, you can always write helper scripts for advanced features. You can check out the ones I am writing [here](https://github.com/darrenldl/osbx-helpers).

CRC-CCITT is currently implemented in pure OCaml and is translated from the implementation in [libcrc](https://github.com/lammertb/libcrc)
  - See src/crcccitt.ml, src/crcccitt.mli for the OCaml implementation
    - The translated source code is under the same MIT license used by and stated in libcrc source code
  - See libcrc\_crcccitt/crcccitt.c, libcrc\_crcccitt/checksum.h for the source code from libcrc used for the translation

Exact behaviours in non-standard cases are not specified in official SeqBox technical specification
  - See [specification](SPECS.md) of ocaml-SeqBox for details on how ocaml-SeqBox behaves(if you care about undefined behaviour those sort of things)

Hashing libraries
  - Nocrypto for SHA1, SHA256, SHA512
  - Digestif for BLAKE2B\_512

## Tips
See [wiki page](https://github.com/darrenldl/ocaml-SeqBox/wiki/Tips)

## Gotchas
See [wiki page](https://github.com/darrenldl/ocaml-SeqBox/wiki/Gotchas)

## Helpers
See [helpers](https://github.com/darrenldl/osbx-helpers)

## Links
[Wiki](https://github.com/darrenldl/ocaml-SeqBox/wiki)

[Index of source code](INDEX.md)

[Changelog](CHANGELOG.md)

[Todo/wishlist](TODO.md)

## Possibly useful additional features of ocaml-SeqBox(possibly not yet in official SeqBox)
  - Allows random ordering in sbx container
    - This also means block corruption will not stop the decoding process
  - Allows duplicate metadata/data blocks to exist within one sbx container
    - This means you can concatenate multiple copies of sbx container together directly to increase chance of recovery in case of corruption

## Technical Specification
The following specification is copied directly from the official specification (with possible slight modifications).

Also see section "Features currently NOT planned to be implemented" for features ocaml-SeqBox is probably not going to have.

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
| HSH | crypto hash (using [Multihash](http://multiformats.io) protocol) |
| PID | parent UID (*not used at the moment*)|

Supported crypto hashes since 1.1.0 are
  - SHA1
  - SHA256
  - SHA512
  - BLAKE2B\_512

#### Features currently NOT planned to be implemented
  - Data hiding (XOR encoding/decoding in official seqbox)
    - Provides neither sufficiently strong encryption nor sufficient stealth for any serious attempt to hide/secure data
    - You should use the appropriate tools for encryption

## License
The following files directly from [libcrc](https://github.com/lammertb/libcrc) are under the MIT License(see license text in each of the code files)
  - libcrc\_crcccitt/crcccitt.c
  - libcrc\_crcccitt/checksum.h

The following files translated from libcrc source code are under the same MIT License as used by libcrc and as stated in libcrc source code, the license text of the crcccitt.c is copied over to src/crcccitt.ml as well
  - src/crcccitt.ml
  - src/crcccitt.mli

The files in tests folder copied from official SeqBox are under its license, which is MIT as of time of writing
  - tests/SeqBox/*

All remaining files are distributed under the 3-Clause BSD license as stated in the LICENSE file
