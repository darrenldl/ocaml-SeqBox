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

