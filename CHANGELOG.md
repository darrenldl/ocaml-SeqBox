## 1.0.1  (development version, not published yet)
  - Changed rescue mode to use the original bytes of the file, rather than regenerating the block bytes itself
  - Fixed jbuild file to work on macOS when building
  - Added graceful handling of CTRL-C breaks in rescue mode
    - log file stays valid after being interrupted
    - in 1.0.0, log file may be empty/broken when osbx is interrupted during rescuing
  - Better progress reporting for encoding, decoding and rescuing

## 1.0.0  (version on opam)
  - Base version
