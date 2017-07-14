## 1.0.1  (development version, not published yet)
  - Changed rescue mode to use the original bytes of the file, rather than regenerating the block bytes itself
  - Fixed jbuild file to work on macOS when building
  - Added graceful handling of CTRL-C breaks in rescue mode
    - log file stays valid after being interrupted
    - in 1.0.0, log file may be empty/broken when osbx is interrupted during rescuing
  - Better progress reporting for encoding, decoding and rescuing
  - Fixed progress reporting issues with rescuing
    - in 1.0.0, rescue mode only does a progress report when it outputs a block, rather than after scanning the block bytes
    - this makes osbx outputs nothing when the scanned file is large and contains no valid blocks
  - Fixed log writing with rescuing
    - in 1.0.0, rescue mode only writes to log file when a valid block is found
    - now it is changed to update log on bytes read, not on blocks written
  - Updated file size retrieval function to act correctly when dealing with block devices
    - Changed File\_utils.getsize to use Core.In\_channel.length instead of Unix.LargeFile.stat
  - Added progress reporting for hash in decode mode

## 1.0.0  (version on opam)
  - Base version
