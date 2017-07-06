#!/bin/bash

multiplier=4

cp dummy_file_encoded dummy_file_encoded.backup

for ((i = 1; i < $multiplier; i++)); do
  cat dummy_file_encoded.backup >> dummy_file_encoded
done
