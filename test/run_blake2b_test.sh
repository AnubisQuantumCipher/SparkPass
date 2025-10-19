#!/bin/bash
./bin/test_blake2b_vectors 2>/dev/null
EXIT_CODE=$?
echo "Exit code: $EXIT_CODE"
exit $EXIT_CODE
