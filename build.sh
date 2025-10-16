#!/bin/bash
# SparkPass Build Script
# Wrapper for build-with-lacontext.sh
#
# This is the primary build script for SparkPass.
# It compiles all Ada/C sources + Objective-C LAContext helpers
# and links with Touch ID/Face ID support.

exec ./scripts/build-with-lacontext.sh "$@"
