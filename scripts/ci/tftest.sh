#!/usr/bin/env bash

set -o nounset
set -o pipefail
set -o errexit

pushd "$(dirname $(basename "${0}"))/examples/s3-full-access-policy" > /dev/null

make init && make plan && make apply && make output 1> /dev/null
make test 1> /dev/null
make clean 1> /dev/null

popd > /dev/null
