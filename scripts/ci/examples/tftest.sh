#!/usr/bin/env bash

set -o nounset
set -o pipefail
set -o errexit

pushd "$(dirname $(basename "${0}"))/examples/s3-full-access-policy" > /dev/null

make test 1> /dev/null

popd > /dev/null
