#!/usr/bin/env bash

set -o nounset
set -o pipefail
set -o errexit

pushd "$(dirname $(basename "${0}"))/tests" > /dev/null

terraform init 1> /dev/null

popd > /dev/null

pushd "$(dirname $(basename "${0}"))/examples/s3-full-access-policy" > /dev/null

terraform init 1> /dev/null

popd > /dev/null
