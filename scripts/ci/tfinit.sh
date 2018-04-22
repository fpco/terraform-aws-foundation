#/bin/bash

set -o nounset
set -o pipefail
set -o errexit

pushd "$(dirname $(basename "${0}"))/tests" > /dev/null

terraform init 1> /dev/null

popd > /dev/null
