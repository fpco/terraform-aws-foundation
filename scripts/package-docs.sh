#!/usr/bin/env bash

set -o nounset
set -x

# WORKSPACE must be defined
if [ -z "${WORKSPACE-}" ]; then
  echo "ERROR: please define \$WORKSPACE , where git repos are checked out"
  exit 1
fi

MKDOCS_SITE=$WORKSPACE/fpco-terraform-aws/site
ARTIFACTS=$WORKSPACE/artifacts
TARFILE=fpco-ops-platform-docs.tar.gz
mkdir -p $ARTIFACTS

# package up the rendered docs, we'll save this as a CI artifact
# and also drop it in a docker image for running later
tar czvf $ARTIFACTS/$TARFILE -C $MKDOCS_SITE ./

