#!/usr/bin/env bash

set -xu

# run CI scripts from within the fpco-terraform-aws git repo
cd $WORKSPACE/fpco-terraform-aws
# auto-generate the docs for each of the TF modules in this repo
./scripts/generate-api-docs.sh
# use mkdocs to render the docs
./scripts/build-docs.sh
# package them with tar to be saved as an artifact
./scripts/package-docs.sh
