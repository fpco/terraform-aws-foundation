#!/usr/bin/env bash

set -xu

SALT_DOC_DIR=$WORKSPACE/fpco-salt-formula
TF_DOC_DIR=$WORKSPACE/fpco-terraform-aws

cd $TF_DOC_DIR
# grab docs from fpco-salt-formula
rsync -avz $SALT_DOC_DIR/docs/fpco-salt-formula $TF_DOC_DIR/docs/saltstack/
# use mkdocs to build the project
mkdocs build --clean
