#!/usr/bin/env bash

set -ex

PACKER_TEMPLATE="$(pwd)/base-host.json"
AMI_BUILD_TEMP="$(pwd)/example-build-config.tpl"
AMI_BUILD_CONF="$(pwd)/example-build-config.json"

# variables to combine with the build config
# lookup the first few in our terraform build env
cd ../../examples/terraform-vpc
export SOURCE_AMI="$(terraform output trusty_ami_id)"
export VPC_ID="$(terraform output vpc_id)"
export SUBNET_ID="$(terraform output subnet_id)"
export REGION="$(terraform output region)"
# copy the AMI to the other regions, if desired
# move this back in later:
#      "ami_regions": ["{{user `copy_to_a` }}", "{{user `copy_to_b` }}"],
export COPY_TO_A="us-west-2"
export COPY_TO_B="us-east-1"
# hard-code the rest
export INSTANCE_TYPE="t2.small"
export DESC="Stock Ubuntu 14.04 + FP Deploy base"
export OS_VERSION="Ubuntu Trusty"
export RELEASE="14.04 LTS"
# fill these in
export BUILD_ID=""
export GIT_REV=""

envsubst <$AMI_BUILD_TEMP > $AMI_BUILD_CONF
cat $AMI_BUILD_CONF

cd $PACKER_BASE_PATH
if [ -f uploads/id_rsa ] ; then
  ssh-keygen -y -f uploads/id_rsa
  chmod 600 uploads/id_rsa
else
  echo "no id_rsa, be sure that's ok?"
fi
packer build -var-file=$AMI_BUILD_CONF $PACKER_TEMPLATE
