#!/bin/sh

cd packer/$BUILD

# move SSH key into place for packer upload
echo "move $SSH_KEY_FILE --> uploads/id_rsa"
if [ -z "$SSH_KEY_FILE" ]; then
  echo "no key file found, not uploading"
else
  mv $SSH_KEY_FILE uploads/id_rsa
fi

cat <<EOT > variables.json
{
    "ami_name": "$NAME",
    "region": "$REGION",
    "vpc_id": "$VPC_ID",
    "subnet_id": "$SUBNET_ID",
    "source_ami": "$AMI",
    "instance_type": "$INSTANCE_TYPE",
    "description": "$DESCRIPTION",
    "os_version": "$OS_VERSION",
    "os_release": "$OS_RELEASE",
    "build_id": "$BUILD_TAG",
    "copy_to_a": "$COPY_AMI_TO_A",
    "copy_to_b": "$COPY_AMI_TO_B"
}
EOT
packer build -var-file=variables.json base-host.json
