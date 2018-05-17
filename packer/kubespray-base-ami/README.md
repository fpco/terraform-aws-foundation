# Instructions

Generate new keypair for the Packer user

    ssh-keygen -q -t rsa -N '' -f id_rsa

Import into AWS using this command:

    aws-env -p admin aws import-key-pair --key-name Packer-Kubespray-Builder --region us-east-1 --public-key-material "$(cat id_rsa.pub)"

`NOTE`: Make sure to change the aws profile as well as `--region` and `--key-name` accordingly.

Create the necessary `packer.vars` file:

    cat << EOF > packer.vars
    {
        "aws_vpc_id": "vpc-xxxx",
        "aws_subnet_id": "subnet-xxx",
        "docker_release_tag": "latest"
    }
    EOF

You have to override at least `aws_vpc_id` and `aws_subnet_id`. See
`packer.json` for list of all variables.

`NOTE`: You'll probably want to override `aws_region`, `ami_owner` and `ami_image_name_regex`.

To run issue the following command:

    aws-env -p admin packer build -var-file=packer.vars packer.json

`NOTE`: Make sure to use the correct aws-env profile

`NOTE`: To add a description to the AMI use `-var 'aws_ami_description=Description goes here'`.

Every instance has to invoke this as part of their cloudinit as the docker provisioner
need to mount the sshkey to be able to ssh into localhost.

        "cd /home/core/.ssh",
        "ssh-keygen -q -t rsa -N '' -f id_rsa",
        "chmod 600 id_rsa"
        "cat id_rsa.pub > authorized_keys"

