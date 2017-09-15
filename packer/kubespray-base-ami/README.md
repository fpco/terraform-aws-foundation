# Instructions

Generate new keypair for the Packer user

    ssh-keygen -q -t rsa -N '' -f id_rsa

`NOTE`: You have to upload this public key manually through the aws console


Change the desired packer variables and run:

    aws-env -p admin packer build packer.json

`NOTE`: Use -var 'key=value' to override packer variables

Every instance has to invoke this as part of their cloudinit as the docker provisioner
need to mount the sshkey to be able to ssh into localhost.

        "cd /home/core/.ssh",
        "ssh-keygen -q -t rsa -N '' -f id_rsa",
        "chmod 600 id_rsa"
        "cat id_rsa.pub > authorized_keys"

