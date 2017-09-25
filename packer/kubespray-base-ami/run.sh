#!/bin/bash

docker run --privileged --net=host --rm \
    -v /tmp/kube-packer-base-image-temp/vars.yml:/opt/kubespray/vars.yml \
    -v /tmp/kube-packer-base-image-temp/id_rsa:/tmp/id_rsa \
    -v /var/run/docker.sock:/var/run/docker.sock \
    -v /usr/bin/docker:/usr/bin/docker \
    -v /run/metadata:/run/metadata \
    -v /run/torcx:/run/torcx \
    -v /tmp/releases:/tmp/releases \
    -e ANSIBLE_SSH_ARGS="-i /tmp/id_rsa" \
    -e ANSIBLE_HOST_KEY_CHECKING=False \
    -e PYTHONUNBUFFERED=True \
    fpco/kubespray ansible-playbook \
        -u core -b -i inventory/localhost \
        --extra-vars="@vars.yml" \
        --tags="bootstrap-os,adduser,download" \
        cluster.yml

