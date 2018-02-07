resource "aws_instance" "bastion" {
  ami               = "${data.aws_ami.ubuntu-xenial.id}"
  key_name          = "${aws_key_pair.main.key_name}"
  instance_type     = "t2.nano"
  availability_zone = "${var.region}a"
  root_block_device {
    volume_type = "gp2"
    volume_size = "10"
  }
  associate_public_ip_address = "true"
  vpc_security_group_ids      = ["${module.public-ssh-sg.id}",
                                 "${module.open-egress-sg.id}"
  ]
  lifecycle = {
    ignore_changes = ["ami", "user_data"]
  }
  subnet_id = "${module.vpc.public_subnet_ids[0]}"
  tags {
    Name = "${var.name}-bastion"
  }
  user_data = <<END_INIT
#!/bin/bash
echo "hello!"
wget -O kops https://github.com/kubernetes/kops/releases/download/1.6.2/kops-linux-amd64
chmod +x kops
mv kops /usr/local/bin/
kops version
curl -LO https://storage.googleapis.com/kubernetes-release/release/$(curl -s https://storage.googleapis.com/kubernetes-release/release/stable.txt)/bin/linux/amd64/kubectl && \
chmod +x kubectl
mv kubectl /usr/local/bin/
kubectl version
END_INIT
}
