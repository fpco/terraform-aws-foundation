module "bastion-sg" {
  source      = "../../modules/security-group-base"
  name        = "${var.name}-bastion"
  vpc_id      = "${module.vpc.vpc_id}"
  description = "Security group for the basion hosts in ${var.name}"
}

module "bastion-public-ssh-rule" {
  source            = "../../modules/ssh-sg"
  cidr_blocks       = ["0.0.0.0/0"]
  description       = "Allow public ssh to bastion host in ${var.name}"
  security_group_id = "${module.bastion-sg.id}"
}

module "bastion-open-egress-rule" {
  source            = "../../modules/open-egress-sg"
  security_group_id = "${module.bastion-sg.id}"
}

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

  vpc_security_group_ids = [
    "${module.bastion-sg.id}",
  ]

  lifecycle = {
    ignore_changes = [
      "ami",
      "user_data",
    ]
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
