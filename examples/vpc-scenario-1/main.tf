/**
 * ## Example to test the VPC Scenario 1 Module
 *
 *
 */
variable "extra_tags" {
  description = "Extra tags that will be added to aws_subnet resources"
  default     = {}
}

variable "name" {
  description = "name of the project, use as prefix to names of resources created"
  default     = "test-project"
}

variable "region" {
  description = "Region where the project will be deployed"
  default     = "us-east-2"
}

variable "vpc_cidr" {
  description = "Top-level CIDR for the whole VPC network space"
  default     = "10.23.0.0/16"
}

variable "ssh_pubkey" {
  description = "File path to SSH public key"
  default     = "./id_rsa.pub"
}

variable "ssh_key" {
  description = "File path to SSH public key"
  default     = "./id_rsa"
}

variable "public_subnet_cidrs" {
  default     = ["10.23.11.0/24", "10.23.12.0/24", "10.23.13.0/24"]
  description = "A list of public subnet CIDRs to deploy inside the VPC"
}

provider "aws" {
  region = "${var.region}"
}

data "aws_availability_zones" "available" {}

module "vpc" {
  source      = "../../modules/vpc-scenario-1"
  name_prefix = "${var.name}"
  region      = "${var.region}"
  cidr        = "${var.vpc_cidr}"
  azs         = ["${slice(data.aws_availability_zones.available.names, 0, 3)}"]

  extra_tags = {
    kali = "ma"
  }

  public_subnet_cidrs = ["${var.public_subnet_cidrs}"]
}

module "ubuntu-xenial-ami" {
  source  = "../../modules/ami-ubuntu"
  release = "14.04"
}

resource "aws_key_pair" "main" {
  key_name   = "${var.name}"
  public_key = "${file(var.ssh_pubkey)}"
}

module "web-sg" {
  source      = "../../modules/security-group-base"
  description = "For my-web-app instances in ${var.name}"
  name        = "${var.name}-web"
  vpc_id      = "${module.vpc.vpc_id}"
}

# shared security group for SSH
module "web-public-ssh-rule" {
  source              = "../../modules/ssh-sg"
  security_group_id   = "${module.web-sg.id}"
}

# shared security group, open egress (outbound from nodes)
module "web-open-egress-rule" {
  source = "../../modules/open-egress-sg"

  security_group_id = "${module.web-sg.id}"
}

resource "aws_instance" "web" {
  ami               = "${module.ubuntu-xenial-ami.id}"
  count             = "${length(var.public_subnet_cidrs)}"
  key_name          = "${aws_key_pair.main.key_name}"
  instance_type     = "t2.nano"
  availability_zone = "${data.aws_availability_zones.available.names[count.index]}"

  root_block_device {
    volume_type = "gp2"
    volume_size = "8"
  }

  associate_public_ip_address = "true"
  vpc_security_group_ids      = ["${module.web-sg.id}"]

  subnet_id = "${element(module.vpc.public_subnet_ids, count.index)}"

  tags {
    Name = "${var.name}-web-${count.index}"
  }

  user_data = <<END_INIT
#!/bin/bash
echo "hello!"
wget -O kops https://github.com/kubernetes/kops/releases/download/1.6.0-beta.1/kops-linux-amd64
chmod +x kops
mv kops /usr/local/bin/
kops version
END_INIT

  provisioner "remote-exec" {
    inline = [
      "echo 'success!'",
      "lsb_release -a",
      "kops version",
      "tail /var/log/cloud-init-output.log",
    ]

    connection {
      type        = "ssh"
      user        = "ubuntu"
      private_key = "${file(var.ssh_key)}"
    }
  }
}
