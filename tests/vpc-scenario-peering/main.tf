/**
 * ## Run Tests on the VPC Peering Scenario
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
  default     = "us-east-1"
}

variable "vpc1_cidr" {
  description = "Top-level CIDR for the whole VPC network space"
  default     = "10.120.0.0/16"
}

variable "vpc2_cidr" {
  description = "Top-level CIDR for the whole VPC network space"
  default     = "10.130.0.0/16"
}

variable "vpc1_public_subnet_cidrs" {
  description = "list of CIDR ranges for the public subnets"
  default     = ["10.120.10.0/24", "10.120.11.0/24", "10.120.12.0/24"]
}

variable "vpc2_public_subnet_cidrs" {
  description = "list of CIDR ranges for the public subnets"
  default     = ["10.130.10.0/24", "10.130.11.0/24", "10.130.12.0/24"]
}

variable "aws_availability_zones" {
  default     = ["us-east-1a", "us-east-1c", "us-east-1d"]
  description = "List of availability zones to use. Should match number of CIDR blocks"
}

variable "ssh_pubkey" {
  description = "File path to SSH public key"
  default     = "./id_rsa.pub"
}

variable "ssh_key" {
  description = "File path to SSH public key"
  default     = "./id_rsa"
}

provider "aws" {
  region = "${var.region}"
}

module "ubuntu-xenial-ami" {
  source  = "../../tf-modules/ami-ubuntu"
  release = "14.04"
}

resource "aws_key_pair" "main" {
  key_name   = "${var.name}"
  public_key = "${file(var.ssh_pubkey)}"
}

module "vpc1" {
  source      = "../../tf-modules/vpc"
  name_prefix = "${var.name}-vpc1"
  region      = "${var.region}"
  cidr        = "${var.vpc1_cidr}"
}

module "vpc2" {
  source      = "../../tf-modules/vpc"
  name_prefix = "${var.name}-vpc2"
  region      = "${var.region}"
  cidr        = "${var.vpc2_cidr}"
}

module "vpc1-public-subnets" {
  source      = "../../tf-modules/subnets"
  azs         = "${var.aws_availability_zones}"
  vpc_id      = "${module.vpc1.vpc_id}"
  name_prefix = "${var.name}-vpc1-public"
  cidr_blocks = "${var.vpc1_public_subnet_cidrs}"
  extra_tags  = "${var.extra_tags}"
}

module "vpc2-public-subnets" {
  source      = "../../tf-modules/subnets"
  azs         = "${var.aws_availability_zones}"
  vpc_id      = "${module.vpc2.vpc_id}"
  name_prefix = "${var.name}-vpc2-public"
  cidr_blocks = "${var.vpc2_public_subnet_cidrs}"
  extra_tags  = "${var.extra_tags}"
}

module "vpc1-sg" {
  source      = "../../tf-modules/security-group-base"
  description = "Test project security group"
  name        = "${var.name}-vpc1-sg"
  vpc_id      = "${module.vpc1.vpc_id}"
}

module "vpc2-sg" {
  source      = "../../tf-modules/security-group-base"
  description = "Test project security group"
  name        = "${var.name}-vpc2-sg"
  vpc_id      = "${module.vpc2.vpc_id}"
}

module "vpc1-open-ssh" {
  source = "../../tf-modules/ssh-sg"

  # this is actually used as a name-prefix
  security_group_id = "${module.vpc1-sg.id}"
}

module "vpc1-open-egress" {
  source = "../../tf-modules/open-egress-sg"

  # this is actually used as a name-prefix
  security_group_id = "${module.vpc1-sg.id}"
}

module "vpc2-open-ssh" {
  source = "../../tf-modules/ssh-sg"

  # this is actually used as a name-prefix
  security_group_id = "${module.vpc2-sg.id}"
}

module "vpc2-open-egress" {
  source = "../../tf-modules/open-egress-sg"

  # this is actually used as a name-prefix
  security_group_id = "${module.vpc2-sg.id}"
}

module "vpc1-public-gateway" {
  source            = "../../tf-modules/route-public"
  vpc_id            = "${module.vpc1.vpc_id}"
  name_prefix       = "${var.name}-vpc1-public"
  extra_tags        = "${var.extra_tags}"
  public_subnet_ids = ["${concat(module.vpc1-public-subnets.ids)}"]
}

module "vpc2-public-gateway" {
  source            = "../../tf-modules/route-public"
  vpc_id            = "${module.vpc2.vpc_id}"
  name_prefix       = "${var.name}-vpc2-public"
  extra_tags        = "${var.extra_tags}"
  public_subnet_ids = ["${concat(module.vpc2-public-subnets.ids)}"]
}

# Peering connection for vpc1 to vpc2 communication
resource "aws_vpc_peering_connection" "vpc1-to-vpc2" {
  peer_vpc_id = "${module.vpc2.vpc_id}"
  vpc_id      = "${module.vpc1.vpc_id}"
  auto_accept = true

  tags {
    Name = "${var.name}-vpc1-to-vpc2"
  }
}

resource "aws_route" "vpc1-to-vpc2" {
  route_table_id            = "${module.vpc1-public-gateway.route_table_id}"
  destination_cidr_block    = "${module.vpc2.vpc_cidr_block}"
  vpc_peering_connection_id = "${aws_vpc_peering_connection.vpc1-to-vpc2.id}"
}

resource "aws_route" "vpc2-to-vpc1" {
  route_table_id            = "${module.vpc2-public-gateway.route_table_id}"
  destination_cidr_block    = "${module.vpc1.vpc_cidr_block}"
  vpc_peering_connection_id = "${aws_vpc_peering_connection.vpc1-to-vpc2.id}"
}

resource "aws_instance" "vpc1-machine" {
  ami               = "${module.ubuntu-xenial-ami.id}"
  count             = "1"
  key_name          = "${aws_key_pair.main.key_name}"
  instance_type     = "t2.nano"
  availability_zone = "${element(var.aws_availability_zones, 0)}"

  root_block_device {
    volume_type = "gp2"
    volume_size = "8"
  }

  associate_public_ip_address = "true"
  vpc_security_group_ids      = ["${module.vpc1-sg.id}"]
  subnet_id                   = "${element(module.vpc1-public-subnets.ids, count.index)}"

  tags {
    Name = "${var.name}-vpc1-machine-${count.index}"
  }

  provisioner "remote-exec" {
    inline = [
      "echo 'success!'",
      "lsb_release -a",
      "tail /var/log/cloud-init-output.log",
    ]

    connection {
      type        = "ssh"
      user        = "ubuntu"
      private_key = "${file(var.ssh_key)}"
    }
  }
}

resource "aws_instance" "vpc2-machine" {
  ami               = "${module.ubuntu-xenial-ami.id}"
  count             = "1"
  key_name          = "${aws_key_pair.main.key_name}"
  instance_type     = "t2.nano"
  availability_zone = "${element(var.aws_availability_zones, 0)}"

  root_block_device {
    volume_type = "gp2"
    volume_size = "8"
  }

  associate_public_ip_address = "true"
  vpc_security_group_ids      = ["${module.vpc2-sg.id}"]
  subnet_id                   = "${element(module.vpc2-public-subnets.ids, count.index)}"

  tags {
    Name = "${var.name}-vpc2-machine-${count.index}"
  }

  provisioner "remote-exec" {
    inline = [
      "echo 'success!'",
      "lsb_release -a",
      "tail /var/log/cloud-init-output.log",
    ]

    connection {
      type        = "ssh"
      user        = "ubuntu"
      private_key = "${file(var.ssh_key)}"
    }
  }
}
