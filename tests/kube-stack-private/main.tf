provider "aws" {
  region = "${var.region}"
}

resource "aws_key_pair" "main" {
  key_name   = "${var.name}"
  public_key = "${file(var.ssh_pubkey)}"
}

module "vpc" {
  source               = "../../tf-modules/vpc-scenario-2"
  region               = "${var.region}"
  cidr                 = "${var.vpc_cidr}"
  name_prefix          = "${var.name}"
  #extra_tags           = "${var.extra_tags}"
  public_subnet_cidrs  = ["${var.public_subnet_cidrs}"]
  private_subnet_cidrs = ["${var.private_subnet_cidrs}"]
  azs = ["${slice(data.aws_availability_zones.available.names, 0, 2)}"]
}

# shared security group for public access over SSH
module "public-ssh-sg" {
  source              = "../../tf-modules/ssh-sg"
  name                = "${var.name}-public"
  vpc_id              = "${module.vpc.vpc_id}"
  allowed_cidr_blocks = "0.0.0.0/0"
}
# shared security group for SSH - for private subnet (access from VPC)
module "private-ssh-sg" {
  source              = "../../tf-modules/ssh-sg"
  name                = "${var.name}-private"
  vpc_id              = "${module.vpc.vpc_id}"
  allowed_cidr_blocks = "${var.vpc_cidr}"
}
# shared security group, open ingress (inbound to nodes), for kube workers
module "open-ingress-sg" {
  source              = "../../tf-modules/open-ingress-sg"
  name_prefix         = "${var.name}"
  vpc_id              = "${module.vpc.vpc_id}"
  allowed_cidr_blocks = "${var.vpc_cidr}"
}
# shared security group, open egress (outbound from nodes), use in public subnet
module "open-egress-sg" {
  source = "../../tf-modules/open-egress-sg"
  name   = "${var.name}"
  vpc_id = "${module.vpc.vpc_id}"
}
