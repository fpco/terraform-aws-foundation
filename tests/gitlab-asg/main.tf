/**
 * ## Test Gitlab on a Single-Node ASG
 * 
 * Run a Gitlab on a single EC2 instance.
 * This instance will be part of a single-node autoscaling group
 * that shares an EBS volume to store data.
 *
 * Note that there is a peculiarity with the EBS volume in that it
 * requires some manual setup the very first time to make it available
 * for use (unless a snapshot id is supplied):
 *
 * parted --script /dev/xvdf -- mklabel msdos
 * parted --script /dev/xvdf -- mkpart primary 0 -1
 * mkfs -t ext4 -F /dev/xvdf1
 * e2label /dev/xvdf1 gitlab
 *
 * After running the above code to initialise the EBS, terminate the instance
 * and the autoscaling group will bring up a new instance that will be running
 * gitlab once it is done initialising.
 * 
 */

variable "name" {
  default = "gitlab-asg-test"
}

variable "region" {
  default = "us-east-1"
}

variable "ssh_pubkey" {
  description = "The path to the SSH pub key to use"
}

provider "aws" {
  region = "${var.region}"
}

data "aws_availability_zones" "available" {}

module "ubuntu-xenial-ami" {
  source  = "../../tf-modules/ami-ubuntu"
  release = "16.04"
} 

resource "aws_key_pair" "main" {
  key_name   = "${var.name}"
  public_key = "${file(var.ssh_pubkey)}"
}

module "gitlab-asg" {
  source        = "../../tf-modules/single-node-asg"
  name_prefix   = "${var.name}"
  name_suffix   = "gitlab-server"
  region        = "${var.region}"
  az            = "${element(data.aws_availability_zones.available.names, 0)}"
  key_name      = "${aws_key_pair.main.key_name}"
  ami           = "${module.ubuntu-xenial-ami.id}"
  instance_type = "t2.medium"
  subnet_id     = "${module.vpc.public_subnet_ids[0]}"

  security_group_ids    = ["${aws_security_group.gitlab.id}"]
  data_volume_encrypted = false

  init_prefix = <<END_INIT
apt-get update
${module.init-install-awscli.init_snippet}
${module.init-install-ops.init_snippet}
END_INIT

  init_suffix = <<END_INIT
mkdir -p /gitlab
mount /dev/xvdf1 /gitlab

cp /etc/fstab /etc/fstab.orig
echo "LABEL=gitlab            /gitlab  ext4   defaults,nofail     0 2" >> /etc/fstab

apt-get install -y docker docker.io
${module.init-gitlab-docker.init_snippet}
END_INIT
}

module "init-install-awscli" {
  source = "../../tf-modules/init-snippet-install-awscli"
}

module "init-install-ops" {
  source = "../../tf-modules/init-snippet-install-ops"
}

module "init-gitlab-docker" {
  source = "../../tf-modules/init-snippet-gitlab-docker"
}

module "vpc" {
  source              = "../../tf-modules/vpc-scenario-1"
  azs                 = ["${slice(data.aws_availability_zones.available.names, 0, 1)}"]
  name_prefix         = "${var.name}"
  cidr                = "192.168.0.0/16"
  public_subnet_cidrs = ["192.168.0.0/16"]
  region              = "${var.region}"
}

resource "aws_security_group" "gitlab" {
  name        = "gitlab-asg"
  vpc_id      = "${module.vpc.vpc_id}"
  description = "Security group for the single-node autoscaling group"
}

module "ssh-rule" {
  source            = "../../tf-modules/ssh-sg"
  cidr_blocks       = ["0.0.0.0/0"]
  security_group_id = "${aws_security_group.gitlab.id}"
}

module "http-rule" {
  source            = "../../tf-modules/single-port-sg"
  port              = 80
  description       = "Allow ingress for HTTP, port 80 (TCP)"
  cidr_blocks       = ["0.0.0.0/0"]
  security_group_id = "${aws_security_group.gitlab.id}"
}

module "https-rule" {
  source            = "../../tf-modules/single-port-sg"
  port              = 443
  description       = "Allow ingress for HTTPS, port 443 (TCP)"
  cidr_blocks       = ["0.0.0.0/0"]
  security_group_id = "${aws_security_group.gitlab.id}"
}

module "gitlab-ssh-rule" {
  source            = "../../tf-modules/single-port-sg"
  port              = 8022
  description       = "Allow ingress for Git over SSH, port 8022 (TCP)"
  cidr_blocks       = ["0.0.0.0/0"]
  security_group_id = "${aws_security_group.gitlab.id}"
}

module "open-egress-rule" {
  source            = "../../tf-modules/open-egress-sg"
  security_group_id = "${aws_security_group.gitlab.id}"
}

// region deployed to
output "region" {
  value = "${var.region}"
}

// name of the Gitlab autoscaling group
output "gitlab_asg_name" {
  value = "${var.name}-gitlab-asg-${element(data.aws_availability_zones.available.names, 0)}"
}
