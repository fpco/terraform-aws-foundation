/**
 * ## Cloud Dev Workspace on AWS
 * 
 * Run a dev workspace on a single EC2 instance.
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
 * e2label /dev/xvdf1 data
 *
 * After running the above code to initialise the EBS, terminate the instance
 * and the autoscaling group will bring up a new instance that will be running
 * gitlab once it is done initialising.
 * 
 */

variable "name_prefix" {
  description = ""
  type        = "string"
}

variable "region" {
  default = "us-east-2"
  type    = "string"
}

variable "instance_type" {
  description = "the type of EC2 instance"
  default     = "t2.small"
  type        = "string"
}

variable "ssh_pubkey" {
  description = "The path to the SSH pub key to use"
  default     = "./id_rsa.pub"
}

variable "dns_zone_name" {
  description = "The name of the DNS zone on Route53, to create records in for the workspace"
  type        = "string"
}

provider "aws" {
  region = "${var.region}"
}

data "aws_availability_zones" "available" {}

module "vpc" {
  source              = "../../tf-modules/vpc-scenario-1"
  azs                 = ["${slice(data.aws_availability_zones.available.names, 0, 1)}"]
  name_prefix         = "${var.name_prefix}-workspace"
  cidr                = "192.168.0.0/24"
  public_subnet_cidrs = ["192.168.0.0/24"]
  region              = "${var.region}"
}

resource "aws_key_pair" "main" {
  key_name   = "${var.name_prefix}"
  public_key = "${file(var.ssh_pubkey)}"
}

module "ubuntu-xenial-ami" {
  source  = "../../tf-modules/ami-ubuntu"
  release = "16.04"
}

module "init-install-awscli" {
  source = "../../tf-modules/init-snippet-install-awscli"
}

module "init-install-ops" {
  source = "../../tf-modules/init-snippet-install-ops"
}

# boxed module to attach the EBS volume to the node
module "init-attach-ebs" {
  source    = "../../tf-modules/init-snippet-attach-ebs-volume"
  region    = "${var.region}"
  volume_id = "${module.workspace-data.volume_id}"
}

module "workspace-data" {
  source      = "../../tf-modules/persistent-ebs"
  name_prefix = "${var.name_prefix}-workspace-data"
  region      = "${var.region}"
  az          = "${element(data.aws_availability_zones.available.names, 0)}"
  size        = "20"
  volume_type = "gp2"
  encrypted   = false

  # kms_key_id  = "${var.data_volume_kms_key_id}"
  # snapshot_id = "${var.data_volume_snapshot_id}"
}

##################
## Security Group

resource "aws_security_group" "workspace" {
  name        = "${var.name_prefix}-cloud-workspace"
  vpc_id      = "${module.vpc.vpc_id}"
  description = "Security group for the cloud workspace for ${var.name_prefix}"
}

module "ssh-rule" {
  source            = "../../tf-modules/ssh-sg"
  cidr_blocks       = ["0.0.0.0/0"]
  security_group_id = "${aws_security_group.workspace.id}"
}

module "sandstorm-rule" {
  source            = "../../tf-modules/single-port-sg"
  port              = 6080
  description       = "Allow ingress for sandcats HTTP, port 6080 (TCP)"
  cidr_blocks       = ["0.0.0.0/0"]
  security_group_id = "${aws_security_group.workspace.id}"
}

module "https-rule" {
  source            = "../../tf-modules/single-port-sg"
  port              = 443
  description       = "Allow ingress for HTTPS, port 443 (TCP)"
  cidr_blocks       = ["0.0.0.0/0"]
  security_group_id = "${aws_security_group.workspace.id}"
}

module "open-egress-rule" {
  source            = "../../tf-modules/open-egress-sg"
  security_group_id = "${aws_security_group.workspace.id}"
}

##################
## EC2 instance

resource "aws_instance" "workspace" {
  ami             = "${module.ubuntu-xenial-ami.id}"
  instance_type   = "${var.instance_type}"
  subnet_id       = "${module.vpc.public_subnet_ids[0]}"
  key_name        = "${aws_key_pair.main.key_name}"
  security_groups = ["${aws_security_group.workspace.id}"]

  iam_instance_profile        = "${module.workspace-data.iam_profile}"
  associate_public_ip_address = true

  tags = {
    Name = "${var.name_prefix}-workspace"
  }

  user_data = <<END_INIT
#!/bin/bash
apt-get update
${module.init-install-awscli.init_snippet}
${module.init-install-ops.init_snippet}
${module.init-attach-ebs.init_snippet}
if [ -f /dev/xvdf1 ] ; then
  parted --script /dev/xvdf -- mklabel msdos
  parted --script /dev/xvdf -- mkpart primary 0 -1
  mkfs -t ext4 -F /dev/xvdf1
  e2label /dev/xvdf1 data
fi

mkdir -p /data
mount /dev/xvdf1 /data
cp /etc/fstab /etc/fstab.orig
echo "LABEL=data            /data  ext4   defaults,nofail     0 2" >> /etc/fstab

# install saltstack and bootstrap CM formula
wget -O - https://raw.githubusercontent.com/fpco/bootstrap-salt-formula/master/simple-bootstrap.sh | sh

#####
mkdir /home/ubuntu/bin
wget https://releases.hashicorp.com/terraform/0.10.2/terraform_0.10.2_linux_amd64.zip
wget https://releases.hashicorp.com/terraform/0.11.1/terraform_0.11.1_linux_amd64.zip

unzip terraform_0.11.1_linux_amd64.zip -d /home/ubuntu/bin/terraform-0.11.1
unzip terraform_0.10.2_linux_amd64.zip -d /home/ubuntu/bin/terraform-0.10.2

mkdir /data/sandstorm
ln -sf /data/sandstorm /opt/sandstorm
echo "install sandstorm with: curl https://install.sandstorm.io | bash"
END_INIT
}

##################
## DNS setup

data "aws_route53_zone" "selected" {
  name = "${var.dns_zone_name}"
}

resource "aws_route53_record" "workspace" {
  zone_id = "${data.aws_route53_zone.selected.zone_id}"
  name    = "${var.name_prefix}.${data.aws_route53_zone.selected.name}"
  type    = "CNAME"
  ttl     = "300"
  records = ["${aws_instance.workspace.public_dns}"]
}

##################
## Outputs

// region deployed to
output "region" {
  value = "${var.region}"
}

// name of the Gitlab autoscaling group
output "workspace_dns" {
  value = "${aws_route53_record.workspace.name}"
}
