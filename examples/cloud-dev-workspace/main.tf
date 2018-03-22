provider "aws" {
  region = "${var.region}"
}

data "aws_availability_zones" "available" {}

module "vpc" {
  source              = "../../modules/vpc-scenario-1"
  azs                 = ["${slice(data.aws_availability_zones.available.names, 0, 1)}"]
  name_prefix         = "${var.name_prefix}-${var.name_suffix}"
  cidr                = "${var.vpc_cidr}"
  public_subnet_cidrs = ["${var.vpc_cidr}"]
  region              = "${var.region}"
  extra_tags          = "${var.extra_tags}"
}

resource "aws_key_pair" "main" {
  key_name   = "${var.name_prefix}-${var.name_suffix}"
  public_key = "${file(var.ssh_pubkey)}"
}

module "ubuntu-xenial-ami" {
  source  = "../../modules/ami-ubuntu"
  release = "16.04"
}

module "init-install-awscli" {
  source = "../../modules/init-snippet-install-awscli"
}

module "init-install-ops" {
  source = "../../modules/init-snippet-install-ops"
}

# boxed module to attach the EBS volume to the node
module "init-attach-ebs" {
  source    = "../../modules/init-snippet-attach-ebs-volume"
  region    = "${var.region}"
  volume_id = "${module.workspace-data.volume_id}"
}

module "workspace-data" {
  source      = "../../modules/persistent-ebs"
  name_prefix = "${var.name_prefix}-${var.name_suffix}-data"
  region      = "${var.region}"
  az          = "${element(data.aws_availability_zones.available.names, 0)}"
  size        = "20"
  volume_type = "gp2"
  encrypted   = false
  extra_tags  = "${var.extra_tags}"

  # kms_key_id  = "${var.data_volume_kms_key_id}"
  # snapshot_id = "${var.data_volume_snapshot_id}"
}

##################
## Security Group

module "workspace-sg" {
  source      = "../../modules/security-group-base"
  name        = "${var.name_prefix}-${var.name_suffix}"
  vpc_id      = "${module.vpc.vpc_id}"
  description = "Security group for the ${var.name_prefix} cloud workspace"
  extra_tags  = "${var.extra_tags}"
}

module "ssh-rule" {
  source            = "../../modules/ssh-sg"
  cidr_blocks       = ["0.0.0.0/0"]
  security_group_id = "${module.workspace-sg.id}"
}

module "open-egress-rule" {
  source            = "../../modules/open-egress-sg"
  security_group_id = "${module.workspace-sg.id}"
}

##################
## EC2 instance

resource "aws_instance" "workspace" {
  ami             = "${module.ubuntu-xenial-ami.id}"
  instance_type   = "${var.instance_type}"
  subnet_id       = "${module.vpc.public_subnet_ids[0]}"
  key_name        = "${aws_key_pair.main.key_name}"
  security_groups = ["${module.workspace-sg.id}"]

  iam_instance_profile        = "${module.workspace-data.iam_profile_id}"
  associate_public_ip_address = true

  tags = "${merge(map("Name", "${var.name_prefix}-${var.name_suffix}"), "${var.extra_tags}")}"

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
${var.init_suffix}
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
