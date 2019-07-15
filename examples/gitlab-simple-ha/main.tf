/**
 * ## Test Gitlab on a Single-Node ASG
 * 
 * Run a Gitlab on a single EC2 instance.
 * This instance will be part of a single-node autoscaling group
 * that shares an EBS volume to store data.
 *
 * 
 */

provider "aws" {
  region = "${var.region}"
}

data "aws_availability_zones" "available" {}

module "ubuntu-xenial-ami" {
  source  = "../../modules/ami-ubuntu"
  release = "16.04"
}

resource "aws_key_pair" "main" {
  key_name   = "${var.name}"
  public_key = "${file(var.ssh_pubkey)}"
}

# S3 bucket for the Docker Registry (running in gitlab) to store Docker Images
module "docker-registry-s3-storage" {
  source      = "../../modules/s3-remote-state"
  bucket_name = "${var.registry_bucket_name}"
  versioning  = "false"
  principals  = []
}

module "docker-registry-s3-full-access" {
  source       = "../../modules/s3-full-access-policy"
  name         = "${var.name}-docker-registry-s3-full-access"
  bucket_names = ["${module.docker-registry-s3-storage.bucket_id}"]
}

resource "aws_iam_role_policy_attachment" "s3-full-access-attachment" {
  role       = "${module.gitlab-asg.asg_iam_role_name}"
  policy_arn = "${module.docker-registry-s3-full-access.arn}"
}

resource "aws_eip" "gitlab" {
  vpc = true
}

resource "aws_iam_role_policy" "associate_eip" {
  role = "${module.gitlab-asg.asg_iam_role_name}"

  policy = <<POLICY
{
    "Version": "2012-10-17",
    "Statement": [
        {
            "Effect": "Allow",
            "Action": "ec2:AssociateAddress",
            "Resource": "*"
        }
    ]
}
POLICY
}

module "gitlab-asg" {
  source        = "../../modules/single-node-asg"
  name_prefix   = "${var.name}"
  name_suffix   = "gitlab-server"
  region        = "${var.region}"
  key_name      = "${aws_key_pair.main.key_name}"
  ami           = "${module.ubuntu-xenial-ami.id}"
  instance_type = "t2.medium"
  subnet_id     = "${module.vpc.public_subnet_ids[0]}"

  security_group_ids    = ["${aws_security_group.gitlab.id}"]
  root_volume_size      = "${var.root_volume_size}"
  data_volume_encrypted = false

  init_prefix = <<END_INIT
apt-get update
${module.init-install-awscli.init_snippet}
${module.init-install-ops.init_snippet}
END_INIT

  init_suffix = <<END_INIT
aws ec2 associate-address --allocation-id=${aws_eip.gitlab.id} --instance-id=$$(ec2metadata --instance-id) --allow-reassociation --region=${var.region}
mkdir -p /gitlab
mount /dev/xvdf1 /gitlab

cp /etc/fstab /etc/fstab.orig
echo "LABEL=gitlab            /gitlab  ext4   defaults,nofail     0 2" >> /etc/fstab

apt-get install -y docker docker.io
${module.init-gitlab-docker.init_snippet}
${module.init-gitlab-runner.init_snippet}
END_INIT
}

module "init-install-awscli" {
  source = "../../modules/init-snippet-install-awscli"
}

module "init-install-ops" {
  source = "../../modules/init-snippet-install-ops"
}

module "init-gitlab-docker" {
  source        = "../../modules/init-snippet-gitlab-docker"
  gitlab_domain = "${var.dns_zone_name}"
  gitlab_name   = "${var.gitlab_name}"
  gitlab_registry_name   = "${var.gitlab_registry_name}"
  config_elb    = "${var.config_elb}"

  # write docker images to this S3 bucket (created separate from this env)
  registry_bucket_name   = "${var.registry_bucket_name}"
  registry_bucket_region = "${var.region}"
}

module "init-gitlab-runner" {
  source = "../../modules/init-snippet-exec"

  init = <<END_INIT
mkdir /etc/gitlab-runner
cp /gitlab/gitlab-runner-config.toml /etc/gitlab-runner/config.toml
curl -L https://packages.gitlab.com/install/repositories/runner/gitlab-runner/script.deb.sh |  bash
apt-get install -y gitlab-runner
END_INIT
}

module "vpc" {
  source              = "../../modules/vpc-scenario-1"
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
  source            = "../../modules/ssh-sg"
  cidr_blocks       = ["0.0.0.0/0"]
  security_group_id = "${aws_security_group.gitlab.id}"
}

module "http-rule" {
  source            = "../../modules/single-port-sg"
  port              = 80
  description       = "Allow ingress for HTTP, port 80 (TCP)"
  cidr_blocks       = ["0.0.0.0/0"]
  security_group_id = "${aws_security_group.gitlab.id}"
}

module "https-rule" {
  source            = "../../modules/single-port-sg"
  port              = 443
  description       = "Allow ingress for HTTPS, port 443 (TCP)"
  cidr_blocks       = ["0.0.0.0/0"]
  security_group_id = "${aws_security_group.gitlab.id}"
}

module "gitlab-ssh-rule" {
  source            = "../../modules/single-port-sg"
  port              = 8022
  description       = "Allow ingress for Git over SSH, port 8022 (TCP)"
  cidr_blocks       = ["0.0.0.0/0"]
  security_group_id = "${aws_security_group.gitlab.id}"
}

module "open-egress-rule" {
  source            = "../../modules/open-egress-sg"
  security_group_id = "${aws_security_group.gitlab.id}"
}

##################
## DNS setup

data "aws_route53_zone" "selected" {
  name = "${var.dns_zone_name}"
}

resource "aws_route53_record" "gitlab" {
  zone_id = "${data.aws_route53_zone.selected.zone_id}"
  name    = "${var.gitlab_name}.${data.aws_route53_zone.selected.name}"
  type    = "A"
  ttl     = "300"
  records = ["${aws_eip.gitlab.public_ip}"]
}

resource "aws_route53_record" "registry" {
  zone_id = "${data.aws_route53_zone.selected.zone_id}"
  name    = "${var.gitlab_registry_name}.${data.aws_route53_zone.selected.name}"
  type    = "A"
  ttl     = "300"
  records = ["${aws_eip.gitlab.public_ip}"]
}

