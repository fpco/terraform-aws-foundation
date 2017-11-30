/**
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
 */

module "gitlab-asg" {
  source                  = "../single-node-asg"
  name                    = "${var.name_prefix}"
  az                      = "${var.az}"
  load_balancers          = "${var.load_balancers}"
  key_name                = "${var.key_name}"
  ami                     = "${var.instance_ami}"
  instance_type           = "${var.instance_type}"
  name_suffix             = "gitlab-asg"
  public_ip               = "${var.public_ip}"
  subnet_id               = "${var.subnet_id}"
  security_group_ids      = "${var.security_group_ids}"
  region                  = "${var.region}"
  root_volume_type        = "${var.root_volume_type}"
  root_volume_size        = "${var.root_volume_size}"
  data_volume_type        = "${var.data_volume_type}"
  data_volume_size        = "${var.data_volume_size}"
  data_volume_encrypted   = "${var.data_volume_encrypted}"
  data_volume_kms_key_id  = "${var.data_volume_kms_key_id}"
  data_volume_snapshot_id = "${var.data_volume_snapshot_id}"
  data_volume_iops        = "${var.data_volume_iops}"
  aws_cloud               = "${var.aws_cloud}"

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
cmd="docker run --detach \
    --publish ${var.gitlab_https_port}:443 \
    --publish ${var.gitlab_http_port}:80 \
    --publish ${var.gitlab_ssh_port}:22 \
    --restart always \
    --volume /gitlab/config:/etc/gitlab \
    --volume /gitlab/logs:/var/log/gitlab \
    --volume /gitlab/data:/var/opt/gitlab \
    gitlab/gitlab-ce"
echo "$cmd" > /etc/rc.local
$cmd
END_INIT
}

module "init-install-awscli" {
  source = "../../tf-modules/init-snippet-install-awscli"
}

module "init-install-ops" {
  source = "../../tf-modules/init-snippet-install-ops"
}

# The ARN of the IAM Role attached the single-node ASG instance
output "asg_iam_role_arn" {
  value = "${module.gitlab-asg.asg_iam_role_arn}"
}
