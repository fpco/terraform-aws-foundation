/**
 * ## Prometheus Server
 *
 * Document.
 *
 */
module "prometheus-data" {
  source      = "../persistent-ebs"
  name_prefix = "${var.name}-prometheus-data"
  region      = var.region
  az          = "${var.region}${var.az}"
  size        = var.data_volume_size
  iops        = var.data_volume_iops
  volume_type = var.data_volume_type
  encrypted   = var.data_volume_encrypted
  kms_key_id  = var.data_volume_kms_key_id
  snapshot_id = var.data_volume_snapshot_id
  iam_instance_profile_role_name = module.instance_profile.iam_role_name
}

# Data source for the AWS subnet we deploy the EC2 instances into
data "aws_subnet" "server-subnet" {
  id = var.subnet_id
}

# Create an IAM Instance profile we can use on EC2, associated with the Prometheus Server ASG
module "instance_profile" {
  source      = "../iam-instance-profile"
  name_prefix = "${var.name}-${data.aws_subnet.server-subnet.availability_zone}"
}

module "prometheus-server" {
  source             = "../asg"
  security_group_ids = var.security_group_ids
  name_prefix        = var.name

  # append this to the ASG name
  name_suffix      = "prometheus-${var.az}"
  instance_type    = var.instance_type
  ami              = var.ami
  subnet_ids       = [var.subnet_id]
  public_ip        = var.public_ip
  key_name         = var.key_name
  elb_names        = var.load_balancers
  max_nodes        = 1
  min_nodes        = 1
  root_volume_type = var.root_volume_type
  root_volume_size = var.root_volume_size

  #
  iam_profile = module.instance_profile.iam_profile_id

  user_data = <<END_INIT
#!/bin/bash
${var.init_prefix}
${module.init-hostname.init_snippet}
${module.init-attach-ebs.init_snippet}
${module.init-prometheus.init_snippet}
${var.init_suffix}
END_INIT

}

module "init-hostname" {
  source = "../init-snippet-hostname"
  hostname_prefix = "prometheus"
}

module "init-attach-ebs" {
  source = "../init-snippet-attach-ebs-volume"
  volume_id = module.prometheus-data.volume_id
  region = var.region
}

module "init-prometheus" {
  source = "../init-snippet-prometheus"

  init_prefix = <<END_INIT
# to find the new device, need to call mount with bash, "outside" this init
sleep 5
bash -c "salt-call --local mount.mount /prometheus /dev/xvdf1 mkmnt=True"
salt-call --local mount.set_fstab /prometheus /dev/xvdf1 ext4
END_INIT


prometheus_pillar = <<END_PILLAR
${var.prometheus_pillar}
END_PILLAR

}

