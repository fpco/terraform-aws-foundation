/**
 * ## Single-Node Auto-Scaling Group
 *
 * This module uses an EBS volume and an auto-scaling group with a single-node
 * to establish a reliable and robust pattern for HA in various forms.
 *
 * NOTE: This module assumes that all the dependencies for the
 * `../init-snippet-attach-ebs-volume` module are satisfied
 * by the `init_prefix`.
 * In particular, this means that the aws cli tool and ec2metadata are installed.
 *
 */

# Data source for AWS subnet
data "aws_subnet" "server-subnet" {
  id = var.subnet_id
}

# Create an IAM Instance profile we can use on EC2, associated with the ASG
module "instance_profile" {
  source      = "../iam-instance-profile"
  name_prefix = "${var.name_prefix}-${var.name_suffix}"
}

# Create a single EBS volume that can be used in a single/specific AZ, for the ASG
module "service-data" {
  source      = "../persistent-ebs"
  name_prefix = "${var.name_prefix}-${var.name_suffix}-data"
  region      = var.region
  az          = data.aws_subnet.server-subnet.availability_zone
  size        = var.data_volume_size
  iops        = var.data_volume_iops
  volume_type = var.data_volume_type
  encrypted   = var.data_volume_encrypted
  kms_key_id  = var.data_volume_kms_key_id
  snapshot_id = var.data_volume_snapshot_id

  # EBS module will create an IAM policy and associate with this role
  iam_instance_profile_role_name = module.instance_profile.iam_role_name
}

# Create an ASG with just 1 EC2 instance
module "server" {
  source = "../asg"

  # the prefix and suffix names are combined in the `asg` module to create the full name
  name_prefix        = var.name_prefix
  name_suffix        = "${var.name_suffix}-${data.aws_subnet.server-subnet.availability_zone}"
  placement_group    = var.placement_group
  public_ip          = var.public_ip
  iam_profile        = module.instance_profile.iam_profile_id
  instance_type      = var.instance_type
  ami                = var.ami
  azs                = [data.aws_subnet.server-subnet.availability_zone]
  elb_names          = var.load_balancers
  key_name           = var.key_name
  max_nodes          = 1
  min_nodes          = 1
  root_volume_type   = var.root_volume_type
  root_volume_size   = var.root_volume_size
  security_group_ids = var.security_group_ids
  subnet_ids         = [var.subnet_id]

  user_data = <<END_INIT
#!/bin/bash
${var.init_prefix}
${module.init-attach-ebs.init_snippet}
${var.init_suffix}
END_INIT

}

# Render init snippet - boxed module to attach the EBS volume to the node
module "init-attach-ebs" {
  source    = "../init-snippet-attach-ebs-volume"
  region    = var.region
  volume_id = module.service-data.volume_id
}
