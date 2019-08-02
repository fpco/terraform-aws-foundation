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

resource "aws_eip" "eip" {
  count = var.assign_eip ? 1 : 0
}

module "service-data" {
  source                         = "../persistent-ebs"
  name_prefix                    = "${var.name_prefix}-${var.name_suffix}-data"
  region                         = var.region
  az                             = data.aws_subnet.server-subnet.availability_zone
  size                           = var.data_volume_size
  iops                           = var.data_volume_iops
  volume_type                    = var.data_volume_type
  encrypted                      = var.data_volume_encrypted
  kms_key_id                     = var.data_volume_kms_key_id
  snapshot_id                    = var.data_volume_snapshot_id
  iam_instance_profile_role_name = module.instance_profile.iam_role_name
}

module "instance_profile" {
  source      = "../iam-instance-profile"
  name_prefix = "${var.name_prefix}-${var.name_suffix}"
}

module "server" {
  source             = "../asg"
  security_group_ids = var.security_group_ids

  # combine the prefix and suffix for the full name
  name_prefix      = var.name_prefix
  name_suffix      = "${var.name_suffix}-${data.aws_subnet.server-subnet.availability_zone}"
  placement_group  = var.placement_group
  instance_type    = var.instance_type
  ami              = var.ami
  subnet_ids       = [var.subnet_id]
  azs              = [data.aws_subnet.server-subnet.availability_zone]
  key_name         = var.key_name
  elb_names        = var.load_balancers
  max_nodes        = 1
  min_nodes        = 1
  root_volume_type = var.root_volume_type
  root_volume_size = var.root_volume_size
  iam_profile      = module.instance_profile.iam_profile_id

  user_data = <<END_INIT
#!/bin/bash
${var.init_prefix}
apt update
apt install -y awscli
${module.init-attach-ebs.init_snippet}
${var.assign_eip ? "aws ec2 associate-address --instance-id \"$(ec2metadata --instance-id)\" --region \"${var.region}\" --allocation-id \"${element(aws_eip.eip.*.id,0)}\"" : ""}
${var.init_suffix}
END_INIT

}

# boxed module to attach the EBS volume to the node
module "init-attach-ebs" {
  source = "../init-snippet-attach-ebs-volume"
  region = var.region
  volume_id = module.service-data.volume_id
}

# Data source for AWS subnet
data "aws_subnet" "server-subnet" {
  id = var.subnet_id
}

resource "aws_iam_role_policy_attachment" "associate_eip" {
  role       = module.instance_profile.iam_role_name
  policy_arn = aws_iam_policy.associate_eip_policy.arn
}

resource "aws_iam_policy" "associate_eip_policy" {
  name   = "associate_address"
  policy = data.aws_iam_policy_document.associate_eip_policy_doc.json
}

data "aws_iam_policy_document" "associate_eip_policy_doc" {
  statement {
    sid = ""
    effect = "Allow"
    actions = [
      "ec2:AssociateAddress"
    ]
    resources = ["*"]
  }
}
