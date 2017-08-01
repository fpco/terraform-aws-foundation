resource "aws_iam_instance_profile" "master-node-iam-profile" {
  count = "${var.master_node_count}"
  name = "${var.name_prefix}-master-node-profile-${count.index}"
  role = "${element(aws_iam_role.master-node-role.*.name, count.index)}"
}

resource "aws_iam_instance_profile" "data-node-iam-profile" {
  count = "${var.data_node_count}"
  name = "${var.name_prefix}-data-node-profile-${count.index}"
  role = "${element(aws_iam_role.data-node-role.*.name, count.index)}"
}

resource "aws_iam_role_policy_attachment" "master-node-attach-ec2-discovery" {
  count = "${var.master_node_count}"
  role = "${element(aws_iam_role.master-node-role.*.name, count.index)}"
  policy_arn = "${aws_iam_policy.ec2-discovery-policy.arn}"
}

resource "aws_iam_role_policy_attachment" "data-node-attach-ec2-discovery" {
  count = "${var.data_node_count}"
  role = "${element(aws_iam_role.data-node-role.*.name, count.index)}"
  policy_arn = "${aws_iam_policy.ec2-discovery-policy.arn}"
}

module "credstash-reader" {
  source = "../credstash"
  name_prefix = "${var.name_prefix}-elasticsearch"
  create_reader_policy = true
  kms_key_arn = "${var.credstash_kms_key_arn}"
  table_name = "${var.credstash_table_name}"
}

resource "aws_iam_role_policy_attachment" "master-node-credstash-reader-policy-attachment" {
  count = "${var.master_node_count}"
  role = "${element(aws_iam_role.master-node-role.*.name, count.index)}"
  policy_arn = "${module.credstash-reader.reader_policy_arn}"
}

resource "aws_iam_role_policy_attachment" "data-node-credstash-reader-policy-attachment" {
  count = "${var.data_node_count}"
  role = "${element(aws_iam_role.data-node-role.*.name, count.index)}"
  policy_arn = "${module.credstash-reader.reader_policy_arn}"
}

resource "aws_iam_role" "master-node-role" {
  count = "${var.master_node_count}"
  name = "${var.name_prefix}-master-node-role-${format("%02d", count.index)}"
  provisioner "local-exec" {
    command = "${path.module}/../credstash/grant.sh create reader ${var.credstash_kms_key_arn} ${self.arn}"
  }
  provisioner "local-exec" {
    when       = "destroy"
    on_failure = "continue"
    command    = "${path.module}/../credstash/grant.sh revoke ${var.credstash_kms_key_arn} ${self.arn}"
  }
  assume_role_policy = <<END_POLICY
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Action": "sts:AssumeRole",
      "Principal": {
        "Service": "ec2.amazonaws.com"
      },
      "Effect": "Allow",
      "Sid": ""
    }
  ]
}
END_POLICY
}


resource "aws_iam_role" "data-node-role" {
  count = "${var.data_node_count}"
  name = "${var.name_prefix}-data-node-role-${format("%02d", count.index)}"
  provisioner "local-exec" {
    command = "${path.module}/../credstash/grant.sh create reader ${var.credstash_kms_key_arn} ${self.arn}"
  }
  provisioner "local-exec" {
    when       = "destroy"
    on_failure = "continue"
    command    = "${path.module}/../credstash/grant.sh revoke ${var.credstash_kms_key_arn} ${self.arn}"
  }
  assume_role_policy = <<END_POLICY
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Action": "sts:AssumeRole",
      "Principal": {
        "Service": "ec2.amazonaws.com"
      },
      "Effect": "Allow",
      "Sid": ""
    }
  ]
}
END_POLICY
}


resource "aws_iam_policy" "ec2-discovery-policy" {
  name = "${var.name_prefix}-ec2-discovery-policy"
  policy = <<END_POLICY
{
  "Statement": [
    {
      "Action": ["ec2:DescribeInstances"],
      "Effect": "Allow",
      "Resource": ["*"]
    }
  ],
  "Version": "2012-10-17"
}
END_POLICY
}

