resource "aws_iam_instance_profile" "master-node-iam-profile" {
  count = "${var.master_node_count}"
  name = "${var.name_prefix}-master-node-profile-${count.index}"
  roles = ["${element(aws_iam_role.master-node-role.*.name, count.index)}"]
}

resource "aws_iam_instance_profile" "data-node-iam-profile" {
  count = "${var.data_node_count}"
  name = "${var.name_prefix}-data-node-profile-${count.index}"
  roles = ["${element(aws_iam_role.data-node-role.*.name, count.index)}"]
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

resource "aws_iam_role" "master-node-role" {
  count = "${var.master_node_count}"
  name = "${var.name_prefix}-master-node-role-${count.index}"
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
  name = "${var.name_prefix}-data-node-role-${count.index}"
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

