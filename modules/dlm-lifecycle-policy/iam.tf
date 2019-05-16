# Create the iam role
resource "aws_iam_role" "dlm_lifecycle_role" {
  count = "${var.create_dlm_iam_role == "true" ? 1 : 0}"

  name                = "${var.role_name}"
  assume_role_policy  = <<EOF
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Action": "sts:AssumeRole",
      "Principal": {
        "Service": "dlm.amazonaws.com"
      },
      "Effect": "Allow",
      "Sid": ""
    }
  ]
}
EOF
}

# DLM lifecycle Policy
resource "aws_iam_role_policy" "dlm_lifecycle" {
  count = "${var.create_dlm_iam_role == "true" ? 1 : 0}"

  name  = "dlm-lifecycle-policy"
  role  = "${aws_iam_role.dlm_lifecycle_role.id}"

  policy  = <<EOF
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Effect": "Allow",
      "Action": [
        "ec2:CreateSnapshot",
        "ec2:DeleteSnapshot",
        "ec2:DescribeVolumes",
        "ec2:DescribeSnapshots"
      ],
      "Resource": "*"
    },
    {
      "Effect": "Allow",
      "Action": [
        "ec2:CreateTags"
      ],
      "Resource": "arn:aws:ec2:*::snapshot/*"
    }
  ]
}
EOF
}

data "aws_iam_role" "dlm_lifecycle_role" {
  name = "${var.role_name}"
}
