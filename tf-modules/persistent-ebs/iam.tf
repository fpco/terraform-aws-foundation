resource "aws_iam_instance_profile" "attach_ebs" {
  name = "${var.name_prefix}-${var.az}-attach-ebs"
  role = "${aws_iam_role.attach_ebs.name}"
}

#
resource "aws_iam_role" "attach_ebs" {
  name = "${var.name_prefix}-${var.az}-attach-ebs"
  path = "/"

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

#
resource "aws_iam_role_policy" "attach_ebs" {
  name = "${var.name_prefix}-${var.az}-attach-ebs-${aws_ebs_volume.main.id}"
  role = "${aws_iam_role.attach_ebs.id}"

  policy = <<END_POLICY
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Effect": "Allow",
      "Action": [
        "ec2:AttachVolume",
        "ec2:DetachVolume"
      ],
      "Resource": [
        "arn:${var.aws_cloud}:ec2:${var.region}:${data.aws_caller_identity.current.account_id}:volume/${aws_ebs_volume.main.id}",
        "arn:${var.aws_cloud}:ec2:${var.region}:${data.aws_caller_identity.current.account_id}:instance/*"
      ]
    }
  ]
}
END_POLICY
}
