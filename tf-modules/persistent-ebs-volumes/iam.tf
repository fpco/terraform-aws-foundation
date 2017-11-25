data "aws_caller_identity" "current" {}
data "aws_region" "current" {
  current = true
}

resource "aws_iam_policy" "ebs-volume-policy" {
  count = "${var.volume_count}"
  name = "${var.name_prefix}-ebs-volume-${count.index + 1}"
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
        "arn:${var.aws_cloud}:ec2:${data.aws_region.current.name}:${data.aws_caller_identity.current.account_id}:volume/${element(aws_ebs_volume.volumes.*.id, count.index)}",
        "arn:${var.aws_cloud}:ec2:${data.aws_region.current.name}:${data.aws_caller_identity.current.account_id}:instance/*"
      ]
    }
  ]
}
END_POLICY
}
