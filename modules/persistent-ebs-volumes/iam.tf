data "aws_caller_identity" "current" {
}

data "aws_region" "current" {
}

data "aws_partition" "current" {
}

resource "aws_iam_policy" "ebs-volume-policy" {
  count  = var.volume_count
  name   = "${var.name_prefix}-ebs-volume-${count.index + 1}"
  policy = data.aws_iam_policy_document.ebs-volume-policy[count.index].json
}

data "aws_iam_policy_document" "ebs-volume-policy" {
  count = var.volume_count
  statement {
    effect = "Allow"

    actions = [
      "ec2:AttachVolume",
      "ec2:DetachVolume",
    ]

    resources = [
      "arn:${data.aws_partition.current.partition}:ec2:${data.aws_region.current.name}:${data.aws_caller_identity.current.account_id}:volume/${element(aws_ebs_volume.volumes.*.id, count.index)}",
      "arn:${data.aws_partition.current.partition}:ec2:${data.aws_region.current.name}:${data.aws_caller_identity.current.account_id}:instance/*",
    ]
  }
}

