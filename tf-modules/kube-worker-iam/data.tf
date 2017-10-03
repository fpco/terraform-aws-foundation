data "aws_iam_policy_document" "kube_node" {
  statement {
    effect  = "Allow"
    actions = [
      "elasticloadbalancing:*",
      "ec2:Describe*"
    ]
    resources = ["*"]
  }
}

data "aws_iam_policy_document" "assume" {
  statement {
    effect = "Allow"
    actions = [
      "sts:AssumeRole"
    ]
    sid = ""

    principals {
      type = "Service"
      identifiers = ["ec2.amazonaws.com"]
    }
  }
}

