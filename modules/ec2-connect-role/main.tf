data "aws_caller_identity" "current" {
}

data "aws_iam_policy_document" "ec2-instance-connect" {
  statement {
    actions = [
      "ec2:DescribeInstances",
    ]

    resources = ["*"]
  }

  statement {
    actions = [
      "ec2-instance-connect:SendSSHPublicKey",
    ]

    resources = [for i in var.instance_ids : "arn:aws:ec2:${var.region}:${var.account_id}:instance/${i}"]

    condition {
      test     = "StringEquals"
      variable = "ec2:osuser"

      values = [
        "ubuntu",
      ]
    }
  }
}

resource "aws_iam_policy" "ec2-instance-connect" {
  name        = "ec2-instance-connect"
  description = "grants permissions to connect to an instance using EC2 Instance Connect"
  policy      = data.aws_iam_policy_document.ec2-instance-connect.json
}

module "role" {
  source            = "../cross-account-role"
  name              = var.name
  trust_account_ids = concat([data.aws_caller_identity.current.account_id],
    var.trust_account_ids)
}

resource "aws_iam_role_policy_attachment" "role_ec2-instance-connect" {
  role       = module.role.name
  policy_arn = aws_iam_policy.ec2-instance-connect.arn
}
