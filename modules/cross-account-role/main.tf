/**
 * ## Cross-Account Role
 *
 * Creates an IAM role that can be assumed by users in given accounts.
 * It is up to the caller to attach desired policies to this role.
 *
 */

variable "trust_account_ids" {
  description = "List of other accounts to trust to assume the role"
  default     = []
  type        = "list"
}

variable "name" {
  description = "Name to give the role"
  type        = "string"
}

output "arn" {
  value = "${aws_iam_role.role.arn}"
}

output "name" {
  value = "${aws_iam_role.role.name}"
}

data "aws_caller_identity" "current" {}

data "aws_partition" "current" {}

data "aws_iam_policy_document" "assume-role" {
  statement {
    effect  = "Allow"
    actions = ["sts:AssumeRole"]

    condition {
      test     = "Bool"
      variable = "aws:MultiFactorAuthPresent"
      values   = ["true"]
    }

    principals {
      type        = "AWS"
      identifiers = ["${formatlist("arn:${data.aws_partition.current.partition}:iam::%s:root",var.trust_account_ids)}"]
    }
  }
}

resource "aws_iam_role" "role" {
  name               = "${var.name}"
  assume_role_policy = "${data.aws_iam_policy_document.assume-role.json}"
}
