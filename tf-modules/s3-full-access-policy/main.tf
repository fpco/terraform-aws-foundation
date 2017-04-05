/**
 *## IAM Policy for Full Access on a List of S3 Buckets
 *
 */
variable "name" {
    description = "the name of the account or deployment to use with this policy"
}
variable "bucket_names" {
    description = "the name of the buckets to grant access to"
}
//`id` exported from `aws_iam_policy`
output "id" {
    value = "${aws_iam_policy.s3-full-access.id}"
}
//`arn` exported from `aws_iam_policy`
output "arn" {
    value = "${aws_iam_policy.s3-full-access.arn}"
}
//`name` exported from `aws_iam_policy`
output "name" {
    value = "${aws_iam_policy.s3-full-access.name}"
}

data "aws_iam_policy_document" "s3-full-access" {
  statement {
    effect  = "Allow"
    actions = [
      "s3:ListBucket",
      "s3:GetBucketLocation",
      "s3:ListBucketMultipartUploads",
    ]

    resources = ["${join("\",\"",formatlist("arn:aws:s3:::%s",split(",", var.bucket_names)))}"]
  }

  statement {
    effect = "Allow"
    actions = [
      "s3:ListObjects",
      "s3:PutObject",
      "s3:GetObject",
      "s3:DeleteObject",
      "s3:CreateMultipartUpload",
      "s3:ListMultipartUploadParts",
      "s3:AbortMultipartUpload"
    ]

    resources = ["${join("\",\"",formatlist("arn:aws:s3:::%s/*",split(",", var.bucket_names)))}"]
  }
}

resource "aws_iam_policy" "s3-full-access" {
    name = "${var.name}_s3_bucket_access"
    policy = "${data.aws_iam_policy_document.s3-full-access.json}"
}
