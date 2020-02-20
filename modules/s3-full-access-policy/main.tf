/**
 * ## IAM Policy for Full Access on a List of S3 Buckets
 *
 */
variable "name" {
  description = "the name of the account or deployment to use with this policy"
  type = string
}

variable "bucket_names" {
  description = "list of bucket names to grant access to"
  type        = list(string)
}

output "id" {
  value       = aws_iam_policy.s3-full-access.id
  description = "`id` exported from `aws_iam_policy`"
}

output "arn" {
  value       = aws_iam_policy.s3-full-access.arn
  description = "`arn` exported from `aws_iam_policy`"
}

output "name" {
  value       = aws_iam_policy.s3-full-access.name
  description = "`name` exported from `aws_iam_policy`"
}

data "aws_partition" "current" {
}

data "aws_iam_policy_document" "s3-full-access" {
  statement {
    effect = "Allow"

    actions = [
      "s3:ListBucket",
      "s3:GetBucketLocation",
      "s3:ListBucketMultipartUploads",
    ]

    resources = formatlist(
      "arn:${data.aws_partition.current.partition}:s3:::%s",
      var.bucket_names,
    )
  }

  statement {
    effect = "Allow"

    actions = [
      # "s3:ListObjects", # TODO this might not be a valid action
      # See https://docs.aws.amazon.com/IAM/latest/UserGuide/list_amazons3.html
      "s3:PutObject",
      "s3:GetObject",
      "s3:DeleteObject",
      # "s3:CreateMultipartUpload", # TODO this might not be a valid action
      "s3:ListMultipartUploadParts",
      "s3:AbortMultipartUpload",
    ]

    resources = formatlist(
      "arn:${data.aws_partition.current.partition}:s3:::%s/*",
      var.bucket_names,
    )
  }
}

resource "aws_iam_policy" "s3-full-access" {
  name   = "${var.name}_s3_bucket_access"
  policy = data.aws_iam_policy_document.s3-full-access.json
}

