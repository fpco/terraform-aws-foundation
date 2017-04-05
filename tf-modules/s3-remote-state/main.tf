/**
 *## S3 Bucket to Store Remote State
 *
 * 
 */
variable "bucket_name" {
    description = "the name to give the bucket"
}
variable "principals" {
    default     = []
    description = "list of user/role ARNs to get full access to the bucket"
}
resource "aws_s3_bucket" "remote-state" {
    bucket = "${var.bucket_name}"
    acl    = "private"
    versioning {
        enabled = true
    }
}
data "aws_iam_policy_document" "s3-full-access" {
  statement {
    effect = "Allow"

    actions = [
      "s3:ListBucket",
      "s3:GetBucketLocation",
      "s3:ListBucketMultipartUploads"
    ]

    principals {
      type        = "AWS"
      identifiers = ["${compact(var.principals)}"]
    }

    resources = ["arn:aws:s3:::${aws_s3_bucket.remote-state.id}"]
  }

  statement {
    effect = "Allow"
    # find an authoritative list of valid Actions for a AWS bucket policy,
    # I haven't been able to locate one, and the two commented out are invalid
    actions = [
#     "s3:ListObjects",
      "s3:PutObject",
      "s3:GetObject",
      "s3:DeleteObject",
#     "s3:CreateMultipartUpload",
      "s3:ListMultipartUploadParts",
      "s3:AbortMultipartUpload"
    ]

    principals {
      type        = "AWS"
      identifiers = ["${compact(var.principals)}"]
    }

    resources = ["arn:aws:s3:::${aws_s3_bucket.remote-state.id}/*"]
  }
}

resource "aws_s3_bucket_policy" "s3-full-access" {
  bucket = "${aws_s3_bucket.remote-state.id}"
  policy = "${data.aws_iam_policy_document.s3-full-access.json}"
}
//`arn` exported from `aws_s3_bucket`
output "bucket_arn" {
    value = "${aws_s3_bucket.remote-state.arn}"
}
//`id` exported from `aws_s3_bucket`
output "bucket_id" {
    value = "${aws_s3_bucket.remote-state.id}"
}
//`region` exported from `aws_s3_bucket`
output "region" {
    value = "${aws_s3_bucket.remote-state.region}"
}
//Derived URL to the S3 bucket
output "url" {
    value = "https://s3-${aws_s3_bucket.remote-state.region}.amazonaws.com/${aws_s3_bucket.remote-state.id}"
}
//Export `principals` variable (list of IAM user/role ARNs with access to the bucket)
output "principals" {
    value = "${var.principals}"
}
