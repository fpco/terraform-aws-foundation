/**
 * ## ci-cache-s3
 *
 * This module will create an S3 bucket and IAM user with full access to it, so they
 * can be used by the cache-s3 tool during CI on Travis, AppVeyor, etc.
 *
 *
 */



resource "aws_iam_user" "cache-s3-user" {
  name = "${var.prefix}${var.user_name}"
}

resource "aws_iam_access_key" "cache-s3-user-access-key" {
  user    = "${aws_iam_user.cache-s3-user.name}"
  pgp_key = "${var.pgp_key}"
}

resource "aws_s3_bucket" "bucket" {
  bucket = "${var.prefix}${var.bucket_name}"
  acl    = "private"

  lifecycle_rule {
    id      = "cache"
    prefix  = "cache-s3/"
    enabled = "${var.cache_days == 0 ? false : true}"

    expiration {
      days = "${var.cache_days}"
    }
  }
}

module "s3-full-access" {
  source       = "../s3-full-access-policy"
  name         = "${var.prefix}ci-cache-s3-access"
  bucket_names = ["${aws_s3_bucket.bucket.id}"]
}

resource "aws_iam_user_policy_attachment" "s3-full-access-attachment" {
  user       = "${aws_iam_user.cache-s3-user.name}"
  policy_arn = "${module.s3-full-access.arn}"
}


resource "aws_iam_user_policy_attachment" "s3-grant-public-read" {
  count      = "${var.user_grants_public ? 1 : 0}"
  user       = "${aws_iam_user.cache-s3-user.name}"
  policy_arn = "${element(aws_iam_policy.s3-grant-public-read.*.arn, 0)}"
}


resource "aws_iam_policy" "s3-grant-public-read" {
  count  = "${var.user_grants_public ? 1 : 0}"
  name   = "${var.prefix}ci-cache-s3-grant-public-read"
  policy = "${data.aws_iam_policy_document.s3-grant-public-read.json}"
}

data "aws_iam_policy_document" "s3-grant-public-read" {

  statement {
    effect = "Allow"

    actions = [
      "s3:PutObjectAcl"
    ]

    condition {
      test     = "StringEquals"
      variable = "s3:x-amz-acl"
      values   = ["public-read"]
    }
    resources = ["arn:aws:s3:::${aws_s3_bucket.bucket.id}/*"]
  }
}

