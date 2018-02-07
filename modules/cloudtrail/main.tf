resource "aws_cloudtrail" "cloudtrail" {
  name           = "${var.name_prefix}-cloudtrail"
  s3_bucket_name = "${aws_s3_bucket.cloudtrail.id}"
  kms_key_id     = "${var.kms_key_id}"

  include_global_service_events = "${var.include_global_service_events}"
  enable_logging                = "${var.enable_logging}"
  is_multi_region_trail         = false

  tags = "${merge(map("Name", "${var.name_prefix}-cloudtrail"), "${var.extra_tags}")}"
}

resource "aws_s3_bucket" "cloudtrail" {
  bucket = "${var.name_prefix}-cloudtrail"
  region = "${data.aws_region.current.name}"
  acl    = "private"
  policy = "${data.aws_iam_policy_document.cloudtrail-bucket.json}"

  tags = "${merge(map("Name", "${var.name_prefix}-cloudtrail"), "${var.extra_tags}")}"
}

data "aws_region" "current" {
  current = true
}

data "aws_iam_policy_document" "cloudtrail-bucket" {
  statement {
    sid    = "AWSCloudTrailAclCheck"
    effect = "Allow"

    principals {
      type        = "Service"
      identifiers = ["cloudtrail.amazonaws.com"]
    }

    actions   = ["s3:GetBucketAcl"]
    resources = ["arn:${var.aws_cloud}:s3:::${var.name_prefix}-cloudtrail"]
  }

  statement {
    sid    = "AWSCloudTrailWrite"
    effect = "Allow"

    principals {
      type        = "Service"
      identifiers = ["cloudtrail.amazonaws.com"]
    }

    actions   = ["s3:PutObject"]
    resources = ["arn:${var.aws_cloud}:s3:::${var.name_prefix}-cloudtrail/*"]

    condition {
      test     = "StringEquals"
      variable = "s3:x-amz-acl"
      values   = ["bucket-owner-full-control"]
    }
  }
}
