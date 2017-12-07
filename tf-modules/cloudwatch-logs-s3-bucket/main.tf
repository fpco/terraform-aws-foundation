/**
 * ## CloudWatch Logs S3 Bucket
 *
 * This module creates an S3 bucket with correct IAM policies for
 * shipping instance logs using CloudWatch. Users of this module need
 * to create `aws_cloudwatch_log_group` separately and provide ARNs of
 * writers so permission is granted for writing.
 *
 * Logs are append-only by design. No permission is given to writers
 * to retrieve or alter any objects.
 *
 * ### Example
 *
 * ```
 * # Service account of ELB where writer instances are located
 * data "aws_elb_service_account" "current" {}
 *
 * # Basic log group
 * resource "aws_cloudwatch_log_group" "syslog" {
 *   name = "syslog"
 * }
 *
 * module "cloudwatch-logs" {
 *   source      = "github.com/fpco/fpco-terraform-aws//tf-modules/cloudwatch-logs"
 *   name_prefix = "some-project"
 *   principals  = ["${data.aws_elb_service_account.current.arn}"]
 * }
 * ```
 *
 */

data "aws_iam_policy_document" "cloudwatch-logs-writer" {
  statement {
    effect  = "Allow"
    actions = ["s3:PutObject"]

    resources = ["arn:${var.aws_region}:s3:::${var.name_prefix}-cloudwatch-logs/*}"]

    principals {
      type        = "AWS"
      identifiers = "${var.principals}"
    }
  }
}

resource "aws_s3_bucket" "cloudwatch-logs" {
  bucket = "${var.name_prefix}-cloudwatch-logs"
  acl    = "private"
  policy = "${data.aws_iam_policy_document.cloudwatch-logs-writer.json}"

  tags = "${merge(map("Name","${var.name_prefix}-cloudwatch-logs"), "${var.extra_tags}")}"
}
