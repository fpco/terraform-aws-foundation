/**
 * ## Prometheus Cloudwatch exporter IAM role
 *
 * This role is for creating a IAM policy that can later get attached to the
 * prometheus-cloudwatch exporter pod. To be used in combo with kube2iam.
 *
 */

resource "aws_iam_role" "cloudwatch_exporter" {
  name               = "${var.name_prefix}-cloudwatch-exporter"
  path               = "/"
  assume_role_policy = data.aws_iam_policy_document.assume.json
}

resource "aws_iam_policy" "cloudwatch_exporter" {
  name        = "${var.name_prefix}-cloudwatch-exporter"
  path        = "/"
  description = "Allows reading cloud watch metrics."

  policy = data.aws_iam_policy_document.cloudwatch_exporter.json
}

resource "aws_iam_role_policy_attachment" "cloudwatch_exporter" {
  role       = aws_iam_role.cloudwatch_exporter.name
  policy_arn = aws_iam_policy.cloudwatch_exporter.arn
}

