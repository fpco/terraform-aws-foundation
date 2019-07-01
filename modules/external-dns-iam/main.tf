/**
 * ## External DNS IAM
 *
 * This role is for attaching a iam policy to the
 * external-dns pod. To be used in combo with kube2iam.
 *
 */

resource "aws_iam_role" "dnscontroller" {
  name               = "${var.name_prefix}-dnscontroller"
  path               = "/"
  assume_role_policy = data.aws_iam_policy_document.assume.json
}

resource "aws_iam_policy" "dnscontroller" {
  name        = "${var.name_prefix}-dnscontroller"
  path        = "/"
  description = "Allows changing route53 records"

  policy = data.aws_iam_policy_document.dnscontroller.json
}

resource "aws_iam_role_policy_attachment" "dnscontroller" {
  role       = aws_iam_role.dnscontroller.name
  policy_arn = aws_iam_policy.dnscontroller.arn
}

