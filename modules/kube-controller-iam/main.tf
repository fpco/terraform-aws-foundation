/**
 * ## Kube Controller IAM
 *
 * Document.
 *
 */

resource "aws_iam_role" "kube_master" {
  name               = "${var.name_prefix == "" ? "" : "${var.name_prefix}-"}kube_master"
  path               = "/"
  assume_role_policy = "${data.aws_iam_policy_document.assume.json}"
}

resource "aws_iam_role_policy" "kube_master" {
  name = "${var.name_prefix == "" ? "" : "${var.name_prefix}-"}kube_master"
  role = "${aws_iam_role.kube_master.name}"

  policy = "${data.aws_iam_policy_document.kube_master.json}"
}

resource "aws_iam_instance_profile" "kube_master" {
  name = "${var.name_prefix == "" ? "" : "${var.name_prefix}-"}kube_master"
  role = "${aws_iam_role.kube_master.name}"
}
