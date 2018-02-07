/**
 * ## Kube Worker IAM
 *
 * Document.
 *
 */

resource "aws_iam_role" "kube_node" {
  name               = "${var.name_prefix == "" ? "" : "${var.name_prefix}-"}kube_node"
  path               = "/"
  assume_role_policy = "${data.aws_iam_policy_document.assume.json}"
}

resource "aws_iam_role_policy" "kube_node" {
  name = "${var.name_prefix == "" ? "" : "${var.name_prefix}-"}kube_node"
  role = "${aws_iam_role.kube_node.name}"

  policy = "${data.aws_iam_policy_document.kube_node.json}"
}

resource "aws_iam_instance_profile" "kube_node" {
  name = "${var.name_prefix == "" ? "" : "${var.name_prefix}-"}kube_node"
  role = "${aws_iam_role.kube_node.name}"
}
