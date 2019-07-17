resource "aws_iam_user" "iam_user" {
  name = "${var.user_name}"
  tags = {
    "user"  = "${var.user_name}",
    "stage" = "${var.stage}"
  }
}

resource "aws_iam_user_policy" "iam_user_policy" {
  name   = "${var.iam_policy_name}"
  user   = "${aws_iam_user.iam_user.name}"
  policy = "${var.iam_user_policy}"
}
