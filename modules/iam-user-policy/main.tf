resource "aws_iam_user" "iam_user" {
  name = "${var.user_name}"
  tags = {
    "user"  = "${var.user_name}",
    "stage" = "${var.stage}"
  }
}

resource "aws_iam_policy" "iam_policy" {
  name   = "${var.iam_policy_name}"
  policy = "${var.iam_user_policy}"
}

resource "aws_iam_user_policy_attachment" "iam_user_policy" {
  user       = "${aws_iam_user.iam_user.name}"
  policy_arn = "${aws_iam_policy.iam_policy.arn}"
}
