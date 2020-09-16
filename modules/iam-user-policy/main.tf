resource "aws_iam_user" "main" {
  name = var.username
  tags = merge(var.extra_tags, {"user" = var.username})
}

resource "aws_iam_policy" "main" {
  name   = var.iam_policy_name
  policy = var.iam_user_policy
}

resource "aws_iam_user_policy_attachment" "main" {
  user       = aws_iam_user.main.name
  policy_arn = aws_iam_policy.main.arn
}

