resource "aws_iam_instance_profile" "attach_ebs" {
  name = "${var.name_prefix}-${var.az}-attach-ebs"
  role = aws_iam_role.attach_ebs.name
}

#
resource "aws_iam_role" "attach_ebs" {
  name               = "${var.name_prefix}-${var.az}-attach-ebs"
  path               = "/"
  assume_role_policy = data.aws_iam_policy_document.attach_ebs.json
}

#
resource "aws_iam_role_policy" "attach_ebs" {
  name   = "${var.name_prefix}-${var.az}-attach-ebs-${aws_ebs_volume.main.id}"
  role   = aws_iam_role.attach_ebs.id
  policy = data.aws_iam_policy_document.attach_ebs_policy.json
}

