variable "assume_role_policy" {
  description = "The policy (JSON) that grants an entity permission to assume the role."
}

variable "policy" {
  description = "The policy document (JSON)."
}

variable "name_prefix" {
  description = "Creates a unique name beginning with the specified prefix."
}


resource "aws_iam_instance_profile" "profile" {
  name_prefix = "${var.name_prefix}"
  role        = "${aws_iam_role.role.name}"
}

resource "aws_iam_role" "role" {
  name               = "${var.name_prefix}"
  path               = "/"
  assume_role_policy = "${var.assume_role_policy}"
}

resource "aws_iam_role_policy" "policy" {
  name   = "${var.name_prefix}"
  role   = "${aws_iam_role.role.id}"
  policy = "${var.policy}"
}

output "iam_profile_id" {
  value = "${aws_iam_instance_profile.profile.id}"
}
