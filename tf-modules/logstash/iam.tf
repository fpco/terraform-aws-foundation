module "credstash-reader" {
  source = "../credstash"
  name_prefix = "${var.name_prefix}"
  create_reader_policy = true
  kms_key_arn = "${var.credstash_kms_key_arn}"
}

resource "aws_iam_instance_profile" "logstash-profile" {
  name = "${var.name_prefix}-logstash-profile"
  roles = ["${aws_iam_role.logstash-role.name}"]
}

resource "aws_iam_role_policy_attachment" "credstash-reader-policy-attachment" {
  role = "${aws_iam_role.logstash-role.name}"
  policy_arn = "${module.credstash-reader.reader_policy_arn}"
}

resource "aws_iam_role" "logstash-role" {
  provisioner "local-exec" {
    command = "../credstash/grant.sh create reader ${var.credstash_kms_key_arn} ${aws_iam_role.logstash-role.arn}"
  }
  ## Cleanup action that requires terraform 0.9.0
  ## See RFC: https://docs.google.com/document/d/15nEcV7fxskDgYrXoNMl6RYIo10PCiZGle7TP8xitrFE/edit#
  ## and: https://docs.google.com/document/d/15nEcV7fxskDgYrXoNMl6RYIo10PCiZGle7TP8xitrFE/edit#
  # provisioner "local-exec" {
  #   when       = "destroy"
  #   on_failure = "continue"
  #   command    = "../credstash/grant.sh revoke ${var.credstash_kms_key_arn} ${aws_iam_role.logstash-role.arn}"
  # }
  name_prefix  = "${var.name_prefix}-logstash-role-"
  assume_role_policy = <<END_POLICY
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Action": "sts:AssumeRole",
      "Principal": {
        "Service": "ec2.amazonaws.com"
      },
      "Effect": "Allow",
      "Sid": ""
    }
  ]
}
END_POLICY
}
