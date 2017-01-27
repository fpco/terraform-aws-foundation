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
    command = "../credstash/create-grant.sh create reader ${var.credstash_kms_key_arn} ${aws_iam_role.logstash-role.arn}"
  }
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
