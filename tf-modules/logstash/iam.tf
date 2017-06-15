module "credstash-reader" {
  source = "../credstash"
  name_prefix = "${var.name_prefix}"
  create_reader_policy = true
  kms_key_arn = "${var.credstash_kms_key_arn}"
  table_name = "${var.credstash_table_name}"
}

resource "aws_iam_instance_profile" "logstash-profile" {
  name = "${var.name_prefix}-logstash-profile"
  role = "${aws_iam_role.logstash-role.name}"
}

resource "aws_iam_role_policy_attachment" "credstash-reader-policy-attachment" {
  role = "${aws_iam_role.logstash-role.name}"
  policy_arn = "${module.credstash-reader.reader_policy_arn}"
}

data "template_file" "certstrap" {
  template = "${file("${path.module}/data/certs.tpl.sh")}"
  vars {
    domain_name       = "${var.logstash_dns_name}"
    depot_path        = "${var.certstrap_depot_path}"
    ca_force_new      = "${var.certstrap_ca_force_new}"
    ca_common_name    = "${var.certstrap_ca_common_name}"
    ca_passphrase     = "${var.certstrap_ca_passphrase}"
    credstash_get_cmd = "${module.credstash-reader.get_cmd}"
    credstash_put_cmd = "${module.credstash-reader.put_cmd}"
    ca_cert_name      = "${var.credstash_prefix}${var.credstash_ca_cert_name}"
    ca_key_name       = "${var.credstash_prefix}${var.credstash_ca_key_name}"
    server_cert_name  = "${var.credstash_prefix}${var.credstash_server_cert_name}"
    server_key_name   = "${var.credstash_prefix}${var.credstash_server_key_name}"
  }
}

resource "aws_iam_role" "logstash-role" {
  provisioner "local-exec" {
    command = "${data.template_file.certstrap.rendered}"
  }
  provisioner "local-exec" {
    command = "${path.module}/../credstash/grant.sh create reader ${var.credstash_kms_key_arn} ${aws_iam_role.logstash-role.arn}"
  }
  ## This cleanup action requires at least terraform 0.9.0.
  ## See RFC: https://docs.google.com/document/d/15nEcV7fxskDgYrXoNMl6RYIo10PCiZGle7TP8xitrFE/edit#
  ## and: https://github.com/hashicorp/terraform/issues/386 ## for documentation
  provisioner "local-exec" {
    when       = "destroy"
    on_failure = "continue"
    command    = "${path.module}/../credstash/grant.sh revoke ${var.credstash_kms_key_arn} ${aws_iam_role.logstash-role.arn}"
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
