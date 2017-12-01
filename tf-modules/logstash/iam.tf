module "credstash-grant" {
  source            = "../credstash-grant"
  kms_key_arn       = "${var.credstash_kms_key_arn}"
  reader_policy_arn = "${var.credstash_reader_policy_arn}"
  reader_context    = "env=${var.name_prefix}"
  roles_count       = 1
  roles_arns        = ["${aws_iam_role.logstash-role.arn}"]
  roles_names       = ["${aws_iam_role.logstash-role.name}"]
}

resource "aws_iam_instance_profile" "logstash-profile" {
  name = "${var.name_prefix}-logstash-profile"
  role = "${aws_iam_role.logstash-role.name}"
}

data "template_file" "certstrap" {
  template = "${file("${path.module}/data/certs.tpl.sh")}"

  vars {
    domain_name       = "${var.logstash_dns_name}"
    depot_path        = "${var.certstrap_depot_path}"
    ca_force_new      = "${var.certstrap_ca_force_new}"
    ca_common_name    = "${var.certstrap_ca_common_name}"
    ca_passphrase     = "${var.certstrap_ca_passphrase}"
    credstash_get_cmd = "${var.credstash_get_cmd}"
    credstash_put_cmd = "${var.credstash_put_cmd}"
    credstash_context = "env=${var.name_prefix}"
    ca_cert_name      = "${var.name_prefix}-logstash-ca-cert"
    ca_key_name       = "${var.name_prefix}-logstash-ca-key"
    server_cert_name  = "${var.name_prefix}-logstash-server-cert"
    server_key_name   = "${var.name_prefix}-logstash-server-key"
    client_cert_name  = "${var.name_prefix}-logstash-client-cert"
    client_key_name   = "${var.name_prefix}-logstash-client-key"
  }
}

resource "aws_iam_role" "logstash-role" {
  provisioner "local-exec" {
    command = "${data.template_file.certstrap.rendered}"
  }

  name_prefix = "${var.name_prefix}-logstash-role-"

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
