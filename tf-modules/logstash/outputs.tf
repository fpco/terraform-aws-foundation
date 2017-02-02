output "elb_dns" {
  value = "${aws_elb.logstash-elb.dns_name}"
}

output "elb_name" {
  value = "${aws_elb.logstash-elb.name}"
}

output "logstash_role_name" {
  value = "${aws_iam_role.logstash-role.name}"
}

// Until terraform 9 is released this is a cleanup action that needs to be
//invoked prior to destruction
output "kms_grant_revoke_cmd" {
  value = "${path.module}/../credstash/grant.sh revoke ${var.credstash_kms_key_arn} ${aws_iam_role.logstash-role.arn}"
}
