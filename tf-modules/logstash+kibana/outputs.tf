// Elastic Load Balancer DNS for Logstash
output "logstash_elb_dns" {
  value = "${module.logstash-kibana.elb_dns}"
}

// Elastic Load Balancer DNS for Kibana
output "kibana_elb_dns" {
  value = "${module.kibana.elb_dns}"
}

// Until terraform 9 is released this is a cleanup action that needs to be
//invoked prior to destruction
output "kms_grant_revoke_cmd" {
  value = "${module.logstash-kibana.kms_grant_revoke_cmd}"
}
