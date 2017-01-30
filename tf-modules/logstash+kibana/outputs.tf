output "logstash_elb_dns" {
  value = "${module.logstash-kibana.elb_dns}"
}

output "kibana_elb_dns" {
  value = "${module.kibana.elb_dns}"
}
