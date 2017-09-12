variable "route53_zone_id" {
  description = "Route53 Zone id where records for ELBs will be added to"
}

# Example:
# elasticsearch = {
#    "dns_name"     = "elasticsearch.example.com"
#    "elb_dns_name" = "${aws_elb.elasticsearch-elb.dns_name}"
#    "elb_zone_id"  = "${aws_elb.elasticsearch-elb.zone_id}"
#  }
variable "elasticsearch" {
  description = "Elasticsearch DNS and ELB info"
  type = "map"
}


# Example:
# logstash = {
#    "dns_name"     = "logstash.example.com"
#    "elb_dns_name" = "${aws_elb.logstash-elb.dns_name}"
#    "elb_zone_id"  = "${aws_elb.logstash-elb.zone_id}"
#  }
variable "logstash" {
  description = "Logstash DNS and ELB info"
  type = "map"
}


# Example:
# kibana = {
#    "dns_name"     = "kibana.example.com"
#    "elb_dns_name" = "${aws_elb.kibana-elb.dns_name}"
#    "elb_zone_id"  = "${aws_elb.kibana-elb.zone_id}"
#  }
variable "kibana" {
  description = "Kibana DNS and ELB info"
  type = "map"
}
