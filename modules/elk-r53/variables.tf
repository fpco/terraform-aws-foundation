variable "route53_zone_id" {
  description = "Route53 Zone id where records for LBs will be added to"
  type        = string
}

# Example:
# elasticsearch_dns_name = "elasticsearch.example.com"
# elasticsearch_lb       = {
#    "dns_name" = "${aws_alb.elasticsearch-lb.dns_name}"
#    "zone_id"  = "${aws_alb.elasticsearch-lb.zone_id}"
#  }
variable "elasticsearch_dns_name" {
  description = "DNS name for Elasticsearch"
  type        = string
}

variable "elasticsearch_lb" {
  description = "Elasticsearch DNS and LB info"
  type        = map(string)
}

# Example:
# dns_name = "logstash.example.com"
# logstash = {
#    "dns_name" = "${aws_elb.logstash-lb.dns_name}"
#    "zone_id"  = "${aws_elb.logstash-lb.zone_id}"
#  }
variable "logstash_dns_name" {
  description = "DNS name for Logstash"
  type        = string
}

variable "logstash_lb" {
  description = "Logstash DNS and LB info"
  type        = map(string)
}

# Example:
# kibana_dns_name = "kibana.example.com"
# kibana_lb = {
#    "dns_name" = "${aws_alb.kibana-lb.dns_name}"
#    "zone_id"  = "${aws_alb.kibana-lb.zone_id}"
#  }
variable "kibana_dns_name" {
  description = "DNS name for Kibana"
  type        = string
}

variable "kibana_lb" {
  description = "Kibana DNS and LB info"
  type        = map(string)
}

