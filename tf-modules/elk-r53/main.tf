/**
 * ## ELK Route53
 *
 * This module adds all domain names to the Hosted Zone, with an A Alias to
 * their respective load balancers.
 *
**/

resource "aws_route53_record" "elasticsearch-lb" {
  zone_id = "${var.route53_zone_id}"
  name    = "${var.elasticsearch_dns_name}"
  type    = "A"

  alias {
    name                   = "${var.elasticsearch_lb["dns_name"]}"
    zone_id                = "${var.elasticsearch_lb["zone_id"]}"
    evaluate_target_health = true
  }
}

resource "aws_route53_record" "logstash-lb" {
  zone_id = "${var.route53_zone_id}"
  name    = "${var.logstash_dns_name}"
  type    = "A"

  alias {
    name                   = "${var.logstash_lb["dns_name"]}"
    zone_id                = "${var.logstash_lb["zone_id"]}"
    evaluate_target_health = true
  }
}

resource "aws_route53_record" "kibana-lb" {
  zone_id = "${var.route53_zone_id}"
  name    = "${var.kibana_dns_name}"
  type    = "A"

  alias {
    name                   = "${var.kibana_lb["dns_name"]}"
    zone_id                = "${var.kibana_lb["zone_id"]}"
    evaluate_target_health = true
  }
}
