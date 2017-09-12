/** This module adds all domain names to the Hosted Zone
 *  with an A Alias to their respective ELBs
**/

resource "aws_route53_record" "elasticsearch-elb" {
  zone_id = "${var.route53_zone_id}"
  name    = "${var.elasticsearch["dns_name"]}"
  type    = "A"

  alias {
    name                   = "${var.elasticsearch["elb_dns_name"]}"
    zone_id                = "${var.elasticsearch["elb_zone_id"]}"
    evaluate_target_health = true
  }
}

resource "aws_route53_record" "logstash-elb" {
  zone_id = "${var.route53_zone_id}"
  name    = "${var.logstash["dns_name"]}"
  type    = "A"

  alias {
    name                   = "${var.logstash["elb_dns_name"]}"
    zone_id                = "${var.logstash["elb_zone_id"]}"
    evaluate_target_health = true
  }
}

resource "aws_route53_record" "kibana-elb" {
  zone_id = "${var.route53_zone_id}"
  name    = "${var.kibana["dns_name"]}"
  type    = "A"

  alias {
    name                   = "${var.kibana["elb_dns_name"]}"
    zone_id                = "${var.kibana["elb_zone_id"]}"
    evaluate_target_health = true
  }
}
