/**
 * ## AWS Route 53 Zone and Records for Consul Leaders
 *
 * This module is for use with a cluster of Consul leaders, it provides a DNS
 * zone and records that point to the IPs where the consul leaders _might_ be.
 * While there may only be 3 to 9 Consul leaders, those leaders may move around
 * (they are an auto-scaling group), and we don't knoe _exactly_ where they are,
 * but if a consul agent uses the DNS record from this module, the agent will
 * continue to retry until it has found a leader. In practice, this works well,
 * and Consul agents will find their leaders in 5 - 90 seconds (depending on
 * the retry period and other settings configured in each agent).
 *
 */

resource "aws_route53_zone" "leaders" {
  name   = "${var.domain}"
  vpc_id = "${var.vpc_id}"
}

resource "aws_route53_record" "leaders" {
  zone_id = "${aws_route53_zone.leaders.zone_id}"
  name    = "${var.name}"
  type    = "A"
  ttl     = "${var.ttl}"

  records = [
    "${cidrhost(var.cidr_a, 4)}",
    "${cidrhost(var.cidr_c, 4)}",
    "${cidrhost(var.cidr_a, 5)}",
    "${cidrhost(var.cidr_c, 5)}",
    "${cidrhost(var.cidr_a, 6)}",
    "${cidrhost(var.cidr_c, 6)}",
    "${cidrhost(var.cidr_a, 7)}",
    "${cidrhost(var.cidr_c, 7)}",
    "${cidrhost(var.cidr_a, 8)}",
    "${cidrhost(var.cidr_c, 8)}",
    "${cidrhost(var.cidr_a, 9)}",
    "${cidrhost(var.cidr_c, 9)}",
    "${cidrhost(var.cidr_a, 10)}",
    "${cidrhost(var.cidr_c, 10)}",
    "${cidrhost(var.cidr_a, 11)}",
    "${cidrhost(var.cidr_c, 11)}",
    "${cidrhost(var.cidr_a, 12)}",
    "${cidrhost(var.cidr_c, 12)}",
    "${cidrhost(var.cidr_a, 13)}",
    "${cidrhost(var.cidr_c, 13)}",
    "${cidrhost(var.cidr_a, 14)}",
    "${cidrhost(var.cidr_c, 14)}",
    "${cidrhost(var.cidr_a, 15)}",
    "${cidrhost(var.cidr_c, 15)}",
  ]
}
