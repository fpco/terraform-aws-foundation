resource "aws_route53_zone" "leaders" {
  name   = "consul-${var.name}.leaders"
  vpc_id = "${var.vpc_id}"
}

resource "aws_route53_record" "leaders" {
  zone_id = "${aws_route53_zone.leaders.zone_id}"
  name    = "consul-${var.name}.leaders"
  type    = "A"
  ttl     = "300"

  records = [
    "${var.cidr_prefix_a}.5",
    "${var.cidr_prefix_c}.5",
    "${var.cidr_prefix_a}.6",
    "${var.cidr_prefix_c}.6",
    "${var.cidr_prefix_a}.7",
    "${var.cidr_prefix_c}.7",
    "${var.cidr_prefix_a}.8",
    "${var.cidr_prefix_c}.8",
    "${var.cidr_prefix_a}.9",
    "${var.cidr_prefix_c}.9",
    "${var.cidr_prefix_a}.10",
    "${var.cidr_prefix_c}.10",
    "${var.cidr_prefix_a}.11",
    "${var.cidr_prefix_c}.11",
    "${var.cidr_prefix_a}.12",
    "${var.cidr_prefix_c}.12",
    "${var.cidr_prefix_a}.13",
    "${var.cidr_prefix_c}.13",
    "${var.cidr_prefix_a}.14",
    "${var.cidr_prefix_c}.14",
    "${var.cidr_prefix_a}.15",
    "${var.cidr_prefix_c}.15",
  ]
}
