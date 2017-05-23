output "elb_dns" {
  value = "${aws_elb.elasticsearch-elb.dns_name}"
}

output "elb_r53_id" {
  value = "${aws_elb.elasticsearch-elb.zone_id}"
}

