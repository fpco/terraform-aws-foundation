# module outputs!
output "leader_fqdn" {
    value = "${aws_route53_record.leaders.fqdn}"
}
output "zone_id" {
    value = "${aws_route53_zone.leaders.zone_id}"
}
output "name_servers" {
    value = "${aws_route53_zone.leaders.name_servers}"
}
