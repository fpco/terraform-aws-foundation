output "leader_fqdn" {
  value       = aws_route53_record.leaders.fqdn
  description = "FQDN (for the Consul Leaders) exported from `aws_route53_record`"
}

output "zone_id" {
  value       = aws_route53_zone.leaders.zone_id
  description = "Zone ID exported from `aws_route53_record`"
}

output "name_servers" {
  value       = aws_route53_zone.leaders.name_servers
  description = "Name servers exported from `aws_route53_record`"
}

