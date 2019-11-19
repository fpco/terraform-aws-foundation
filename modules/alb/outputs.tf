output "lb_dns_name" {
  value = aws_lb.alb.dns_name
}

output "lb_zone_id" {
  value = aws_lb.alb.zone_id
}

output "security_group_id" {
  value = aws_security_group.alb_sg.id
}

output "lb_arn" {
  value = aws_lb.alb.arn
}
