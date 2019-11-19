output "target_group_arn" {
  value = aws_lb_target_group.lb-tg.arn
}

output "listener_arn" {
  value = aws_lb_listener.lb-listener.arn
}
