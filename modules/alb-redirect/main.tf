resource "aws_lb_listener" "lb-listener" {
  load_balancer_arn = var.lb_arn
  port              = var.http_port
  protocol          = "HTTP"
  default_action {
    type             = "redirect"
    redirect {
      port        = var.https_port
      protocol    = "HTTPS"
      status_code = "HTTP_301"
    }
  }
}
