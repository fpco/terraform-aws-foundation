# ALB default forward

With `alb` module setup basic ALB, this module (can be used multiple times) is to setup the detail listener and target group with default action forward.

To have more control on the action, attach `aws_lb_listener_rule` resource to this listener.

Checkout example/alb-test for usage.
