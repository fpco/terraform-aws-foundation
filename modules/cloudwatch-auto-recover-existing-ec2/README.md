## Cloud Watch EC2 Auto-Recovery

This module creates one or more `aws_cloudwatch_metric_alarm` resources, each
of which is associated with a specific EC2 `InstanceId`. For select failure
scenarios, this resource will have AWS auto-recover an EC2 instance.

