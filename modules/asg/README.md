## ASG (Autoscaling Group)

The purpose of this module is to provide a Launch Configuration and Autoscaling
Group as a pair.

The module supports:

* spanning N Availability Zones
* load balancers may be associated with the ASG
* Application load balancer defining the variable alb_target_group_arn
* the health checks are not yet parametized, (easy to change)
* the Launch Configuration supports an arbitrary list of security groups
* `lifecycle` and `create_before_destroy` are used to ensure updates are graceful
* lifecycle hooks which get enabled using `enable_launching_hook` and/or
  `enable_terminating_hook`, don't forget to set proper `lifecycle_sns_topic_arn`
  and `aws_role_arn` for setting up SNS
* public IPs may be enabled/disabled
* supports appending `extra_tags`
* all important details (instance type, ami, key, user data, iam profile, additional
  volumes encryption) are
  specified as variables in the modules.

Note that, Terraform does not run a rolling update when an ASG/LC pair have
changed. After the ASG/LC have been updated, the EC2 instances running before
the update will still be running. As a result, you will need to terminate each
of those instances to perform a rolling update.

