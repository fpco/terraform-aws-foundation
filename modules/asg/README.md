## ASG (Autoscaling Group)

The purpose of this module is to provide a Launch Configuration and Autoscaling
Group as a pair.

The module supports:

* spanning N Availability Zones
* load balancers may be associated with the ASG
* the health checks are not yet parametized, (easy to change)
* the Launch Configuration supports an arbitrary list of security groups
* `lifecycle` and `create_before_destroy` are used to ensure updates are graceful
* public IPs may be enabled/disabled
* supports appending `extra_tags`
* all important details (instance type, ami, key, user data, iam profile) are
  specified as variables in the modules.

Note that, Terraform does not run a rolling update when an ASG/LC pair have
changed. After the ASG/LC have been updated, the EC2 instances running before
the update will still be running. As a result, you will need to terminate each
of those instances to perform a rolling update.

