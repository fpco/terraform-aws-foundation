## Auto-Recovering EC2 Instances

**UPDATE THESE DOCS**

Use the `aws_instance` and `aws_cloudwatch_metric_alarm` resources in the
following pattern:

* run N instances, where N is the number of IP addresses in the `private_ips`
  list parameter to the module
* set the private IP addresses, don't get something random from AWS
* accept arbitrary `user_data`
* setup a metric alarm and action to auto-recover instances that fail the
  health check
* no use of instance store, or dedicated EBS volumes which follow an instance

This pattern is useful for:

* DNS servers
* a server that needs a fixed IP address
* a server which should be automatically replaced
* a server that does not need some dedicated EBS volume to follow (state is
  not important

Also note that the number of subnets provided does not need to match the
number of private IP addresses. Specifically, the `element()` interpolation
function is used for `aws_instance.subnet_id`, and that function will wrap
using a standard mod algorithm.

### Example

    # The DNS servers
    module "dns" {
      source = "fpco/foundation/aws//modules/ec2-auto-recover-instances"

      name_prefix         = "${var.name}"
      ami                 = "${data.aws_ami.ubuntu-xenial.id}"
      key_name            = "${aws_key_pair.main.id}"
      subnet_ids          = ["${module.private-subnets.ids}"]
      private_ips         = ["${var.list_of_ips}"]
      security_group_ids  = [
        "${module.dns-server-sg.id}",
        "${module.public-ssh-sg.id}",
        "${module.open-egress-sg.id}",
      ]
      user_data = <<END_INIT
    ufw allow 53
    echo "10.10.0.10 foobar.${var.private_dns_zone_name}" >> /etc/hosts
    END_INIT
      alarm_actions = []
    }


### Note on Private IPs

```
# if we have no private_ips in our list, use the subnet_ids to determine how many
# EC2 instances to create. This lets the module create N instances across M subnets
# (interleave), or N instances across N subnets.
data "template_file" "cnt" {
  template = "$${ip_count ? ip_count : subnet_count}"

  vars {
    ip_count     = "${length(var.private_ips)}"
    subnet_count = "${length(var.subnet_ids)}"
  }
}
```

