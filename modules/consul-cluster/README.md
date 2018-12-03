
## Autoscaling Clusters (built on Consul)

**DEPRECATED** - This module will be removed in a future release.

This module reuses two other modules in this suite:

* `asg`
* `cluster-net`

The purpose of this module is to pair the ASG with subnets created specifically
for the ASG. Similarly, the individual parts (`asg` / `cluster-net`), exist to
allow flexibility, and to enable user-developers with the option of putting a
new ASG into an existing network, and upfront or further down the line.

NOTES:

* this should be updated to support N number of subnets
* need to plumb in support for `public_ip` and `elb`

### Example

```
# cluster of "workers", built on a cluster of consul agents
module "cworkers-a" {
    source = "../modules/consul-cluster"
    ami = "${var.ami}"
    name = "${var.name}"
    max_nodes = 5
    min_nodes = 3
    desired_capacity = 3
    key_name = "${aws_key_pair.tests.key_name}"
    region = "${var.region}"
    cidr_minions_a = "${var.cidr_minions_a}"
    cidr_minions_c = "${var.cidr_minions_c}"
    vpc_id = "${module.test-vpc.id}"
    route_table_id = "${module.test-vpc.route_table_id}"
    cluster_security_group_ids = "${module.management-cluster.nomad_agent_sg}, ${module.consul-agent-sg.id}, ${aws_security_group.worker-service.id}, ${module.public-ssh-sg.id}"
    user_data = "${module.worker-init.user_data}"
}
```
