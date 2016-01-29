## Autoscaling Clusters (built on Consul)

This module reuses two other modules in this suite:

* `asg`
* `cluster-net`

The purpose of this module is to pair the ASG with subnets created specifically
for the ASG. Similarly, the individual parts (`asg` / `cluster-net`), exist to
allow flexibility, and to enable user-developers with the option of putting a
new ASG into an existing network, and upfront or further down the line.


### Example

```
# cluster of "workers", built on a cluster of consul agents
module "cworkers-a" {
    source = "../tf-modules/consul-cluster"
    ami = "${var.ami}"
    name = "${var.name}"
    max_nodes = 5
    min_nodes = 3
    desired_capacity = 3
    key_name = "${aws_key_pair.tests.key_name}"
    access_key = "${var.access_key}"
    secret_key = "${var.secret_key}"
    region = "${var.region}"
    cidr_minions_a = "${var.cidr_minions_a}"
    cidr_minions_c = "${var.cidr_minions_c}"
    vpc_id = "${module.test-vpc.id}"
    route_table_id = "${module.test-vpc.route_table_id}"
    cluster_security_group_ids = "${module.management-cluster.nomad_agent_sg}, ${module.consul-agent-sg.id}, ${aws_security_group.worker-service.id}, ${module.public-ssh-sg.id}"
    user_data = "${module.worker-init.user_data}"
}
```


### Variables

* `name` - The name of this auto-scaling cluster, this should be unique.
* `key_name` - The name of the (AWS) SSH key to associate with the instances in
the cluster.
* `ami` - The base AMI for each AWS instance created.
* `iam_profile` - The IAM profile to associate with AWS instances in the ASG.
* `instance_type` - The type of AWS instance (size).
* `user_data` - The user_data string to pass to cloud-init.
* `max_nodes` - The maximum number of nodes in each group.
* `min_nodes` - The minimum number of nodes in each group.
* `desired_capacity` - The desired number of nodes in each group.
* `access_key` - AWS key id.
* `secret_key` - AWS key secret.
* `cidr_minions_a` - The CIDR block for subnet a, eg: 10.100.7.0/24.
* `cidr_minions_c` - The CIDR block for subnet c, eg: 10.100.8.0/24.
* `region` - AWS region to deploy to.
* `vpc_id` - The ID of the VPC to deploy to.
* `route_table_id` - The ID of the routing table to use.
* `public_ip` - Boolean flag to enable/disable `map_public_ip_on_launch` in each `aws_subnet`.


### Outputs

* `subnet_a_id` - The `id` of one `aws_subnet` created for the cluster's ASG.
* `subnet_c_id` - The `id` of the other `aws_subnet` created for the cluster's ASG.
* `asg_name` - The name of the ASG created for the cluster.
