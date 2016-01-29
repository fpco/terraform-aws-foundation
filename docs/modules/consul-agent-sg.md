## Security Group for Consul Agents

This module creates a _single_ `aws_security_group` resource. This resource has
an ingress rule to allow port `8301`, for TCP and UDP each. Use the security
group on any nodes you wish to use the consul agent on:


### Example

```
# boxed security group for consul leader services, no egress/custom rules
module "consul-agent-sg" {
    source = "../tf-modules/consul-agent-sg"
    name = "${var.name}"
    vpc_id = "${module.test-vpc.id}"
    region = "${var.region}"
    access_key = "${var.access_key}"
    secret_key = "${var.secret_key}"
    cidr_blocks = "${module.test-vpc.cidr_block}"
}

module "my-cluster" {
    source = "../tf-modules/consul-cluster"
    ...
    cluster_security_group_ids = "${module.consul-agent-sg.id}, ${aws_security_group.worker-service.id}, ${module.public-ssh-sg.id}"
```

### Variables

* `name` - A short identifier to use in the name of the security group, comes in
the form `${var.name}-${var.region}-consul-agent`.
* `access_key` - AWS key id.
* `secret_key` - AWS key secret.
* `region` - AWS region to deploy to.
* `vpc_id` - The ID of the VPC to deploy to.
* `cidr_blocks` - The list of CIDR IP blocks allowed to access the consul ports
(as a string).


### Outputs

All outputs map to the same from the `aws_security_group` Terraform resource:

* `id`
* `name`
* `ingress`
