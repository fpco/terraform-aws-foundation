## cluster-network

This module creates two `aws_subnet` and `aws_route_table_association` as a
pair, named `a` and `c`. Relevant details are passed in as variables.

### Variables

* `name` - The name of this network, this should be unique.
* `access_key` - AWS key id.
* `secret_key` - AWS key secret.
* `region` - AWS region to deploy to.
* `cidr_a` - The CIDR block for subnet a, eg: 10.100.7.0/24.
* `cidr_c` - The CIDR block for subnet c, eg: 10.100.8.0/24.
* `vpc_id` - The ID of the VPC to deploy to.
* `route_table_id` - The ID of the routing table to use.
* `public_ip` - Boolean flag to enable/disable `map_public_ip_on_launch` in each `aws_subnet`.

### Outputs

* `id_a` - The `id` of the AWS subnet `a`.
* `id_c` - The `id` of the AWS subnet `c`.
* `cidr_a` - The `cidr_block` from the AWS subnet `a`.
* `cidr_c` - The `cidr_block` from the AWS subnet `c`.
