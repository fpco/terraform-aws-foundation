## ASG (Autoscaling Group)

The purpose of this module is to provide a Launch Configuration and Autoscaling
Group as a pair.

The ASG supports spanning two regions and load balancers may be specified. At
present, the health checks are defaults and hardcoded. The Launch Configuration
supports an arbitrary list of security groups, public IPs may be enabled/disabled,
and all important details (instance type, ami, key, user data, iam profile) are
specified as variables in the modules.


### Variables

* `name` - The name of this auto-scaling cluster, this should be unique.
* `suffix` - The suffix to the name of this auto-scaling cluster.
* `key_name` - The name of the (AWS) SSH key to associate with the instance.
* `ami` - The base AMI for each AWS instance created.
* `iam_profile` - The IAM profile to associate with AWS instances in the ASG.
* `instance_type` - The type of AWS instance (size).
* `user_data` - The user_data string to pass to cloud-init.
* `cidr_a` - The CIDR block for subnet a, eg: 10.100.7.0/24.
* `cidr_c` - The CIDR block for subnet c, eg: 10.100.8.0/24.
* `max_nodes` - The maximum number of nodes in each group.
* `min_nodes` - The minimum number of nodes in each group.
* `desired_capacity` - The desired number of nodes in each group.
* `access_key` - AWS key id.
* `secret_key` - AWS key secret.
* `region` - AWS region to deploy to.
* `vpc_id` - The ID of the VPC to deploy to.
* `route_table_id` - The ID of the routing table to use.
* `public_ip` - Boolean flag to enable/disable `map_public_ip_on_launch` in each `aws_subnet`.
* `az_list` - string-list of availability zones to associate with the ASG.
* `subnet_ids` - string-list of subnets to associate with the ASG (by id).
* `security_group_ids` - string-list of security groups to associate with the ASG (by id).
* `elb_names` - string-list of load balancers to associate with the ASG (by name).


### Outputs

* `name` - The name of the Autoscaling Group.
* `id` - The id of the Autoscaling Group.
* `lc_id` - The id of the Launch Configuration.
* `lc_name` - The name of the Launch Configuraiton.
