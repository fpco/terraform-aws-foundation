## AWS VPC, Scenario 4: VPC w/ Private Subnets, and VPN only

This module creates a VPC with private subnets across one or more availability
zones, a hardware VPN gateway, and a route table for those subnets to pass
traffic through the gateway. No NAT or public subnets/gateway.

Scenario 4 from AWS docs:
http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_Scenarios.html

Note that, when using this module and deploying the VPC for the first time,
Terraform needs the user to add the VPC, Subnets, and then Route Tables. For
example, use targets to apply these updates sequentially:

```
ᐅ terraform plan -out=tf.out -target=module.vpc.module.vpc
ᐅ terraform apply tf.out
ᐅ terraform plan -out=tf.out -target=module.vpc.module.private-subnets
ᐅ terraform apply tf.out
```

This module is not as well tested as the other modules, let us know how it
works for you.
