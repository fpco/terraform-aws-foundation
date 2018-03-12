## AWS VPC, Scenario 2

**VPC with both Public and Private Subnets, NAT and Internet Gateway**

This module creates a VPC with both public and private subnets across one or
more availability zones. The public subnets have routes to the public
thru an Internet Gateway, while the private subnets use NAT Gateways.

Scenario 2 from AWS docs:
http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_Scenarios.html

Note that, when using this module and deploying the VPC for the first time,
Terraform needs the user to add the VPC, Subnets, and then Route Tables. For
example, use targets to apply these updates sequentially:

    ᐅ terraform plan -out=tf.out -target=module.vpc.module.vpc
    ᐅ terraform apply tf.out
    ᐅ terraform plan -out=tf.out -target=module.vpc.module.public-subnets
    ᐅ terraform apply tf.out
    ᐅ terraform plan -out=tf.out -target=module.vpc.module.public-gateway
    ᐅ terraform apply tf.out
    ᐅ terraform plan -out=tf.out -target=module.vpc.module.private-subnets
    ᐅ terraform apply tf.out
