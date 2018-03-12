## AWS VPC, Scenario 3: VPC w/ Public, Private Subnets and a Hardware VPN

This module creates a VPC with both public and private subnets spanning one or
more availability zones, an internet gateway for the public subnets and a
hardware VPN gateway for the private subnets.

Scenario 3 from AWS docs:
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
