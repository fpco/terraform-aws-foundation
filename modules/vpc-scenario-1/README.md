## AWS VPC, Scenario 1: VPC w/ Public Subnets

This module creates a VPC with public subnets across one or more availability
zones, an internet gateway, and a route table for those subnets to pass
traffic through the gateway.

Scenario 1 from AWS docs:
http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_Scenarios.html

Note that, when using this module and deploying the VPC for the first time,
Terraform needs the user to add the VPC, Subnets, and then Route Tables. For
example, use targets to apply these updates sequentially:

    ᐅ tfp -out=tf.out -target=module.vpc.module.vpc
    ᐅ tfa tf.out
    ᐅ tfp -out=tf.out -target=module.vpc.module.public-subnets
    ᐅ tfa tf.out
    ᐅ tfp -out=tf.out -target=module.vpc.module.public-gateway
    ᐅ tfa tf.out

