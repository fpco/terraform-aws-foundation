## EC2 NAT Instance

**UPDATE THESE DOCS**

This module creates a single EC2 instance in one public subnet, to provide
NAT to one or more private subnets. The instance will use the latest Ubuntu
16.04 AMI.

The module will output the instance ID for use in configuring a route table
for those private subnets.

