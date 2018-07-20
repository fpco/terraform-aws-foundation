# Example to test the VPC Scenario 3 Module

## Environment creation and deployment

This module creates a VPC with both public and private subnets spanning one or
more availability zones, an internet gateway for the public subnets and a
hardware VPN gateway for the private subnets.


To use this example set up AWS credentials and then run the commands in the
following order:

Note that, when using this module and deploying the VPC for the first time,
Terraform needs the user to add the VPC, Subnets, and then Route Tables. For
example, use targets to apply these updates sequentially:

```
make ssh-key
make init
make plan-subnets
make apply
make plan-gateways
make apply
make plan
make apply
```

## Testing

Get the public IP address of the newly created ec2 web instance with the AWS console.

SSH into the machine with the command:

```
ssh -i id_rsa ubuntu@<vpc-ip-address>
```

## Destruction

To destroy the test environment run the following commands:

```
make destroy
make clean
```

## Notes

* This example was last tested with `Terraform v0.11.7`
* This example assumes AWS credentials setup with access to the **us-east-2** region.
* This example when deploying the VPC for the first time, Terraform needs the user to add the VPC, Subnets, and then Route Tables.



