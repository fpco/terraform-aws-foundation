# Example to test the VPC Scenario 1 Module

## Environment creation and deployment

To use this example set up AWS credentials and then run the commands in the 
following order:

```
make ssh-key
make init
make plan-vpc
make apply
make plan-subnets
make apply
make plan-gateway
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
- This example was last tested with `Terraform v0.11.6`
- This example assumes AWS credentials setup with access to the **us-east-2** region.
