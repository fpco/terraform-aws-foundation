# Example to test basic ASG integration with lifecycle hooks

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

You can see in the machine that `lifecycled` daemon would be
running. You can check the status of the service using

```
systemctl status lifecycled.service
```

You can also see that the logs from the lifecycled would be continously
pushed to the Amazon's CloudWatch logs.

## Destruction

To destroy the test environment run the following commands:

```
make destroy
make clean
```

## Notes
- This example was last tested with `Terraform v0.11.11`
- This example assumes AWS credentials setup with access to the **us-east-2** region.
