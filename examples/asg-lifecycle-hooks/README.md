# Example to test basic ASG integration with lifecycle hooks

This example uses [lifecycled](https://github.com/buildkite/lifecycled) to process
lifecycle events. As of version 3.0.2 `lifecycled` supports only instance termination
events and reacts to a termination event for a node it is running on.

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
ssh -i id_rsa ec2-user@<ec2-ip-address>
```

You can see in the machine that `lifecycled` daemon would be
running. You can check the status of the service using

```
systemctl status lifecycled.service
```

Output from a handler could be seen in the service log e.g. by using

```
journalctl -f -u lifecycled.service
```


## Test the Notification

To generate a notification for a termination event, update the Auto Scaling group by decreasing the desired capacity of the Auto Scaling group by 1. You receive a notification within a few minutes after instance termination.

To change the desired capacity using the console

    Open the Amazon EC2 console at https://console.aws.amazon.com/ec2/.

    On the navigation pane, under Auto Scaling, choose Auto Scaling Groups.

    Select your Auto Scaling group.

    On the Details tab, choose Edit.

    For Desired, decrease the current value by 1.

    Choose Save.

    After a few minutes, you'll see that the lifecycle-handler.sh script will be executed and it's side effect operation will be performed: in the log of lifecycled.service you'll see a line with something like "hello from the handler, received autoscaling:EC2_INSTANCE_TERMINATING i-01234567890123456"

## Destruction

To destroy the test environment run the following commands:

```
make destroy
make clean
```

## Notes
- This example was last tested with `Terraform v0.12.4`
- This example assumes AWS credentials setup with access to the **us-east-2** region.
