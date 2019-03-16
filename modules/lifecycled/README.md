## Terraform example

Quick way of bootstrapping an autoscaling group with a termination lifecycle hook enabled
and `lifecycled` running on the instance, using terraform. It creates the the following:

- Autoscaling group running Amazon linux 2.
- Topic and lifecycle hook which sends termination notices to the topic.
- Log group where each instance will send their `lifecycled` logs.
- Creating a S3 bucket and uploading `lifecycled` from your computer.
- Giving the instance profile necessary permissions to run `lifecycled` and:
  - Download `lifecycled` from the S3 bucket.
  - Send daemon logs to CloudWatch.

### Prerequisites

You may need to create an EC2 key pair if you want to be able to SSH into the instances to debug.

### Usage

1. Compile `lifecycled` for linux with `make release`.

2. Configure [example.tf](./example.tf) and run the following:

```bash
terraform init
terraform apply
```

Terraform state will be stored locally unless you add a remote backend to `example.tf`,
and when you are done testing you can tear everything down with `terraform destroy`.

3. Optional: SSH into the instance:

```bash
ssh -i ~/Downloads/lifecycled-example.pem ec2-user@<instance-ip>
```

### Testing

To change the desired capacity using the console

* Open the Amazon EC2 console at https://console.aws.amazon.com/ec2/.

* On the navigation pane, under Auto Scaling, choose Auto Scaling Groups.

* Select your Auto Scaling group.

* On the Details tab, choose Edit.

* For Desired, increase the current value by 1. If this value exceeds Max, you must also increase the value of Max by 1.

* Choose Save.

* After a few minutes, you'll receive notification for the event. If
  you do not need the additional instance that you launched for this
  test, you can decrease Desired by 1. After a few minutes, you'll
  receive notification for the event.


## Caveat

You may need to run `terraform apply` twice to update the binary running on the example instance.
