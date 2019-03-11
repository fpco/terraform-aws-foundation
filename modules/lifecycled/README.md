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

## Caveat

You may need to run `terraform apply` twice to update the binary running on the example instance.
