# Active Directory with Linux EC2 join (from ASG)

The terraform code is built on top of
[vpc-scenario1](https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Scenario1.html)
with two additional private subnets and a NAT gateway on a public
subnet. This example demonstrate how an Linux EC2 instance present in
[ASG](https://docs.aws.amazon.com/autoscaling/ec2/userguide/AutoScalingGroup.html)
joins an Active directory when it gets newly spawned. The only
difference between this example and the [ad-asg](../ad-asg/) is
operating system.

We use a custom AMI to have active directory related softwares
available in our instance.

## AMI Creation

``` shellsession
$ cd packer
$ make build
....
    amazon-ebs: Stopping instance
==> amazon-ebs: Waiting for the instance to stop...
==> amazon-ebs: Creating AMI centos7-server-ami-1569914218 from instance i-074b5b3a18dde102b
    amazon-ebs: AMI: ami-0473c8e0ea159ff34
==> amazon-ebs: Waiting for AMI to become ready...
==> amazon-ebs: Terminating the source AWS instance...
==> amazon-ebs: Cleaning up any extra volumes...
==> amazon-ebs: Destroying volume (vol-0ba5fc0a8d3e4d180)...
==> amazon-ebs: Deleting temporary security group...
==> amazon-ebs: Deleting temporary keypair...
Build 'amazon-ebs' finished.

==> Builds finished. The artifacts of successful builds are:
--> amazon-ebs: AMIs were created:
us-east-2: ami-0473c8e0ea159ff34
```

Note the new ami-id built from the above command. You would need to
put that in the [variables.tf](./variables.tf) for the `ami_id`
variable.

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

## Execution

Once you run the above commands, you will get an output like this:

``` shellsession
...
module.nat-gateway.aws_route_table_association.private-rta[0]: Refreshing state... [id=rtbassoc-0be4f2c71ef12e768]
module.nat-gateway.aws_route_table_association.private-rta[1]: Refreshing state... [id=rtbassoc-08a1f878abab73841]
aws_ssm_association.associate_ssm: Refreshing state... [id=996ff9a8-0931-4000-85aa-d01ef536f5a7]


Outputs:

asg-name = test-ad-project-asg-cluster20190919093341776000000005
microsoft-ad_dns_ip_addresses = [
  "10.23.21.134",
  "10.23.22.45",
]
microsoft-ad_dns_name = dev.fpcomplete.local
```

## Testing

You need to test that the Linux EC2 instance from the ASG actually
joined the Active directory. Let's SSH into our instance and verify
it:

* Find the public IP address of your EC2 instance either via the `aws`
  cli or through the console.
* SSH into the instance.
* Verify if the instance has actually joined the AD domain:

``` shellsession
[centos@ip-10-23-11-81 ~]$ realm list
dev.fpcomplete.local
  type: kerberos
  realm-name: DEV.FPCOMPLETE.LOCAL
  domain-name: dev.fpcomplete.local
  configured: kerberos-member
  server-software: active-directory
  client-software: sssd
  required-package: oddjob
  required-package: oddjob-mkhomedir
  required-package: sssd
  required-package: adcli
  required-package: samba-common-tools
  login-formats: %U@dev.fpcomplete.local
  login-policy: allow-realm-logins
```

## Destruction

To destroy the test environment run the following commands:

```
$ make destroy
$ make clean
```

## Debugging

The script execution using `user_data` is usually hard to debug. The
execution of our [bootstrap script](./bootstrap.linux.txt) results in
a log which can be viewed [by either SSHing the instance or through
AWS Console](https://stackoverflow.com/q/15904095/1651941).

## Reference

* [AWS docs on AWS Managed Microsoft AD](https://docs.aws.amazon.com/directoryservice/latest/admin-guide/ms_ad_getting_started.html)
* [AWS docs on Joining an EC2 instance](https://docs.aws.amazon.com/directoryservice/latest/admin-guide/ms_ad_join_instance.html)
* [AWS docs on Systems manager and AD](https://aws.amazon.com/premiumsupport/knowledge-center/ec2-systems-manager-dx-domain/)
