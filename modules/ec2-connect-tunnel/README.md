# EC2 Instance Connect tunnel

Creates a s single node ASG (using the `singe-node-asg` module) allowing SSH
connections using [EC2 Instance Connect](https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Connect-using-EC2-Instance-Connect.html).
Assumes Ubuntu AMI to be used (`ec2-instance-connect` gets installed using
`apt`). Use `ec2-connect-role` to setup an IAM role for SSH access.
