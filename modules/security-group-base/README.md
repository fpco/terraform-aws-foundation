## Security Group Base

Create the `aws_security_group` that we will then go attach a bunch of
`aws_security_group_rule`s to.

Note that, when setting `aws_security_group.name`, the AWS API will require
removing and recreating the resource if the `name` is changed.

