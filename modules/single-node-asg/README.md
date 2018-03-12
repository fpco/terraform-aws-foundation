## Single-Node Auto-Scaling Group

This module uses an EBS volume and an auto-scaling group with a single-node
to establish a reliable and robust pattern for HA in various forms.

NOTE: This module assumes that all the dependencies for the
`../init-snippet-attach-ebs-volume` module are satisfied
by the `init_prefix`.
In particular, this means that the aws cli tool and ec2metadata are installed.
