## Persistent Data on EBS

This module provides an EBS volume and associated IAM profile/role to be
used with an EC2 instance or auto-scaling group. This module is best when
used in conjunction with a single-node auto-scaling group, and with the
init-snippet that attaches the named EBS volume on boot.

