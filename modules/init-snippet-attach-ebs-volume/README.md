## Init Snippet: Attach EBS Volume

Create an init snippet that will attach an EBS volume to the instance.
This snippet requires that the instance has an IAM instance profile which
grants it the access needed to find and attach the EBS volume. There are
other modules in this repo which can create EBS volumes with IAM profiles
for each volume. Attaching the EBS volume will loop until it succeeds.

