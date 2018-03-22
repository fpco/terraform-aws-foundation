## Cloud Dev Workspace on AWS

Run a dev workspace on a single EC2 instance.
This instance will be part of a single-node autoscaling group
that shares an EBS volume to store data.

Note that there is a peculiarity with the EBS volume in that it
requires some manual setup the very first time to make it available
for use (unless a snapshot id is supplied):

    parted --script /dev/xvdf -- mklabel msdos
    parted --script /dev/xvdf -- mkpart primary 0 -1
    mkfs -t ext4 -F /dev/xvdf1
    e2label /dev/xvdf1 data

After running the above code to initialise the EBS, terminate the instance
and the autoscaling group will bring up a new instance that will be running
gitlab once it is done initialising.

