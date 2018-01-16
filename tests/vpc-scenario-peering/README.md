To run this example invoke the following command in order:

* make generate-ssh-keypair
* make init
* make init-plan
* make apply
* make gateway-plan
* make apply
* make plan
* make apply

This should bring up two separate VPC's and run 2 AWS instances, one in each VPC, that should be able to communicate to
each other.

To test this out go to the AWS console and find the public IP address of VPC1-MACHINE and run the following command to
ssh into it:

  ssh -i id_rsa ubuntu@<VPC1-MACHINE-IP>

Then get the PRIVATE IP address from VPC2-MACHINE, also from the AWS console, and try ssh-ing into it from the VPC1-MACHINE.
If you're connection doesn't hang, that is if you get a permission denied this indicates that the VPC peering is working.

The same test should work from the VCP2-MACHINE as well.

To destroy the test environment run the following commands:

* make destroy-plan
* make apply
* make clean

