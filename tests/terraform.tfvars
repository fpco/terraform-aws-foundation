# see variables.tf for more complete descriptions of each variable here

#name = "test"

# Update these with AWS key id/secret
access_key = "FOOBAR"
secret_key = "FOOBAR"

# This is the key consul leaders will use, create with `consul keygen`
consul_secret_key = ""
consul_master_token = ""

# Update these based on your SSH pub key contents and location
ssh_pubkey = ""
key_file="~/.ssh/consul"
key_name="consul-asg"

region = "us-west-2"
ami = "ami-289aab29"


# You might want to update these, but most likely not
#cidr_prefix_leader_a = "10.38.30"
#cidr_prefix_leader_c = "10.38.31"
#cidr_minions_a = "10.38.50.0/24"
#cidr_minions_c = "10.38.51.0/24"

public_ip = "true"

#vpc_cidr_prefix = "10.38"
# Dont update these unless you really know what you are doing
#cidr_mask = "28"
#leader_count = "'3'"

