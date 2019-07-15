# Example to test the VPC Scenario 3 Module
## Environment creation and deployment

This module creates a VPC with both public and private-vpn subnets spanning one or
more availability zones, a hardware VPN gateway for the private subnets.

This example the operator have in place the following networking components:

- A Public subnet spanning one or more availability zones
- A Private subnet spanning one or more availability zones that route the tunnel to connect with a customer VPN
- A virtual private gateway to enable communication with your own network over an IPsec VPN tunnel.
- Instances in the private subnet with IPv4 addresses which enables the instances to communicate with each other and other instances in the VPC.
- The main route table associated with the private (VPN-only subnet). The route table contains an entry that enables instances in the subnet to communicate with other instances in the VPC, and an entry that enables instances in the subnet to communicate directly with your network.

The VPN connection consists of a virtual private gateway located on the Amazon side of the VPN connection and a customer gateway located on your side of the VPN connection.

<p align="center">
  <img width="460" height="300" src="https://github.com/fpco/terraform-aws-foundation/blob/vpc-scenario-3/examples/vpc-scenario-3/VPC-3.png">
</p>

To use this example set up AWS credentials and then run the commands in the
following order:

Note that, when using this module and deploying the VPC for the first time, Terraform needs the user to add the VPC, Subnets, and then Route Tables. For example, use targets to apply these updates sequentially:

```
make ssh-key
make ssh-key-remote-vpc
make init-remote-vpc
```

Replace `vpn_remote_ip` in the variables.tf file for openvpn-prublic-eip output vaule, this will setup the virtual gateway connection.
Once the init-remote-vpc finished, login to openvpn web server using the default user and password `openvpn` the URL should be similar to this sample HTTPS://<openvpn-prublic-eip>:943/admin and follow the next steps to allow connections using the public ip:

- Click in *Nework Setting* and replace the private IP by the public
- Click in *VPN Settings* and add vpc_cidr_block (10.24.0.0/16); turn off  *Internet traffic routed through the VPN*

Setup the scenario 3
```
make init
make subnets
make plan
make apply
make setup-ipsec
```

## Testing

Downloand openvpn client from the server https://<openvpn-prublic-eip>:943/ or from openvpn.net, use the deafult user/password and the openvpn-prublic-eip to connect.

SSH into the machine with the command:

```
ssh -i id_rsa ubuntu@<vpc-ip-address>
```

## Destruction

To destroy the test environment run the following commands:

```
make destroy
make clean
```

## Notes

* This example was last tested with `Terraform v0.11.7`
* This example assumes AWS credentials setup with access to the **eu-central-1** region.
* This example when deploying the VPC for the first time, Terraform needs the user to add the VPC, Subnets, and then Route Tables.



