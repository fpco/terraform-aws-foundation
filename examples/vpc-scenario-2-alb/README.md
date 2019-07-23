# Example to test the VPC Scenario 2 Module with an Application Load Balancer

## Environment creation and deployment

To use this example set up AWS credentials and then run the commands in the 
following order:

```
make ssh-key
make init
make plan-subnets
make apply
make plan-gateways
make apply
make plan
make apply
make poll
```

Or, alternatively, use `make all`.

## Testing

Use `make poll` to hit the ELB until we see an HTTP 200 (or until the 5 minute timeout).

## Bastion

If your SSH key is the default from the example (`./id_rsa`), write a `ssh_config` like:

```
Host bastion
    HostName      ${BASTION_IP_TO_UPDATE}
    User          ubuntu
    IdentityFile  ./id_rsa


Host 10.23.*
    User            ubuntu
    IdentityFile    ./id_rsa
    ProxyCommand    ssh -F ssh_config bastion -W %h:%p
    StrictHostKeyChecking ask
```

Then get the private IP addresses of the web servers in the private subnets with
`make lookup-ips`.

Then `ssh -F ssh_config $IP`.

## Destruction

To destroy the test environment run the following commands:

```
make destroy
make clean
```

## Notes

* This example was last tested with `Terraform v0.11.13`
* This example assumes AWS credentials setup with access to the **us-east-2** region.
