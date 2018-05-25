# Example to test the packer-vpc module.

An example to demonstrate the [packer-vpc](https://github.com/fpco/terraform-aws-foundation/tree/master/modules/packer-vpc) module.
This will use the `packer-vpc` module to create a simple, isolated VPC on AWS which can be dedicated to building AMI's with `packer`.


## Using the Example

### Environment creation and deployment

To use this example set up AWS credentials and then run the commands in the 
following order:

```
make init
make plan
make apply
```

Or simply run the following:

```
make init && make plan && make apply
```

### Destruction

To destroy the test environment and other resources created by the example run the following command:

```
make clean
```

