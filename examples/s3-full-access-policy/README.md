# Example to test the s3-full-access-policy module.

This example provides an environment to demonstrate and test the [s3-full-access-policy](https://github.com/fpco/terraform-aws-foundation/tree/master/modules/s3-full-access-policy) module

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

### Testing

To run the tests execute the command:

```
make test
```

### Destruction

To destroy the test environment run the following command:

```
make clean
```

This will delete the objects and buckets that were created with `terraform destroy`.

## Description

### Abstract High Level Goals

- Implement tests that confirm correctness for IAM policies in the [s3-full-access-policy](https://github.com/fpco/terraform-aws-foundation/tree/master/modules/s3-full-access-policy) module.
  * Test the permissions that the modules sets up. This should cover the IAM policy based permissions that the module sets up and will demostrate that the appropriate IAM role/user has access to an S3 bucket.
  * Further, other roles/users should be shown not to have access/permission to that S3 bucket.

### Requirements

- Tests to demonstrate that correct user has access.
- Tests to demonstrate that all other users do **not** have access.
- Other tests for possible combinations from the module of include-lists and exclude-lists that IAM uses.
- After S3 buckets are created for testing they should be purged - this would require tests to show that buckets have been removed.
- The resulting resource costs from testing need to be tracked.
- CI based requirements:
  * Tests should be run when code in the master branch of the `terraform-aws-foundation` changes.
  * Failed tests should be reported.

### Methodology

1. The example will create and provision the S3 resources and assign/setup roles.
2. The included Haskell code will perform tests and API calls using these resources and roles as targets.

### Notes

This proposal is for issue [122](https://github.com/fpco/terraform-aws-foundation/issues/122).

Additionally it covers the topics raised in issue [47](https://github.com/fpco/terraform-aws-foundation/issues/47).
