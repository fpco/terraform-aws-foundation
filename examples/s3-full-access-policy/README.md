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
make output
```

Or simply run the following:

```
make init && make plan && make apply && make output
```

### Testing

To run the tests, after resource creation, execute the command:

```
make test
```

This will build and run the test executable. The output is currently the HSpec reports and the output from the logger.
The tests poll for around 20 seconds until they find the target bucket. Expect the test overall to take around a minute to run after the executable build is complete.
Stack is a dependency of this step and must be installed in order to build and run the tests.

### Destruction

To destroy the test environment and all buckets and other resources created by the example run the following command:

```
make clean
```

This will delete the objects and buckets that were created with `terraform destroy`.

## Description

### Abstract high level goals for this example

- Implement tests that confirm correctness for IAM policies in the [s3-full-access-policy](https://github.com/fpco/terraform-aws-foundation/tree/master/modules/s3-full-access-policy) module.
  * Test the policy permissions that the module sets up and demonstrate that the appropriate IAM role/user has access to an S3 bucket.
  * Further, other roles/users should be shown not to have access/permission to that S3 bucket.

### Completed Requirements

- Tests to demonstrate that correct user has access.
- Tests to demonstrate that other users do **not** have access.

### Remaining Requirements

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

This example currently only creates a single bucket to test the terraform module on. The test executable currently only tests the first bucket it sees in the json output produced by terraform.

This proposal is for issue [122](https://github.com/fpco/terraform-aws-foundation/issues/122).

Additionally it covers the topics raised in issue [47](https://github.com/fpco/terraform-aws-foundation/issues/47).
