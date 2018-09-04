# Example to test the Kops workflow with an existing VPC on AWS

## Summary

We first use `terraform` to create:
1. a VPC,
2. it's subnets and
3. an internet gateway, along with route tables for local and internet traffic.

We then use `kops` to create the kubernetes cluster in this existing VPC, where `kops`, by itself, manages the creation of:
1. DNS records under the base FQDN,
2. The auto-scaling group, and its launch configurations
3. EBS volumes
4. IAM instance profiles, role policies
5. Keypairs for every kubernetes component
6. Files and secrets required to configu kubernetes clusters
7. Security groups
...and modifies the tags on existing subnets to make things easier for discovery within the kubernetes cluster

## Environment creation and deployment
`NOTE`: For usage with `aws-env` have each command below execute in the form `aws-env -p <your-profile> -- <the-command>`.

### Part 1: Terraform

```
pushd terraform
source prod/env
```

1. Create the VPC
    - 1 SSH Keypair to use throughout
    - 1 VPC, and
    - 3 subnets.

```
make generate-ssh-keypair
make import-ssh-keypair
make init
make init-plan
make apply
```

2. Create the NAT Gateway
    - the Internet Gateway, and
    - its route tables, for traffic between the 3 subnets and the public internet.

```
make plan
make apply
```

### Part 2: Kubernetes (Kops)

```
popd; pushd kubernetes
source prod/env
cp ../terraform/*.pem* prod/    # copy the SSH Keypair
make create-s3-bucket           # create the S3 bucket for `kops` state
```

1. Create the cluster configuration
```
make kops-create-cluster
```
2. Edit the cluster configuration to help `kops` use the existing VPC and its subnets.
```
make kops-edit-cluster
```
...here is a table of variable names between the `terraform` output and `kops` configuration that should match after editing the `kops` configuration.

| Terraform output   | Kops configuration |
-------------------------------------------
| `vpc_id`           | `networkID`        |
| `vpc_cidr_block`   | `networkCIDR`      |
| `cidr_blocks`      | `subnets`[.cidr]   |
| `subnet_ids`       | ...to be added to the kops configuration under each `subnet` as key `id` |

For example, if we have a `terraform` output like:
```
azs = [
  us-east-2a,
  us-east-2b,
  us-east-2c
]
vpc_id = vpc-blah
vpc_cidr_block = 10.10.0.0/16
cidr_blocks = [
 10.10.1.0/24,
 10.10.2.0/24,
 10.10.3.0/24
]
subnet_ids = [
  subnet-blah1,
  subnet-blah2,
  subnet-blah3
]
```
...then, the final `kops` configuration should be edited to look like:
```
networkCIDR: 10.10.0.0/16
networkID: vpc-blah
subnets:
- cidr: 10.10.1.0/24
  id: subnet-blah1
  name: us-east-2a
  type: Public
  zone: us-east-2a
- cidr: 10.10.2.0/24
  id: subnet-blah2
  name: us-east-2b
  type: Public
  zone: us-east-2b
- cidr: 10.10.3.0/24
  id: subnet-blah3
  name: us-east-2c
  type: Public
  zone: us-east-2c
```

3. Update the `kops` cluster configuration with the edited version
```
make kops-update-cluster
```
4. If everything looks fine, instantiate the cluster
```
make OPTS=--yes kops-update-cluster
```

##### `NOTE`: We now have the kubernetes cluster running on a pre-configured VPC we made using `terraform`. Congratulations!

## Testing

It may take a few minutes before all the components of the cluster are up. Note that DNS entries may take a few minutes to propagate. We can validate the cluster by:

```
make kops-validate-cluster
```
and, if one wants to use `kubectl`, then:
```
make fetch-kubeconfig
KUBECONFIG=~/.kube/$ENVIRONMENT.kops-vpc.yaml kubectl get nodes --show-labels
```

## Destruction

1. Delete the `kops` cluster and its state bucket
```
make OPTS=--yes kops-delete-cluster
make delete-s3-bucket
```
2. Destroy the `terraform`-created VPC, subnets and NAT gateway
```
popd; pushd terraform
make plan-destroy
make apply
```
3. Delete the previously imported SSH Keypair (if necessary)
```
make delete-ssh-keypair
```
4. Delete stale files in the project directory
```
make clean
```

## Notes

* `terraform`: `v0.11.8`
* `aws provider`: `v1.6.0`
* `kops`: `v1.10.0 (git-8b52ea6d1)`
* `aws`: `aws-cli/1.15.69 Python/3.6.5 Linux/4.15.0-33-generic botocore/1.10.68`
* `kubectl`: ```
Client Version: version.Info{
  Major:"1",
  Minor:"11",
  GitVersion:"v1.11.1",
  GitCommit:"b1b29978270dc22fecc592ac55d903350454310a",
  GitTreeState:"clean",
  BuildDate:"2018-07-17T18:53:20Z",
  GoVersion:"go1.10.3",
  Compiler:"gc",
  Platform:"linux/amd64"
}
```
