# Example to test the Kops workflow with an existing VPC on AWS

**Motivation:** To make it easier to deploy a `kubernetes` cluster into an existing infrastructure, as against having `kops` manage it. Especially, in cases where the infrastructure needs to be shared for other applications.

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
6. Files and secrets required to configure kubernetes clusters
7. Security groups

...and modifies the tags on existing subnets to make things easier for discovery within the kubernetes cluster

## Environment creation and deployment
`NOTES`:
  - For usage with `aws-env` have each command below execute in the form `aws-env -p <your-profile> -- <the-command>`.
  - The word `test` in the snippets below refers to your target environment. As an example, the `template/` environment is provided under the respective directories for reference. Users *should* copy the template to their own environment name and then modify it, rather than use it as it is. For example, by executing `cp -r template test` and the setting the `$ENVIRONMENT` in `test/env`.

### Part 1: Terraform

```
pushd terraform
source test/env
```

1. Create the VPC
    - 1 SSH Keypair to use throughout
    - 1 VPC, and
    - 3 subnets.

```
make generate-ssh-keypair
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

### Part 2: Copy relevant values over to the Kubernetes' side
```
popd
cp -r kubernetes/template kubernetes/$ENVIRONMENT
make copy-keys       # Copy generated .pem files
make copy-vars       # Copy terraform output to kube(kops) environment: `$ENVIRONMENT/env`
```

### Part 3: Kubernetes (Kops)

```
pushd kubernetes
```

0. Source the environment
```
source test/env
```

1. Create the cluster configuration
```
make kops-create-cluster
```
2. Replace the subnets in the cluster configuration with the ones in the environment, made with Terraform
```
make kops-replace-subnets
```
3. Confirm that the cluster configuration uses the existing VPC and its subnets.
```
make kops-edit-cluster
```
...here is a table of variable names between the `terraform` output and `kops` configuration that should match in the `kops` configuration. We only need to consider `cidr_blocks` and `subnet_ids`, since `kops-create-cluster` in the `make` file takes both `vpc_id` and `vpc_cidr_block` through the `env` file updated in part `2`.

|Terraform output|Kops configuration|
|----------------|------------------|
|`vpc_id`|`networkID`|
|`vpc_cidr_block`|`networkCIDR`|
|`cidr_blocks`|`subnets[.cidr]`|
|`subnet_ids`|...to be added to the kops configuration under each `subnet` as key `id`|

For example, if we have a `terraform` output like:
```
vpc_id = vpc-blah
vpc_cidr = 10.10.0.0/16
subnet_ids = [
  subnet-blah1,
  subnet-blah2,
  subnet-blah3
]
subnet_azs = [
  us-east-2a,
  us-east-2b,
  us-east-2c
]
subnet_cidrs = [
 10.10.1.0/24,
 10.10.2.0/24,
 10.10.3.0/24
]
```
...then, the final `kops` configuration should look like:
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
3. Update the `kops` cluster configuration with the replaced version
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
```
2. Destroy the `terraform`-created VPC, subnets and NAT gateway
```
popd; pushd terraform
make plan-destroy
make apply
```
3. Delete stale files using the main Makefile
```
popd; make clean
```
-----

Have fun!


## Notes

* `terraform`: `v0.11.8`
* `aws provider`: `v1.6.0`
* `kops`: `v1.10.0 (git-8b52ea6d1)`
* `aws`: `aws-cli/1.15.69 Python/3.6.5 Linux/4.15.0-33-generic botocore/1.10.68`
* `kubectl`:
```
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
