# Kube IAM roles

When manually creating a kube cluster with terraform and not using `kops` we
have to make sure to assign the correct IAM roles to the kube nodes (and master)
so that the cluster can manage things like ELBs etc.

Also see: kube-node-iam

