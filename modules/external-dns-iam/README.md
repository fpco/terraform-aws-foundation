# Kube IAM roles

This module creates an `aws_iam_role` that gets assigned to the
external-dns pod so it's able to manipulate route53 entries.

Usage:

    module "dnscontroller" {
      source                 = "fpco/foundation/aws//modules/external-dns-iam"
      version                = "0.7.0-rc3"
      name_prefix            = "example"
      kube_cluster_nodes_arn = "arn:aws:iam::ARN_FOR_KUBE_NODES"
    }

This is to be used in combo with [kube2iam](https://github.com/jtblin/kube2iam) for automatic assignment
of aws iam roles to pods. Specifically, the pod that will need it is the [external-dns](https://github.com/kubernetes/charts/tree/master/stable/external-dns) pod.
See our [foundation helm chart](https://github.com/fpco/helm-charts/tree/master/foundation) for an example
on how to deploy `kube2iam`.

