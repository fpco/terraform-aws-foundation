# Prometheus Cloudwatch exporter IAM role

This module creates an `aws_iam_role` that get's assigned to the
prometheus-cloudwatch pod so it's able to scrape cloudwatch
metrics. For example: RDS Cpu Usage.

Usage:

    module "cloudwatch-exporter" {
      source                 = "fpco/foundation/aws//modules/cloudwatch-exporter-iam"
      version                = "0.7.0-rc3"
      name_prefix            = "example"
      kube_cluster_nodes_arn = "arn:aws:iam::ARN_FOR_KUBE_NODES"
    }

This is to be used in combo with [kube2iam](https://github.com/jtblin/kube2iam) for automatic assignment
of aws iam roles to pods. Specifically, the pod that will need it is the [prometheus-cloudwatch](https://github.com/fpco/helm-charts/tree/master/prometheus-cloudwatch) pod.
See our [foundation helm chart](https://github.com/fpco/helm-charts/tree/master/foundation) for an example
on how to deploy `kube2iam` and `prometheus-cloudwatch`.

