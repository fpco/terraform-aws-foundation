#!/bin/sh

terraform taint -module=prometheus.prometheus-server aws_launch_configuration.cluster
terraform taint -module=prometheus.prometheus-server aws_autoscaling_group.cluster
