## Kube-Stack (w/ Terraform and kubespray)

DOCUMENT.

### Assumptions

Here is a list of assumptions this module makes:

* the AMIs used are based on CoreOS
* the caller provides the list of security group ids, and subnets


### Example - how to use this module

```
    module "etcd-server-sg" {
      source      = "../etcd-server-sg"
      name_prefix = "${var.name}"
      vpc_id      = "${module.vpc.vpc_id}"
      cidr_blocks = ["${var.vpc_cidr}"]
    }
    
    module "kube-controller-sg" {
      source      = "../single-port-tcp-sg"
      port        = "6443"
      name_prefix = "${var.name}"
      name_suffix = "kube-controller"
      vpc_id      = "${module.vpc.vpc_id}"
      cidr_blocks = ["${var.vpc_cidr}"]
    }


     "${module.private-ssh-sg.id}",
     "${module.open-egress-sg.id}",
     "${module.etcd-server-sg.id}",
     "${module.kube-controller-sg.id}",
```

