## BIND (DNS) Server Security Group Rule

Defines a `aws_security_group_rule` TCP/UDP on port 53, as used by BIND or
other DNS servers.

The `description` for each rule is auto-generated using the variables
defined:

```
description = "${var.description} ${var.dns_port} (TCP)"
```
