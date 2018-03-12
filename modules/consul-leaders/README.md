## Consul Leaders (built on Auto-Scaling Groups)

**DEPRECATED**

This module reuses two other modules in this suite:

* `asg`
* `cluster-net`

The purpose of this module is to pair the ASG with subnets created specifically
for the ASG. Similarly, the individual parts (`asg` / `cluster-net`), exist to
allow flexibility, and to enable user-developers with the option of putting a
new ASG into an existing network, and upfront or further down the line.

While this module is similar to `consul-cluster`, there are a few notable
differences:

* The CIDR blocks are significantly smaller to limit the IP space, which
  simplifies bootstrapping the leaders and client agents.
* This module suggests different max/min node counts as defaults

