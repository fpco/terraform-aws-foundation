# The FP Complete AWS Foundation

A modular, composable, extensible, framework for building secure, highly scalable
platforms on AWS. The framework leverages established best practices, modern
tools and paradigms, supports the ops developer who needs to move fast while
maintaining stability, and does not forget how to support your legacy apps.


## Project Goals

This project aims to be..

* an extensible framework for the ops developer under (multiple dimensions
  of) pressure
* a highly-scalable and flexible implementation of modern distributed systems
  architecture and paradigms
* focused on security, maintainability, repeatability, reliability, and simple
  but powerful workflows
* a way for ops to deal with the ever-evolving nature of the ops-landscape,
  while also continuing to support the legacy apps of yesteryear
* an example, a reference stack for the ops developer to use in building their
  own platform


## Tools for Relevant Problems

In pursuit of those goals, the reusable modules documented here form a platform
by leveraging the following tools:

* [Terraform](https://terraform.io) - Orchestrates resources in the cloud,
  declarative expression at the core of ops (network, node, cluster, service)
* [Packer](https://packer.io) - Creates pre-baked VM images for nodes in our
  network (with the tools we need)
* [Amazon Auto-Scaling Groups](http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/WhatIsAutoScaling.html)
- Robust platforms must be self-healing.
* [Docker](https://docker.com) - Many services/applications run on the platform
  will do so as docker containers (but not all, legacy apps are easily supported).
* [Consul](https://consul.io),
  [consul-template](https://github.com/hashicorp/consul-template),
  [consulkv](https://github.com/spiritloose/consulkv) - For service discovery,
  distributed key/value store, and simplifed automation and orchestration in the
  distributed system.
* [Vault](https://www.vaultproject.io/) - Managing secrets, short-lived
  credentials, and TLS certificates, and auditing access to them all.
* [Kubernetes](http://kubernetes.io/) - Scheduling and resource management for
  containers.
* [Nomad](http://nomadproject.io/) - Task scheduling for containers, linux
  executables, and java applications.
* [Saltstack](https://saltstack.com),
  [saltstack-formulas](https://github.com/saltstack-formulas/),
  [fpco-salt-formula](https://github.com/fpco/fpco-salt-formula),
  [bootstrap-salt-formula](https://github.com/fpco/bootstrap-salt-formula) - Simple
  and sane configuration management for nodes at runtime, fault-tolerant
  (masterless) highly-expressive, and highly-scalable, with optional remote
  execution to boot.

As the collection of modules is composable into an endless array of possibilities,
there is no explicit requirement to use all of these tools, you can just as easily
use the IAM management modules and nothing else, or build a more complete stack
with these modules.
