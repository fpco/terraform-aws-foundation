## Init Snippet: Run Prometheus

Generate an init snippet to configure and run `prometheus` as a system
service using SaltStack:

* write out bootstrap pillar
* use `salt-call` to apply the `prometheus.server` formula from
  `fpco-salt-formula`

