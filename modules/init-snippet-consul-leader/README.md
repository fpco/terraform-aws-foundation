## Init Snippet: Consul Leader

Configure and run the consul service as a leader, using the `consul.service`
Salt formula from `fpco-salt-formula`.

This will write out bootstrap pillar before applying the formula. This is
based on the DNS method for discovering the leaders, and should be updated
to leverage the AWS API for that discovery.

