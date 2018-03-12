## AWS Route 53 Zone and Records for Consul Leaders

This module is for use with a cluster of Consul leaders, it provides a DNS
zone and records that point to the IPs where the consul leaders _might_ be.
While there may only be 3 to 9 Consul leaders, those leaders may move around
(they are an auto-scaling group), and we don't knoe _exactly_ where they are,
but if a consul agent uses the DNS record from this module, the agent will
continue to retry until it has found a leader. In practice, this works well,
and Consul agents will find their leaders in 5 - 90 seconds (depending on
the retry period and other settings configured in each agent).

