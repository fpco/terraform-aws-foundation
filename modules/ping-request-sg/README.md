## Ping Request Security Group Rule

Create a simple and reusable security group rule that allows
hosts to ping hosts and receive responses from them

CIDR blocks provided are both hosts that can be queried by this SG
AND the only hosts that can send responses

It is obvious, but it should be noted that if you already have
open egress, you do not need this module. You are probably looking for
`ping-respond-sg` for hosts that can _receive_ ping requests and respond
instead.

