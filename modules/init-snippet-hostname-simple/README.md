## Init Snippet: Hostname (simple shell version)

Generate an init snippet that uses `hostnamectl` to update the hostname.
The hostname is also updated in `/etc/hosts`. The hostname is derived from
the instance ID and the `hostname_prefix` variable, and will look like:
`web-server-i-101abd22`

