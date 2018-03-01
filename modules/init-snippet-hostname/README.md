## Init Snippet: Hostname Management (SaltStack version)

Generage an init snippet that does the following:

* lookup the instance id and derive the new hostname
* write the new hostname to `/etc/salt/minion_id`
* write the new hostname to bootstrap pillar
* use `salt-call` to apply the `hostname` formula from `fpco-salt-formula`.
  This formula will set the hostname and refresh salt so it's minion ID is
  correct in the cache and elsewhere.

The hostname is derived from the instance ID and the `hostname_prefix`
variable, and will look like: `web-server-i-101abd22`.

