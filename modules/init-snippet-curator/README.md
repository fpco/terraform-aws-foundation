## Init Snippet: Curator

This module generates an init snippet that does the following for an
ubuntu or other apt-based system:

* `apt-get update`
* install `python-pip` with apt
* upgrade/set the pip version to 9.0.3 since 10 breaks things
* install `elasticsearch-curator` with `pip`
* add a user `curator`
* make the `/var/log/curator/` directory, owned by the `curator` user
* add a cronjob for this user, to run `/usr/local/bin/curator` at `30 1 * * *`
* write out the config files needed by the curator

