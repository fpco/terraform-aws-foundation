#!/bin/sh

# see http://repo.saltstack.com/#ubuntu for more info
# import the SaltStack repository key
wget -O - https://repo.saltstack.com/apt/ubuntu/14.04/amd64/2016.3/SALTSTACK-GPG-KEY.pub | apt-key add -
# point apt at the official saltstack repo
echo "deb http://repo.saltstack.com/apt/ubuntu/14.04/amd64/2015.5 trusty main" > /etc/apt/sources.list.d/saltstack.list
# ensure PPAs are active and install the salt minion!
apt-get update
apt-get install -y salt-minion
# disable the service until configured
service salt-minion stop

# the bootstrap formula might need git installed..
apt-get install -y git
# update SSH known hosts for strict host key verification and success with git.latest
salt-call --local                              \
          state.single ssh_known_hosts.present \
          name=github.com enc=ssh-rsa          \
          fingerprint=16:27:ac:a5:76:28:2d:36:63:1b:56:4d:eb:df:a6:48

# clone our salt formula to bootstrap salt formula
salt-call --local                                             \
          state.single git.latest                             \
          rev=master                                          \
          name=git@github.com:fpco/bootstrap-salt-formula.git \
          target=/srv/bootstrap-salt-formula

# overwrite the empty bootstrap pillar with the user's
mv /tmp/bootstrap-salt-formula-pillar.sls /srv/bootstrap-salt-formula/pillar/bootstrap.sls

# bootstrap salt formula!
salt-call --local                                           \
          --file-root   /srv/bootstrap-salt-formula/formula \
          --pillar-root /srv/bootstrap-salt-formula/pillar  \
          --config-dir  /srv/bootstrap-salt-formula/conf    \
          state.highstate

# add a helper to make this easier for the admin, later..
cat <<END_ALIAS > /etc/profile.d/salt-file-roots.sh
alias bootstrap-salt-formula="salt-call --local --file-root /srv/bootstrap-salt-formula/formula --pillar-root /srv/bootstrap-salt-formula/pillar --config-dir /srv/bootstrap-salt-formula/conf state.highstate"
END_ALIAS

# setup pillar for running state.highstate
# the user gave us pillar .sls as uploads, move them into place for salt
mv /tmp/pillar /srv/

# the following is a hack to a) allow us to continue running 2015.5 for it's ext_pillar module
# and b) get the pip state/module from 2015.8, which doesn't break pip
echo "extension_modules: /srv/salt-ext" > /etc/salt/minion.d/extension_modules.conf
wget --output-document=/usr/lib/python2.7/dist-packages/salt/utils/locales.py https://raw.githubusercontent.com/saltstack/salt/2015.8/salt/utils/locales.py
# there are files in _states and _modules we want to sync and make available,
# that is where we have our pip state/module source we've imported from upstream
mkdir -p /srv/salt-ext/states /srv/salt-ext/modules
echo "sync salt modules/states/etc from the salt file roots to the minion cache"
salt-call --local saltutil.sync_all --log-level=debug
# run a second time, just in case the first did nothing..
salt-call --local saltutil.sync_all
echo "formula:"
ls -alh /srv/salt-deb/*/_*
ls -alh /srv/salt-git/*/_*

# all of /srv/* is root only, and not world readable
chown -R root:root /srv
chmod -R o-rwx /srv


ls -alh /srv/*
ls -alh /etc/salt/*
cat /etc/salt/minion
cat /etc/salt/minion.d/*
