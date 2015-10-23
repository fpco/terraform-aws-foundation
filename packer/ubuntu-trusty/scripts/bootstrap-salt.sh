#!/bin/sh

# this will give us latest saltstack
add-apt-repository -y ppa:saltstack/salt
# ensure we get zmq v4
add-apt-repository -y ppa:chris-lea/zeromq
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

# all of /srv/* is root only, and not world readable
chown -R root:root /srv
chmod -R o-rwx /srv


ls -alh /srv/*
ls -alh /etc/salt/*
cat /etc/salt/minion
cat /etc/salt/minion.d/*
