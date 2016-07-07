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
          --log-level=debug                                 \
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

echo "minion formula:"
ls -alh /srv/*/* /srv/*/*/_*
echo "minion configs:"
ls -alh /etc/salt/*
cat /etc/salt/minion.d/*
echo "version check:"
salt-call --version

# the following is a hack to a) allow us to continue running 2015.5 for it's
# ext_pillar module, and b) get the pip state/module from 2015.8, which doesn't
# break pip
wget --output-document=/usr/lib/python2.7/dist-packages/salt/utils/locales.py https://raw.githubusercontent.com/saltstack/salt/2015.8/salt/utils/locales.py
# I really, really don't understand why this sleep works, but without it, the
# sync_all in this production build fails to sync anything. If you are interested
# in the details, see https://github.com/saltstack/salt/issues/34080
echo "pause to give time before we run sync_all"
sleep 60
# only needed for the moment, while fpco-salt-formula puts consul ext_pillar
# into this directory "manually", once that module is imported into the repo,
# this snippet/hack could be removed. Here's the catch, that module cannot be
# imported until we use a version of salt (or a version of the saltutils.sync_all
# module) which supports sync'ing pillar modules on masterless hosts. See this
# for context: https://github.com/saltstack/salt/issues/33645
mkdir /srv/salt-ext
cat <<EOF > /etc/salt/minion.d/ext_mods.conf
extension_modules: /srv/salt-ext
EOF
# there are files in _states and _modules we want to sync and make available,
# that is where we have our pip state/module source we've imported from upstream
echo "sync salt modules/states/etc from the salt file roots to the minion cache"
salt-call --local saltutil.sync_all
ls -alh /srv/salt-ext/*
