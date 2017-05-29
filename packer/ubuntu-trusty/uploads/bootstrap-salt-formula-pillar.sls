
# for the `salt.file_roots.git` formula
file_roots_git:
  # run the git checkout as this user
  user: root
  # SSH key for git checkouts
  ssh_key_path: /root/.ssh/id_rsa
  # file_roots source repos
  install:
    openssh-formula:
      url: https://github.com/saltstack-formulas/openssh-formula
      rev: 'master'
    fail2ban-formula:
      url: https://github.com/saltstack-formulas/fail2ban-formula
      rev: 'master'
    fpco-salt-formula:
      url: https://github.com/fpco/fpco-salt-formula
      rev: 'develop'
  # logentries-salt-formula:
  #   url: git@github.com:fpco/logentries-salt-formula.git
  #   rev: 'master'
  # hashicorp-salt-formula:
  #   url: git@github.com:fpco/hashicorp-salt-formula.git
  #   rev: 'develop'
  # update this list to choose the repos to enable, not listed == inactive
  #active:
  #  - salt-formula


# for the `salt.file_roots.deb` formula
#file_roots_deb:
#  install:
#    salt-formula:
#      url: http://ubuntu.example.com/salt-formula-v2015-09-15.deb
#      checksum: FFAADD
#  absent:
#    - old-formula-v2015-07-20
#  active:
#    - salt-formula


# for the `salt.file_roots.single` formula
#file_roots_single:
#  user: root
#  ssh_key_path: /root/.ssh/id_rsa
#  roots_root: /srv/salt
#  url: git@github.com:saltstack-formulas/salt-formula.git
#  rev: develop


