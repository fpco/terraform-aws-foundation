# the default setup includes this pillar definition via top.sls
# add customizations here, or others, but update top.sls when needed

# configures the openssh formula
sshd_config:
  PermitRootLogin: 'no'
  ServerKeyBits: '2048'
  X11Forwarding: 'no'
  PasswordAuthentication: 'no'
  AuthenticationMethods: 'publickey'
