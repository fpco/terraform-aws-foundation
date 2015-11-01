# at the very least.. this fits with the modules in this repo, but
# feel free to customize to suit the specific needs of your project
base:
  '*':
    - packer
    - bootstrap
    - users

  'leaders-i-*':
    - consul_leaders

  'manage-i-*':
    - manage

  'master-i*':
    - master
