# for the `salt.file_roots.deb` formula
file_roots_deb:
  install:
    PKG_NAME:
      url: PKG_URL
      checksum: PKG_CHECKSUM
  active:
    - PKG_NAME

