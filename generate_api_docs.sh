#!/bin/bash

set -xu

MODULE_LIST=$(ls tf-modules)
for m in $MODULE_LIST
  do
  terraform-docs md tf-modules/$m > docs/modules/$m.md
  done

# generate the module index page from our template
# and dynamic list of terraform modules
cat docs/modules/index.tpl > docs/modules/index.md
cat <<EOF >> docs/modules/index.md

$(for m in $MODULE_LIST; do
echo "* [$m](/modules/$m)"
done
)
EOF
