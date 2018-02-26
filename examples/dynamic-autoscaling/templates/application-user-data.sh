#!/bin/bash

set -eu

${setup_repos}

apt-get install --yes sysbench

cat <<EOF > /
${sysbench_unit_file}
EOF

sleep 5m

systemctl enable sysbench
systemctl start  sysbench
