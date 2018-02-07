# Install curator (will only run on the active master)

apt-get update
apt-get install python-pip
pip install elasticsearch-curator
useradd -r curator
mkdir /var/log/curator/
chown curator:curator /var/log/curator
mkdir /etc/curator
TMP_CRON=$(mktemp -t "curator-cron-job-XXXXXX.txt")
echo '30 1 * * * /usr/local/bin/curator --config /etc/curator/config.yaml /etc/curator/actionfile.yaml' > $TMP_CRON
crontab -u curator $TMP_CRON

cat <<EOF > /etc/curator/config.yaml
client:
  hosts:
    - ${elasticsearch_host}
  port: ${elasticsearch_port}
  url_prefix:
  use_ssl: False
  certificate:
  client_cert:
  client_key:
  ssl_no_validate: False
  http_auth:
  timeout: 30
  master_only: ${master_only}

logging:
  loglevel: INFO
  logfile: /var/log/curator/curator.log
  logformat: default
  blacklist: ['elasticsearch', 'urllib3']
EOF

cat <<EOF > /etc/curator/actionfile.yaml
actions:
  1:
    action: delete_indices
    description: >-
      Delete indices older than ${index_retention_period} days (based on index name), for filebeat-
      prefixed indices. Ignore the error if the filter does not result in an
      actionable list of indices (ignore_empty_list) and exit cleanly.
    options:
      ignore_empty_list: True
      timeout_override:
      continue_if_exception: False
      disable_action: ${index_retention_period == 0 ? "True" : "False"}
    filters:
    - filtertype: pattern
      kind: prefix
      value: '^([a-z]+-)+'
      exclude:
    - filtertype: age
      source: name
      direction: older
      timestring: '%Y.%m.%d'
      unit: days
      unit_count: ${index_retention_period}
      exclude:
${extra_curator_actions}
EOF

sudo mkdir -p /etc/filebeat/prospectors/

cat <<EOF > /etc/filebeat/prospectors/curator.yaml
filebeat.prospectors:
- input_type: log
  paths:
    - '/var/log/curator/*.log'
  fields_under_root: true
  fields:
    index_prefix: elk
    log_info:
      origin: aws
      source: curator
      formats:
        - curator
      transport: filebeat
EOF
