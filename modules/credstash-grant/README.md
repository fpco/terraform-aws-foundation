## Credstash Grant

This module will make it possible for anybody assuming the supplied IAM Role to read
and/or write secrets from/to credstash store.

The cleanup actions `when = "destroy"` in this module require at least
terraform 0.9.0.
See [this RFC](https://docs.google.com/document/d/15nEcV7fxskDgYrXoNMl6RYIo10PCiZGle7TP8xitrFE/)
and [this ticket](https://github.com/hashicorp/terraform/issues/386) for more
details on that.

