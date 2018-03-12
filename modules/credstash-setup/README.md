## Credstash Setup

Setup KMS Master Key and a DynamoDB Table for use with Credstash.

By default, this module doesn't need any variables to be set manually, but can
be overridden if necessary. By doing so it is possible to create either key or
database table or both, as well as other customizations.

**Resources created here cannot be deleted using terraform and have to be deleted
manually. This behavior is to prevent possibility of sensitive data loss.**
