## Persistent EBS Volumes

Create an arbitrary number of EBS volumes. By "persistent" we mean that these
volumes are separate from the EC2 instances they are attached to, and can be
attached to a new version of the previous instance when we need to replace the
instance (and we want to keep the service data).

This module provides EBS volumes and associated IAM policies to be
used with an EC2 instances or auto-scaling groups. The `volume-mount-snippets`
module can be used to attach EBS volumes on boot. Volumes created will be
interleaved throughout the Avaialability Zones.

