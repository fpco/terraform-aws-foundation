---
title: Test Infrastructure Design
---


This document describes the design of our testing efforts of this repository.


## Threat model

When writing code for this repository, what are the things that could go/be wrong?

1. Syntactic problems with the code.
2. Semantic problems with the code.
3. Problems with the deployments.

(If any new types of problems show up, be sure to add them to the thread model so that we can design
to mitigate them.)

We allow for pull-requests to this repository, which means that if CI ever gets set up to run anything
in the repository, it needs to be able to deal with untrusted input.

## Mitigations

### Syntactic problems

Syntactic problems are light to test.

- For terraform code, we can use linters to make sure the code is syntactically valid.
- For packer templates, we can use a JSON parser for the template.
- For docker files, we can use docker build to check that the images build correctl.

### Semantic problems

This will require some way to make assertions on the result.
We can get back to this later.

### Problems with the deployments

To make sure that deployments happen without issues, we should make sure that we completely understand 
how to run them.
This will require two things:

- A playbook for every deployment:
  A fool-proof guide to making the deployment happen.
  This guide must not require improvisation or smartness.
- A way to manually trigger an automated test deployment on CI.
