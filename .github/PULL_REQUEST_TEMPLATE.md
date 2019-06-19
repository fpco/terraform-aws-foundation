---
name: Pull request template
about: Make a PR to terraform-aws-foundation
---

Please include the following in your PR:

Please also note that these are not hard requirements, but merely serve to define
what maintainers are looking for in PR's.  Including these will more likely lead 
to your PR being reviewed and accepted.

- [ ] Update the changelog
- [ ] Make sure that modules and files are documented. This can be done inside the module and files.
- [ ] Make sure that new modules directories contain a basic README.md file.
- [ ] Make sure that the module is added to [tests/main.tf](https://github.com/fpco/terraform-aws-foundation/blob/master/tests/main.tf)
- [ ] Make sure that the linting passes on CI.
- [ ] Make sure that there is an up to date example for your code:
      - For new `modules` this would entail example code for how to use the module or some explanation in the module readme.
      - For new examples please provide a README explaining how to run the example. It's also ideal to provide a basic makefile to use the example as well.
- [ ] Make sure that there is a manual CI trigger that can test the deployment.
