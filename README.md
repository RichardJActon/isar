# ISAR

<!-- badges: start -->
[![R-CMD-check](https://github.com/RichardJActon/isar/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/RichardJActon/isar/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/RichardJActon/isar/graph/badge.svg)](https://app.codecov.io/gh/RichardJActon/isar)
[![Lifecycle:experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: AGPL v3](https://img.shields.io/badge/License-AGPL_v3-blue.svg)](https://www.gnu.org/licenses/agpl-3.0)
<!-- badges: end-->

**WARNING pre-alpha software!**

This is a partial reimplementation of [`isa-api`](https://github.com/ISA-tools/isa-api) in R

This implementation may diverge slightly from the [ISA spec](https://isa-specs.readthedocs.io/en/latest/) whilst it is still under development for our use case.

issue tracking and development take place on the [project repository](https://renkulab.io/projects/hdbi/data-management/isar) on the renkulab.io gitlab instance the github repository is just a mirror and build platform to make use of the existing and well maintained github actions for R packages for testing and distribution.

# install instructions

```
renv::install("RichardJActon/isar")
```

# Development

Launch a pre-configured RStudio development environment in Renku

[![launch - renku](https://renkulab.io/renku-badge.svg)](https://renkulab.io/projects/hdbi/data-management/isar/sessions/new?autostart=1&branch=main)

alternatively clone the repository:

```
git clone https://gitlab.renkulab.io/hdbi/data-management/isar.git
```

## Structure of test coverage

[codecov page](https://app.codecov.io/gh/RichardJActon/isar)

![](https://codecov.io/gh/RichardJActon/isar/graphs/tree.svg?token=ABrfqCl8r5)


