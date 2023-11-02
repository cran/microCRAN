# microCRAN

<!-- badges: start 
[![R build status](https://github.com/rstudio/plumber/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rstudio/plumber/actions)
[![](https://www.r-pkg.org/badges/version/plumber)](https://www.r-pkg.org/pkg/plumber)
[![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/plumber?color=brightgreen)](https://www.r-pkg.org/pkg/plumber)
[![codecov](https://app.codecov.io/gh/rstudio/plumber/branch/main/graph/badge.svg)](https://app.codecov.io/gh/rstudio/plumber)
[![RStudio community](https://img.shields.io/badge/community-plumber-blue?style=social&logo=rstudio&logoColor=75AADB)](https://community.rstudio.com/tag/plumber)
<!-- badges: end -->

Stand-alone HTTP capable CRAN repository, that fully supports R's `install.packages` and `available.packages`.
It also contains API endpoints for end-users to add/update packages.

This package can supplement [`miniCRAN`](https://cran.r-project.org/package=miniCRAN), which has functions for maintaining a local (partial) copy of CRAN. 
`microCRAN` adds the HTTP capability that allows us to use the local repository without having direct access to the local files.

Current version is bare-minimum without any user/access-control or much security.

## Getting started

```
library(microCRAN)

# starts a site at http://127.0.0.1:<port>/
microCRAN('/path/to/local/repository')

library(plumber)
# alternativly modify the router a bit before running it:
microCRAN('/path/to/local/repository', prefix = '/cran/, run = FALSE) |>
  pr_get('/hello', \(name) { paste('Hello', name) }, serializer = serializer_text()) |>
  pr_run(port = '1448')
# repository now available at http://127.0.01:1448/cran/

```

## Docker

The accompanying Dockerfile is intended for setting up a neat environment for *development*.

Following example builds the source package and checks it, assuming the git repo is cloned to `~/Rpackage/microcran`
```
docker build -t microcran-dev .
docker run -v ~/Rpackages:/root -w /root/microcran microcran-dev R -e "devtools::document()" -e "devtools::check(manual = TRUE, cran = TRUE)"
```
