#!/usr/bin/env rr
# https://cran.r-project.org/web/packages/littler/vignettes/littler-examples.html
#
# A simple example to install one or more packages from GitHub
#
# Copyright (C) 2014 - 2015  Carl Boettiger and Dirk Eddelbuettel
#
# Released under GPL (>= 2)

## load docopt and remotes (or devtools) from CRAN
suppressMessages(library(docopt))       # we need docopt (>= 0.3) as on CRAN
suppressMessages(library(remotes))      # can use devtools as a fallback

## configuration for docopt
doc <- "Usage: installGithub.r [-h] [-d DEPS] REPOS...

-d --deps DEPS      Install suggested dependencies as well? [default: NA]
-h --help           show this help text

where REPOS... is one or more GitHub repositories.

Examples:
  installGithub.r RcppCore/RcppEigen                     

installGithub.r is part of littler which brings 'r' to the command-line.
See http://dirk.eddelbuettel.com/code/littler.html for more information.
"

## docopt parsing
opt <- docopt(doc)
if (opt$deps == "TRUE" || opt$deps == "FALSE") {
    opt$deps <- as.logical(opt$deps)
} else if (opt$deps == "NA") {
    opt$deps <- NA
}

Sys.unsetenv("GITHUB_PAT")
Sys.unsetenv("GITHUB_TOKEN")
invisible(sapply(opt$REPOS, function(r) install_github(r, dependencies = opt$deps)))

