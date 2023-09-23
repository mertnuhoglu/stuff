#!/usr/bin/env rr
# https://cran.r-project.org/web/packages/littler/vignettes/littler-examples.html

if (is.null(argv) | length(argv)<1) {
  cat("Usage: installr.r pkg1 [pkg2 pkg3 ...]\n")
  q()
}
print(argv)

## adjust as necessary, see help('download.packages')
repos <- "https://cran.rstudio.com" 

## this makes sense on Debian where no packages touch /usr/local
#lib.loc <- "/usr/local/lib/R/site-library"

#install.packages(argv, lib.loc, repos)
install.packages(argv, repos = repos)

