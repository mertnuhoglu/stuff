#!/usr/bin/env Rscript --slave

initial.options <- commandArgs(trailingOnly = FALSE)
file.arg.name <- "--file="
script.name <- sub(file.arg.name, "", initial.options[grep(file.arg.name, initial.options)])
script.dirname <- dirname(script.name)
source(file.path(script.dirname, "source_scripts.R"), chdir = T)
source(file.path(script.dirname, "tsvsed_funs.R"), chdir = T)

