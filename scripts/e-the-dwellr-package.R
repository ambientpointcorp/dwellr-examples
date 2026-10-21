# Generated from chapters/e-the-dwellr-package.qmd by tools/extract.py -- do not edit by hand.
# Chapter: The `dwellr` package
#
# Run from the repository root:
#   Rscript scripts/e-the-dwellr-package.R

library(dwellr)
library(ggplot2)

source("R/theme.R")


## ---- families ----------------------------------------------------
exports <- sort(getNamespaceExports("dwellr"))
family  <- ifelse(grepl("_", exports), sub("_.*", "", exports), "(no prefix)")
tab <- sort(table(family), decreasing = TRUE)

c(exported_objects = length(exports), families = length(tab))
head(tab, 12)

## ---- dataset -----------------------------------------------------
data(orders, package = "dwellr")
c(orders = nrow(orders$orders), events = nrow(orders$events),
  columns = ncol(orders$orders))

## ---- deps --------------------------------------------------------
desc <- read.dcf(system.file("DESCRIPTION", package = "dwellr"))
imports <- trimws(strsplit(desc[1, "Imports"], ",")[[1]])
suggests <- trimws(strsplit(desc[1, "Suggests"], ",")[[1]])

list(imports = imports, suggests = suggests)

## ---- tests -------------------------------------------------------
c(test_files = length(list.files(
    system.file("..", "dwellr", package = "dwellr"), pattern = "^test-",
    recursive = TRUE)),
  exported_functions = sum(vapply(getNamespaceExports("dwellr"),
    function(n) is.function(get(n, asNamespace("dwellr"))), TRUE)))
