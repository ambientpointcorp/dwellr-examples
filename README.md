# dwellr-examples

Worked examples of [`dwellr`](https://github.com/ambientpointcorp/dwellr), one
script per chapter of *Lifecycle Monitoring Analytics: A toolkit for continuous
supply chain operations*, which is the book that works through the package's
methods.

Install the package first. Everything here calls it and nothing here duplicates
it.

```r
# install.packages("remotes")
remotes::install_github("ambientpointcorp/dwellr")
```

## Layout

```
scripts/   one file per chapter and per appendix, generated from the manuscript
R/         the plot theme and figure helper every script shares
tools/     the extractor that generates scripts/ from the book
out/       figures, regenerated rather than tracked
data/      the one cached artifact the scripts share, if you choose to cache it
```

## Running a chapter

Run from the repository root, so that `source("R/theme.R")` resolves.

```bash
Rscript scripts/14-setting-thresholds.R
```

Each script is self-contained: it loads the packages that chapter needs, draws
the corpus, and prints the figures and tables the chapter prints. No script
sources another, so a reader interested in one chapter runs one file.

`make list` names the scripts. `make run` runs all of them and reports which
fail, which takes roughly half an hour: Chapter 9's model fits and Chapter 20's
echelon searches dominate the total.

## The scripts are generated, not written

`tools/extract.py` reads the manuscript's chapter sources and writes one script
per chapter from the R chunks inside them.

```bash
make extract BOOK=../lifecycle-analytics-book
```

This is deliberate. A hand-written example drifts from the chapter it
illustrates and nobody notices until a number disagrees with the page. A
generated one cannot drift, because the chapter is its only source. Do not edit
anything under `scripts/` directly — the next extraction will overwrite it.

The manuscript repository is private until publication, so `make extract`
works only for someone with a checkout of it. The generated scripts are
committed here for that reason.

## The corpus

Every script draws its data from `simulate_lifecycles()` in `dwellr`, at
`n = 5000, seed = 11` for most chapters. The generator is deterministic given
its seed, so a figure here reproduces the figure in print exactly.

The corpus deliberately carries the pathologies the book has to measure:
right-censoring from an observation time inside the horizon, two-component
dwell distributions, partial fills, loops, open-ticket residue, and covariate
effects on dwell. Appendix F of the book states what it does *not* carry —
clean identifiers, no seasonality, no non-stationarity, and two branches that
behave alike — and each of those is a reason to re-measure on your own data
rather than carry a figure across.

## Your own extract

The estimators depend on the shape of the tables rather than on the generator.
Two data frames are enough for all of Part II: an `orders` frame with an
identifier, an entry timestamp, the current stage, a terminal flag and any
cohort keys; and an `events` frame with `order_no`, `event`, `stage` and
`occurred_at`. Supply `as_of` as the extract time and `stage_dwell()` will set
the censoring flag correctly.

Appendix F of the book works a two-order example end to end.

## License

MIT. See [LICENSE](LICENSE). The book itself is copyrighted separately; the
code is a tool and is meant to be used.
