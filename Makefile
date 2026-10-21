# Regenerate the scripts from the manuscript, and run them.
#
# The scripts are generated rather than written, so a chapter's analysis and
# the script that reproduces it cannot drift apart.

BOOK ?= ../lifecycle-analytics-book
RSCRIPT ?= Rscript

.PHONY: extract run clean list

extract:
	python3 tools/extract.py $(BOOK)

list:
	@ls scripts/*.R | sed 's|scripts/||;s|\.R$$||'

# Run every script, reporting the ones that fail. Some are slow: Chapter 20's
# echelon searches and Chapter 9's model fits dominate the total.
run:
	@fail=0; for s in scripts/*.R; do \
	  printf '%-56s' "$$s"; \
	  if $(RSCRIPT) $$s >/dev/null 2>&1; then echo OK; \
	  else echo FAIL; fail=1; fi; \
	done; exit $$fail

clean:
	rm -rf out
