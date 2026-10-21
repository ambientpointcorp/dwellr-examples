#!/usr/bin/env python3
"""Extract each chapter's R chunks into one runnable script.

The book's figures and numbers are produced by chunks inside the chapter that
prints them, which is what makes them reproducible. This script turns those
chunks into a standalone file per chapter so that a reader can run a chapter's
analysis without rendering the book.

Extraction rather than transcription is the point: a hand-written example
drifts from the chapter it illustrates, and nobody notices until a number
disagrees. These scripts cannot drift, because they are generated.

Usage:
    python3 tools/extract.py ../lifecycle-analytics-book
"""
import pathlib
import re
import sys

PREAMBLE = """# Generated from {src} by tools/extract.py -- do not edit by hand.
# Chapter: {title}
#
# Run from the repository root:
#   Rscript {out}

{libraries}source("R/theme.R")

"""

# Every library() call in the chapter is hoisted into the preamble, including
# the ones in the setup chunk that the body skips. Chapter 1 loads R6 there and
# calls R6Class() later, so dropping the setup chunk without hoisting its
# libraries produced a script that failed on its first real line.
# Two shapes of load statement had to be handled before the scripts ran.
# Chapter 6 wraps its load in suppressPackageStartupMessages(), which an
# anchored pattern missed, and the script failed on survreg() three chunks
# later. Chapter 9 puts several loads on one line, so matching once per line
# hoisted survival and left ranger behind.
LIB = re.compile(r"(?:library|require)\(\s*([A-Za-z0-9._]+)\s*\)")


def libraries(text):
    """Collect every package an evaluated chunk loads, in first-use order."""
    seen, out = set(), []
    for name in ["dwellr", "ggplot2"] + loaded(text):
        if name not in seen:
            seen.add(name)
            out.append(f"library({name})")
    return "\n".join(out) + "\n\n"


def loaded(text):
    """Package names loaded by chunks that actually run.

    A chunk marked `eval: false` is a listing rather than code -- hoisting its
    loads would make a script depend on packages the chapter never needed.
    """
    names = []
    for m in re.finditer(r"```\{r\}\n(.*?)```", text, re.S):
        body = m.group(1)
        opts = [ln for ln in body.split("\n") if ln.startswith("#|")]
        if any("eval: false" in o for o in opts):
            continue
        for ln in body.split("\n"):
            code = ln.split("#")[0]           # ignore commented-out loads
            names.extend(LIB.findall(code))
    return names


def chunks(text):
    """Yield (label, code) for every R chunk, in order."""
    for m in re.finditer(r"```\{r\}\n(.*?)```", text, re.S):
        body = m.group(1)
        lines, label, keep = body.split("\n"), None, []
        for ln in lines:
            if ln.startswith("#|"):
                if "label:" in ln:
                    label = ln.split("label:")[1].strip()
                continue
            keep.append(ln)
        code = "\n".join(keep).strip()
        if code:
            yield label or "unlabelled", code


def title_of(text):
    m = re.search(r"^# (.+?)(?:\s*\{|$)", text, re.M)
    return m.group(1).strip() if m else "untitled"


def main(book):
    src_dir = pathlib.Path(book) / "chapters"
    if not src_dir.is_dir():
        sys.exit(f"no chapters/ under {book}")
    out_dir = pathlib.Path("scripts")
    out_dir.mkdir(exist_ok=True)
    written = 0
    for qmd in sorted(src_dir.glob("*.qmd")):
        text = qmd.read_text()
        got = list(chunks(text))
        if not got:
            continue
        out = out_dir / (qmd.stem + ".R")
        parts = [PREAMBLE.format(src=f"chapters/{qmd.name}",
                                 title=title_of(text), out=out,
                                 libraries=libraries(text))]
        for label, code in got:
            # the setup chunk only sets the theme and the seed, both of which
            # the preamble and R/theme.R now supply
            if label == "setup":
                continue
            body = LIB.sub("", code).strip()
            if not body:
                continue
            parts.append(f"## ---- {label} " + "-" * max(0, 60 - len(label)))
            parts.append(body + "\n")
        out.write_text("\n".join(parts))
        written += 1
        print(f"{out}  ({len(got)} chunks)")
    print(f"\n{written} scripts written")


if __name__ == "__main__":
    main(sys.argv[1] if len(sys.argv) > 1 else "../lifecycle-analytics-book")
