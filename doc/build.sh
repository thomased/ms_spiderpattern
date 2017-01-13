#!/bin/bash

# Build main doc
pandoc ms.md -o ms.pdf --bibliography refs.bib --csl refs.csl -H ms.sty --latex-engine=xelatex

open ms.pdf
