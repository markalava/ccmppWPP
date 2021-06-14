#!/bin/bash
Rscript -e 'example("source"); sourceDir("data-raw")'
Rscript -e 'devtools::document()'
R CMD INSTALL "."
