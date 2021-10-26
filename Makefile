install:
	Rscript -e 'pkgs <- c("devtools", "usethis"); sapply(pkgs, function(z) if (!requireNamespace(z, quietly = TRUE)) install.packages(z))'
	Rscript -e 'example("source"); sourceDir("data-raw")'
	Rscript -e 'devtools::document()'
	R CMD INSTALL "."
