.PHONY: doc

all: doc

doc:
	Rscript -e 'rmarkdown::render("pkgstats.Rmd", output_format="rjtools::rjournal_web_article")'

open:
	xdg-open pkgstats.html
