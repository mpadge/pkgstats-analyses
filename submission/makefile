.PHONY: doc

FILE=pkgstats

all: doc clean

doc:
	Rscript -e 'rmarkdown::render("$(FILE).Rmd", output_format="rjtools::rjournal_web_article")'

clean:
	rm $(FILE).R $(FILE).log $(FILE).tex

open:
	xdg-open pkgstats.html
