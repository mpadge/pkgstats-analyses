.PHONY: doc

FILE=pkgstats
LFILE=motivation-letter

all: doc clean

doc:
	Rscript -e 'rmarkdown::render("$(FILE).Rmd", output_format="rjtools::rjournal_web_article")'

clean:
	rm $(FILE).log

letter:
	pandoc -s $(LFILE).md -o $(LFILE).pdf

open:
	xdg-open pkgstats.html
