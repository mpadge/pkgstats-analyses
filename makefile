LFILE = pkgstats

all: init knith
#all: knith open 

init:
	echo "pkgdown::init_site()" | R --no-save -q

knith:
	echo "pkgdown::build_article('$(LFILE)')" | R --no-save -q

open:
	xdg-open docs/articles/pkgstats.html &

clean:
	rm -r docs/
