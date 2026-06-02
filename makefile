LFILE = pkgstats

all: help

doc: ## Update package documentation with `roxygen2`
	Rscript -e 'roxygen2::roxygenise()'

render: ## Render pkgdown site
	init knith

init: ## Initialise pkgdown site
	echo "pkgdown::init_site()" | R --no-save -q

knith: ## Build the main vignette
	echo "pkgdown::build_article('$(LFILE)',quiet=FALSE)" | R --no-save -q

open: ## Open main vignette in browser
	xdg-open docs/articles/pkgstats.html &

check: ## Run `rcmdcheck`
	Rscript -e 'rcmdcheck::rcmdcheck()'

test: ## Run test suite
	Rscript -e 'testthat::test_local()'

clean: ## Rm rendered pkgdown 'docs/' folder
	rm -r docs/

help: ## Show this help
	@printf "Usage:\033[36m make [target]\033[0m\n"
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}'
