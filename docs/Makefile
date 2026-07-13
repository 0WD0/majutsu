SHELL := /bin/sh

EMACS ?= emacs
NPM ?= npm
NODE ?= node
REPO_ROOT := $(abspath $(CURDIR)/..)
CACHE_ROOT := $(REPO_ROOT)/.cache/majutsu-docs
DIST_ROOT := $(REPO_ROOT)/dist
ENVELOPE_ROOT := $(CACHE_ROOT)/envelopes
COMPILED_ROOT := $(CACHE_ROOT)/compiled
SITE_ROOT := $(CURDIR)/site
MAJUTSU_ENVELOPE := $(ENVELOPE_ROOT)/majutsu.edn
DOCS_SOURCE_REVISION ?= $(shell jj --repository "$(REPO_ROOT)" log --no-graph -r @ -T commit_id)

.DEFAULT_GOAL := docs

.PHONY: docs texi info html pdf clean \
	docs-content-check docs-texi-check docs-contract-check docs-test \
	docs-export docs-compile docs-site docs-search docs-check docs-build \
	docs-dev docs-clean

docs: info

info: majutsu.info dir

dir: majutsu.info
	@printf "Generating $@\n"
	@install-info --dir=$@ $<

majutsu.info: majutsu.texi
	@printf "Generating $@\n"
	@makeinfo --no-split $< -o $@

texi: majutsu.texi

majutsu.texi: majutsu.org
	@printf "Generating $@\n"
	@$(EMACS) --batch -Q -l org \
		$< -f org-texinfo-export-to-texinfo
	@grep -Fq 'GENERATED—DO NOT EDIT' $@

html: majutsu.html

majutsu.html: majutsu.texi
	@printf "Generating $@\n"
	@makeinfo --html --no-split $< -o $@

pdf: majutsu.pdf

majutsu.pdf: majutsu.texi
	@printf "Generating $@\n"
	@texi2pdf --clean $< > /dev/null

clean:
	@printf "Cleaning traditional generated documentation...\n"
	@rm -f -- majutsu.info majutsu.html majutsu.pdf dir

docs-content-check:
	@$(EMACS) --batch -Q --script "$(CURDIR)/scripts/check-content.el" "$(REPO_ROOT)"

docs-texi-check:
	@REPO_ROOT="$(REPO_ROOT)" EMACS="$(EMACS)" \
		"$(CURDIR)/scripts/check-texi-drift.sh"

docs-contract-check: docs-content-check docs-texi-check
	@printf '%s\n' 'DOCS_CONTRACT_CHECK=PASS'

docs-test: docs-contract-check

docs-export:
	@if test -z "$(OX_EDN_ROOT)"; then \
		printf '%s\n' 'BLOCKED_ENV: docs-export requires OX_EDN_ROOT' >&2; \
		exit 2; \
	fi
	@mkdir -p "$(ENVELOPE_ROOT)"
	@$(EMACS) --batch -Q --script "$(CURDIR)/scripts/export-site.el" \
		"$(OX_EDN_ROOT)" "$(REPO_ROOT)" "$(CURDIR)/majutsu.org" "$(MAJUTSU_ENVELOPE)"

docs-compile: docs-export
	@if test -z "$(LOAM_ROOT)"; then \
		printf '%s\n' 'BLOCKED_ENV: docs-compile requires LOAM_ROOT' >&2; \
		exit 2; \
	fi
	@$(NPM) --silent --prefix "$(LOAM_ROOT)" run docs:compile -- \
		--repo-root "$(REPO_ROOT)" --output-dir "$(COMPILED_ROOT)" \
		--base /docs --version dev \
		--commit-id "$(DOCS_SOURCE_REVISION)" \
		"$(MAJUTSU_ENVELOPE)"

docs-site: export MAJUTSU_DOCS_SOURCE_REVISION = $(DOCS_SOURCE_REVISION)
docs-site: export MAJUTSU_DOCS_CHANNEL = dev-preview
docs-site: docs-compile
	@if ! test -x "$(SITE_ROOT)/node_modules/.bin/astro"; then \
		$(NPM) --prefix "$(SITE_ROOT)" ci --no-audit --no-fund; \
	fi
	@$(NPM) --prefix "$(SITE_ROOT)" run build

docs-search: docs-site
	@test -f "$(DIST_ROOT)/pagefind/pagefind.js"
	@printf '%s\n' 'DOCS_SEARCH=PASS provider=pagefind'

docs-check: docs-search docs-contract-check
	@$(NODE) "$(SITE_ROOT)/scripts/check-build.mjs" \
		"$(COMPILED_ROOT)/manifest.json" "$(COMPILED_ROOT)/build-report.json" \
		"$(DIST_ROOT)"
	@$(NODE) "$(SITE_ROOT)/scripts/check-search.mjs" \
		"$(DIST_ROOT)" "$(COMPILED_ROOT)/manifest.json" \
		"$(SITE_ROOT)/tests/fixtures/search.json"
	@$(NPM) --prefix "$(SITE_ROOT)" run test:e2e

docs-build: docs-check

docs-dev: docs-compile
	@if ! test -x "$(SITE_ROOT)/node_modules/.bin/astro"; then \
		$(NPM) --prefix "$(SITE_ROOT)" ci --no-audit --no-fund; \
	fi
	@MAJUTSU_DOCS_CHANNEL=dev-preview $(NPM) --prefix "$(SITE_ROOT)" run dev

docs-clean: clean
	@printf 'Cleaning %s and %s...\n' "$(CACHE_ROOT)" "$(DIST_ROOT)"
	@rm -rf -- "$(CACHE_ROOT)" "$(DIST_ROOT)"
