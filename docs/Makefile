SHELL := /bin/sh

EMACS ?= emacs

.DEFAULT_GOAL := docs

.PHONY: docs texi info html pdf clean

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
