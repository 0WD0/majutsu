.PHONY: docs info html pdf clean

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
	@emacs --batch -l org $< -f org-texinfo-export-to-texinfo

html: majutsu.html

majutsu.html: majutsu.texi
	@printf "Generating $@\n"
	@makeinfo --html --no-split $< -o $@

pdf: majutsu.pdf

majutsu.pdf: majutsu.texi
	@printf "Generating $@\n"
	@texi2pdf --clean $< > /dev/null

clean:
	@printf "Cleaning docs/*...\n"
	@rm -f majutsu.info majutsu.html majutsu.pdf dir
