slides.html: slides.md
	@pandoc --to revealjs \
		   --standalone \
		   --output slides.html \
		   --latexmathml \
		   --variable revealjs-url:node_modules/reveal.js \
		   slides.md