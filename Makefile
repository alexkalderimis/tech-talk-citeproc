slides.html: slides.md
	@pandoc --to revealjs \
		   --standalone \
		   --output slides.html \
		   --mathjax \
		   --highlight-style=espresso \
		   --variable width:1200 \
		   --variable history:true \
		   --variable revealjs-url:node_modules/reveal.js \
		   slides.md
