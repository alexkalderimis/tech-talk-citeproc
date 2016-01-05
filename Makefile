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

xml-parsing.html: xml-parsing.md
	@pandoc --to revealjs \
		   --standalone \
		   --output xml-parsing.html \
		   --mathjax \
		   --highlight-style=espresso \
		   --variable width:1200 \
		   --variable history:true \
		   --variable revealjs-url:node_modules/reveal.js \
	       --include-in-header custom.css \
		   xml-parsing.md
