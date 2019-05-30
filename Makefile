make:
	racket example.rkt \
	&& pdflatex *.tex
	rm *.tex
	rm *.log
	rm *.aux
