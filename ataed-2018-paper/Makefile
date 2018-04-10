.PHONY: main.pdf all clean

all: main.pdf

main.pdf:
	latexmk -pdf -pdflatex="pdflatex -shell-escape -interactive=nonstopmode" -use-make main.tex

clean:
	latexmk -CA