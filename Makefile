.PHONY : all clean

all: doc/example.pdf

doc/example.pdf : doc/example.tex doc/example.tikz
	xelatex --file-line-error --interaction=nonstopmode --output-directory="doc" $<

doc/example.tikz : doc/example.jl
	julia $<

clean :
	${RM} doc/*.aux doc/*.log doc/example.tikz doc/example.pdf

