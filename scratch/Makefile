filename=Translation

tex: ${filename}.lhs
	 lhs2TeX -o ${filename}.tex ${filename}.lhs

pdf: ps
	ps2pdf ${filename}.ps

ps: dvi
	dvips ${filename}.dvi

dvi: tex
	latex ${filename}.tex
	latex ${filename}.tex

read:
	evince ${filename}.pdf &

aread:
	acroread ${filename}.pdf &

clean:
	rm -f ${filename}.ps ${filename}.ptb ${filename}.tex ${filename}.pdf ${filename}.log ${filename}.aux ${filename}.out ${filename}.dvi ${filename}.bbl ${filename}.blg
