all: happy.ps happy.info

%.info : %.texinfo
	makeinfo $< -o $*.info

%.ps : %.dvi
	dvips $< -o $*.ps

%.dvi : %.texinfo
	tex $<

clean:
	rm -f *.aux *.cp *.fn *.ky *.log *.pg *.toc *.tp *.vr *~

cleaner: clean
	rm -f *.ps *.info *.dvi
