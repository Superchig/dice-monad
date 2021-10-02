all: grammar/TestDiceExpr

grammar/TestDiceExpr: grammar/DiceExpr.cf grammar/Makefile
	cd grammar && make

	cp grammar/{AbsDiceExpr.hs,ErrM.hs,LexDiceExpr.hs} src
	cp grammar/{ParDiceExpr.hs,PrintDiceExpr.hs,SkelDiceExpr.hs} src

	# cp grammar/*.hs src

grammar/Makefile:
	cd grammar && bnfc -m --haskell DiceExpr.cf

clean:
	rm -f grammar/*.{hs,o,hi.txt,x,y} grammar/{Makefile,TestDiceExpr}

	rm -f src/{TestDiceExpr.hs,AbsDiceExpr.hs,ErrM.hs,LexDiceExpr.hs}
	rm -f src/{ParDiceExpr.hs,PrintDiceExpr.hs,SkelDiceExpr.hs,TestDiceExpr.hs,TestDiceExpr}

	stack clean
