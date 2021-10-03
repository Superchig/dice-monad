all: dice-monad-exe

grammar/TestDiceExpr: grammar/DiceExpr.cf grammar/Makefile
	cd grammar && make

	cp grammar/{AbsDiceExpr.hs,ErrM.hs,LexDiceExpr.hs} src
	cp grammar/{ParDiceExpr.hs,PrintDiceExpr.hs,SkelDiceExpr.hs} src

grammar/Makefile:
	cd grammar && bnfc -m --haskell DiceExpr.cf

dice-monad-exe: src/Lib.hs src/DirectInterpret.hs app/Main.hs grammar/TestDiceExpr grammar/Makefile
	stack build

	cp "$$(stack path | grep local-install-root | awk '{print $$2}')/bin/dice-monad-exe" .

clean:
	rm -f grammar/*.{hs,o,hi.txt,x,y} grammar/{Makefile,TestDiceExpr}

	rm -f src/{TestDiceExpr.hs,AbsDiceExpr.hs,ErrM.hs,LexDiceExpr.hs}
	rm -f src/{ParDiceExpr.hs,PrintDiceExpr.hs,SkelDiceExpr.hs,TestDiceExpr.hs,TestDiceExpr}

	rm -f dice-monad-exe

	stack clean
