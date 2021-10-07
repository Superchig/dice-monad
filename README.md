# dice-monad

This parses and rolls "dice expressions," as in Dungeons and Dragons.

For example, `1d20+5` would mean roll a 20-sided die and add 5 to it. And
`10d6+40` would mean roll 10 6-sided dice and add 40 to the result.

## Build Requirements
- stack 
- bnfc
- happy
- alex
- make
- grep
- awk

## Steps To Build
You should be able to build this project with simply
```bash
make
```
and then run the executable dice-monad-exe with
```bash
echo "1d20+5" | ./dice-monad-exe
```

## TODO

These may never get done, but it'd sure be nice if they were implemented.

- [ ] Write [unit tests](https://hackage.haskell.org/package/HUnit) for the
  evaluation of dice expressions
- [ ] Write [some sort of GUI](https://github.com/fjvallarino/monomer) so that
  the project is actually usable
- [ ] Call bnfc from Setup.hs, rather than layering a Makefile on top of stack
