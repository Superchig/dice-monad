# dice-monad

This parses and rolls "dice expressions," as in Dungeons and Dragons.

For example, `1d20+5` would mean roll a 20-sided die and add 5 to it.

## Build Requirements
- stack 
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
./dice-monad-exe
```

## TODO

These may never get done, but it'd sure be nice if they were implemented.

- [ ] Write [unit tests](https://hackage.haskell.org/package/HUnit) for the
  evaluation of Dice Expressions
- [ ] Write [some sort of GUI](https://github.com/fjvallarino/monomer) so that
  the project is actually usable
