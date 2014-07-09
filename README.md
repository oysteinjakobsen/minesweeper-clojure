# Minesweeper

I recently played with Clojure for the first time, which resulted in this implementation of the Minesweeper game.
The UI is text based and placed in a separate module from the core game functionality.

## Usage

Run from the REPL:

```clojure
(minesweeper.repl/play width height number-of-mines)
```

From Leiningen:

```
lein run 9 9 10
```

Add **-c** if you're running in a terminal supporting ansi coloring.

## Tests

To run Speclj tests, run 

```
lein spec -a
```

Add **-C** if you're running in a terminal _not_ supporting ansi coloring.

## See also

You can find a web-based GUI for this game core here:
https://github.com/oysteinjakobsen/minesweeper-webapp-clojure

