# HS Scheme

This is a learning project, initially implementing based on  
[this](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours) 
tutorial.  After some initial work I also discovered 
[version 2](https://wespiser.com/writings/wyas/home.html) of that tutorial which 
uses more "modern" haskell techniques.  It is at least useful to have multiple 
versions to compare my own implementation against. I also tracked down the 
[R7RS report](https://small.r7rs.org/attachment/r7rs.pdf), which has been 
immensely useful in expanding the parsing logic beyond the tutorial examples.


## Building and running

The project has been most recently tested and developed with GHC v8.10.7 and 
Cabal v3.6.2.  To the best of my knowledge it does not rely on any particularly 
recent features of either of those tools or the dependent libraries and should 
be backwards compatible beyond the bounds listed in the .cabal file.  

- To build the entire project:  `cabal build`
- To run the main CLI application:  `cabal run`
- To run the test suite:  `cabal test`


## Next steps

- Parse more than one expression.
- Better test #true and #false parsing.  I think I am currently leaving "rue" and "alse" unconsumed.  
- Fix parsing failure on missing start quote.  This might be "working" but will instead fail on a full program file parse.
- Maybe add Nil datatype?  Or let it be a special case (empty) of list?
- Get started on evaluation logic.