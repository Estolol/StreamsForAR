# StreamsForAR (Project for [INF551 Artificial Intelligence in Mathematical Reasoning](http://www.enseignement.polytechnique.fr/informatique/INF551/) @ Ecole polytechnique).  


## Description

This project gives an example of how streams can be used to efficiently and fairly explore search spaces. Here we are concerned with Streams of strings of three different characters (A, B, and C) for which a string is a sub-case of another if the first is a prefix of the latter.

## Code structure

All source files are in the *src* folder.  

* **stringtrees.ml**: Everything related to characters (A, B, or C), words (lists of characters) and stringtrees (trees of words designed to work like a search tree).  
* **streams.ml**: Everything about streams and how to combine them. pipetrees are a structure for storing a complex combination of streams.  
* **conversion.ml**: Every conversion function: how to turn a stringtree into a stream, a pipetree into a stream, a pipetree into a list, a word into a string, etc.  
* **printing.ml**: Printing functions.  
* **testing.ml**: Testing functions.  
* **main.ml**: This is where I play around with streams and test things using functions in testing.ml.  
