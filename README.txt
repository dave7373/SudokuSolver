Erlang code for solving sudoku puzzles with brute force - Erlang ADT demonstration
Author: David Almroth, Stockholm, 2012-09-01		

The code was written to show how to make a Abstract Data Type (ADT) in Erlang.
The datatype here is matrix.erl and it encapsulates how the Sudoku matrix is
represented. 

Good to know: The N-value used in the code is the size of the Sudoku matrix.
A normal Sudoku with size 9 x 9 has an N-value of 9.

The module sudoku.erl has the brute force code and some test cases for Sudokus 
with size N=4 and N=9. 
