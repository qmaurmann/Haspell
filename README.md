Haspell
=======

(This is the first working version; the code is a bit of a mess and will be improved in subsequent commits.)

Simple text-based Scrabble game against a computer opponent. At present, the computer just (quickly!) searches the space of all possible moves and chooses the highest scoring move. Despite completely ignoring long-term strategy, the computer is quite a formidable opponent!

Simply compile

    ghc Rules.hs

and run as

    ./Rules dictionary.txt

By default, the human player makes the opening move; run as

    ./Rules dictionary.txt 2

if you'd prefer to play second.

The file dictionary.txt is the 178,691-word [Official Tournament and Club Word List (OWL)](http://en.wikipedia.org/wiki/Official_Tournament_and_Club_Word_List) as it appears [here](https://code.google.com/p/scrabblehelper2/source/browse/trunk/src/scrabble/dictionary.txt?r=3). The game can be played with an alternate dictionary in the obvious way.


Game play
---------

The starting board looks like this:

      | 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5
    --+------------------------------
    1 | #     2       #       2     #
    2 |   @       3       3       @  
    3 |     @       2   2       @    
    4 | 2     @       2       @     2
    5 |         @           @        
    6 |   3       3       3       3  
    7 |     2       2   2       2    
    8 | #     2       @       2     #
    9 |     2       2   2       2    
    0 |   3       3       3       3  
    1 |         @           @        
    2 | 2     @       2       @     2
    3 |     @       2   2       @    
    4 |   @       3       3       @  
    5 | #     2       #       2     #

The characters 2 and 3 indicate double and triple letter score squares respectively; the "upper case" 2 and 3 (namely @ and #) squares indicate double and triple word score squares.

Moves are entered in the following format:

    direction row column word

Here the direction is H for horizontal, or V for vertical, and row and column are both numbers between 1 and 15. For example

    H 8 8 CATS 

will take the necessary letters from your hand (all of C, A, T, S if this is the opening move, or a subset of these letters if some of them are already on the board) to play the word CATS horizontally, in row 8, starting at column 8. Illegal plays are not punished (e.g. there is no "challenging"of moves); you will simply be prompted for another move.

Blank characters are indicated as lower case letters, e.g. CaTS.

A player may also pass his or her turn, exchanging 0 or more letters from hand, e.g. with

    pass ABC

Per standard rules, a non-zero number of letters may be exchanged only if there are at least 7 letters remaining in the bag. Six consecutive passes ends the game early. (Slightly more generally, six consecutive zero-scoring turns ends the game early; in a game without "challenges" to moves, these conditions are *almost* equivalent.)
