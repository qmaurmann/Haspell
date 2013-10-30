Haspell
=======

Simple text-based Scrabble game against a formidable computer opponent. Tested with GHC 7.4.1 on Ubuntu 12.04.

The AI simply searches the space of all possible moves, and chooses the highest scoring move, adjusted by simple heuristics (such as: don't waste the S's and blanks for modest gains). The search algorithm is based on prefix trees, and runs quite quickly except in the rare situation that the computer player has *both* blank tiles in hand (which induces a lag of 5-30 seconds on my machine).

To play, simply compile

    ghc Scrabble.hs

and run as

    ./Scrabble dictionary.txt

By default, the human player makes the opening move; run as

    ./Scrabble dictionary.txt 2

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

The characters 2 and 3 indicate open double and triple letter score squares respectively; the "upper case" 2 and 3 (@ and #) indicate double and triple word score squares.

Moves are entered in the following format:

    DIRECTION ROW COLUMN WORD

Here the DIRECTION is H for horizontal, or V for vertical, and ROW and COLUMN are both numbers between 1 and 15. For example

    H 8 6 CATS

will take the necessary letters from your hand (all of C, A, T, S if this is the opening move, or a subset of these letters if some of them are already on the board) to play the word CATS horizontally, in row 8, starting at column 6. While illegal plays are not *punished* (there is no "challenging" of moves), they are *caught*, and you will be prompted for another move.

Blank characters are indicated as lower case letters, e.g. CaTS.

A player may also pass his or her turn, exchanging 0 or more letters from hand, e.g. with

    PASS ABC

or simply

    PASS

to exchange no letters.

Per [standard rules](http://www.scrabbleplayers.org/rules/rules-20130910.pdf), a non-zero number of letters may be exchanged only if there are at least 7 letters remaining in the bag. Six consecutive passes ends the game early. (Slightly more generally, six consecutive zero-scoring turns ends the game early; in a game without "challenges" to moves, these conditions are *almost* equivalent.)

