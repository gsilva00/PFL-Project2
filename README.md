# Functional and Logic Programming - 3rd Year 1st Semester - 2nd Project (TP2)

## Game

turtles, as described by the creator in the [official website](https://turtlesgame.xyz/), is a game of turtles colliding in three dimensions.

## Group T12G02

Guilherme Moura Pinho Andrade Silva, <up202205298@up.pt>

Valentina Pereira Cadime, <up202206262@up.pt>

## Contribution

## Guilherme Silva (50%)

-
-
-

## Valentina Cadime (50%)

-
-
-

## Installation and Execution

load_files('~/Desktop/projectPFL2/src/game.pl').

> include all the necessary steps for the correct execution of the game in both Linux and Windows environments (in addition to the installation of SICStus Prolog 4.9).

After loading the correct files, use SICSTus to consult the `game.pl` file. Then, proceed to write `play.` in the console and press `Enter`. The game will be displayed shortly after.

## Description of the game

> a brief description of the game and its rules; you should also include the links used to gather information (official game website, rule book, etc.)

`turtles, a game of turtles colliding in three dimensions` is a competitive two-player game, where the players have to face eachother to win the game.

**Goal:** Get all of your turtles in the oponent's nest.

### How to start the game:
The menu of the game will include all the gameplay options available at the moment:  Human vs. Human (H/H), Human vs. Computer (H/PC), Computer vs. Human (PC/H), Computer vs. Computer (PC/PC). Choose a number between 1-4 to play the respective gamemode, otherwise write 5 to exit the game.

If you choose to have humam players, the game will

**Setup:** Each player has 5 turtles that they can move on the 4 x 2 board. Firstly, each player will have all of their turtles placed inside their nest (outside the board).

Each 




**How to win:** 

## Considerations for game extensions

> describe the considerations taken into account when extending the game design, namely when considering variable-sized boards, optional rules (e.g., simplified rules for novice players, additional rules for expert players), and other aspects.

## Game Logic

> Describe the main design decisions regarding the implementation of the game logic in Prolog (do not copy the source code). This section should have information on the following topics, among others:

## Game Configuration Representation

> describe the information required to represent the game configuration, how it is represented internally and how it is used by the initial_state/2 predicate.

## Internal Game State Representation

> describe the information required to represent the game state, how it is represented internally, including an indication of the meaning of each atom (i.e. how different pieces are represented). Include examples of representations of initial, intermediate, and final game states.

## Move Representation

> describe the information required to represent a move, and how it is represented internally (e.g., the coordinates of a board location, and/or other information necessary to represent a move) and how it is used by the move/3 predicate.

## User Interaction

> briefly describe the game menu system, as well as how interaction with the user is performed, focusing on input validation (e.g., when reading a move).

## Conclusions

> Conclusions about the work carried out, including limitations of the program (and known issues), as well as possible improvements (future developments roadmap).

## Bibliography

### ChatGPT queries

**Query 1:** How to include variables' values in strings printed to console in SICStus prolog?

**Answer 1:** In SICStus Prolog, you can include variables' values in strings printed to the console using the format/2 or format/3 predicates. Here's an example:

```prolog
% Example using format/2
print_values(X, Y) :-
    format('The value of X is ~w and the value of Y is ~w.~n', [X, Y]).

% Example using format/3
print_values_with_stream(Stream, X, Y) :-
    format(Stream, 'The value of X is ~w and the value of Y is ~w.~n', [X, Y]).
```

In these examples:

- ~w is a placeholder for a term.
- [X, Y] is the list of variables whose values will replace the placeholders in the string.

You can call these predicates like this:

```prolog
?- print_values(10, 20).
The value of X is 10 and the value of Y is 20.
true.

?- open('output.txt', write, Stream),
   print_values_with_stream(Stream, 10, 20),
   close(Stream).
```

This will print the values of X and Y to the console or write them to a file.

**Query 2:** Give me a way to clear the terminal in Prolog.

**Answer 2:**
You can add a predicate to clear the terminal in Linux by using the ANSI escape code. Here is how you can do it:

```prolog
% ...existing code...

% clear_console.
% Clears the terminal screen.
clear_console :-
    write('\33\[2J').

% ...existing code...
```

**Query 3:** Write me write/1 predicates that draw this ASCII art:

```plaintext
  _____     ____
 /      \  |  o |
|        |/ ___\|
|_________/
|_|_| |_|_|
```

**Answer 3:** Here is the write/1 predicates to draw the given ASCII art:

```prolog
draw_ascii_art :-
    write('  _____     ____'), nl,
    write(' /      \\  |  o |'), nl,
    write('|        |/ ___\\|'), nl,
    write('|_________/     '), nl,
    write('|_|_| |_|_|'), nl.
```

You can call draw_ascii_art to display the ASCII art.

**Query 4:** How to get a random element from a list in SICStus Prolog?

**Answer 4:** To get a random element from a list in SICStus Prolog, you can use the random_member/2 predicate. Here's an example:

```prolog
% Define a predicate to get a random element from a list
random_element(List, Element) :-
    random_member(Element, List).

% Example usage
?- random_element([a, b, c, d, e], X).
```

In this example, X will be unified with a random element from the list [a, b, c, d, e].

**Query 5:** Can this:

```prolog
translate_turtle(white-1, 'W1').
translate_turtle(white-2, 'W2').
translate_turtle(white-3, 'W3').
translate_turtle(white-4, 'W4').
translate_turtle(white-5, 'W5').
translate_turtle(black-1, 'B1').
translate_turtle(black-2, 'B2').
translate_turtle(black-3, 'B3').
translate_turtle(black-4, 'B4').
translate_turtle(black-5, 'B5').
```

be generalized into a single predicate?

**Answer 5:** Yes, you can generalize this into a single predicate by using a variable for the color and number. Here's how you can do it:

```prolog
translate_turtle(Color-Number, Code) :-
  sub_atom(Color, 0, 1, _, FirstLetter),
  number_chars(Number, NumberChars),
  atom_chars(NumberAtom, NumberChars),
  atom_concat(FirstLetter, NumberAtom, Code).
```

### Prolog documentation (SICStus)

[format predicate](https://sicstus.sics.se/sicstus/docs/4.9.0/html/sicstus/mpg_002dref_002dformat.html)
[between library](https://sicstus.sics.se/sicstus/docs/latest/html/sicstus.html/lib_002dbetween.html)
[consult predicate](https://sicstus.sics.se/sicstus/docs/3.7.1/html/sicstus_8.html)
[lists library](https://sicstus.sics.se/sicstus/docs/4.9.0/html/sicstus/lib_002dlists.html)
[Understanding input tools](https://sicstus.sics.se/sicstus/docs/4.9.0/html/sicstus/ref_002diou_002dcin.html)
[random library](https://sicstus.sics.se/sicstus/docs/4.9.0/html/sicstus/lib_002drandom.html)

> List of books, papers, web pages and other resources used during the development of the assignment. If you used tools such as ChatGPT, list the queries used.
