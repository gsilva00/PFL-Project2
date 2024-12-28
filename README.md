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

## Description of the game

> a brief description of the game and its rules; you should also include the links used to gather information (official game website, rule book, etc.)

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

**Query 1:** Give me a way to clear the terminal in Prolog.

**Answer 1:**
You can add a predicate to clear the terminal in Linux by using the ANSI escape code. Here is how you can do it:

```prolog
% ...existing code...

% clear_console.
% Clears the terminal screen.
clear_console :-
    write('\33\[2J').

% ...existing code...
```

### Prolog documentation (SICStus)

[format predicate](https://sicstus.sics.se/sicstus/docs/4.9.0/html/sicstus/mpg_002dref_002dformat.html)
[between library](https://sicstus.sics.se/sicstus/docs/latest/html/sicstus.html/lib_002dbetween.html)
[consult predicate](https://sicstus.sics.se/sicstus/docs/3.7.1/html/sicstus_8.html)

> List of books, papers, web pages and other resources used during the development of the assignment. If you used tools such as ChatGPT, list the queries used.
