play :-
  write('Welcome to the Turtles Game!'), nl,
  display_menu,
  get_choice(Choice),
  execute_choice(Choice).

display_menu :-
  write('1. Human vs Human (H/H)'), nl,
  write('2. Human vs Computer (H/PC)'), nl,
  write('3. Computer vs Computer (PC/PC)'), nl,
  write('4. Exit'), nl.

get_choice(Choice) :-
  write('Type your choice: '),
  read(Choice).

execute_choice(1) :-
  write('Starting Human vs Human game...'), nl,
  start_game(human, human).
execute_choice(2) :-
  write('Starting Human vs Computer game... but first!'), nl,
  choose_difficulty(1, Difficulty),
  start_game(human, computer(Difficulty)).
execute_choice(3) :-
  write('Starting Computer vs Computer game... but first!'), nl,
  choose_difficulty(1, Difficulty1),
  choose_difficulty(2, Difficulty2),
  start_game(computer(Difficulty1), computer(Difficulty2)).
execute_choice(4) :-
  write('Exiting the game! See you soon!'), nl.
execute_choice(_) :-
  write('Invalid choice, please try again.'), nl,
  play.

choose_difficulty(Num, Difficulty) :-
  format('Choose Computer ~w\'s difficulty level:', [Num]), nl,
  write('1. Easy'), nl, % AI executes random moves
  write('2. Hard'), nl, % AI executes the best move
  write('Type your choice: '),
  read(DifficultyChoice),
  ( difficulty_level(DifficultyChoice, Difficulty) -> true
  ; write('Invalid choice, please try again.'), nl,
    choose_difficulty(Num, Difficulty)
  ).

difficulty_level(1, easy).
difficulty_level(2, hard).


start_game(Player1, Player2) :-
  format('Game started with ~w vs ~w~n', [Player1, Player2]).


% TODO: Implement the following predicates

initial_state(+GameConfig, -GameState).

display_game(+GameState).

move(+GameState, +Move, -NewGameState).

valid_moves(+GameState, -ListOfMoves).

game_over(+GameState, -Winner).

value(+GameState, +Player, -Value).

choose_move(+GameState, +Level, -Move).