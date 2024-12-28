:- consult(aux).


config(Width,Length,Level1,Level2).

% Game start
play :-
  clear,
  banner,
  choose_gamemode(GameMode),
  choose_board_size(Width, Length),
  choose_difficulty(GameMode, Level1-Level2),
  clear.


banner :-
  writeln('================================'),
  writeln('  Welcome to the Turtles Game!  '),
  writeln('         _____     ____'),
  writeln('        /      \\  |  o |'),
  writeln('       |        |/ ___\\|'),
  writeln('       |_________/     '),
  writeln('       |_|_| |_|_|'),
  writeln('================================').

% Gamemode
choose_gamemode(GameMode) :-
  display_gamemode_menu,
  get_choice_ln('Option', Choice, 1, 5),
  gamemode(Choice, GameMode),
  handle_gamemode(GameMode).

display_gamemode_menu :-
  writeln('Choose your game mode'),
  writeln('1. Human vs. Human (H/H)'),
  writeln('2. Human vs. Computer (H/PC)'),
  writeln('3. Computer vs. Human (PC/H)'),
  writeln('4. Computer vs. Computer (PC/PC)'),
  writeln('5. Exit').

gamemode(1, hh).
gamemode(2, hc).
gamemode(3, ch).
gamemode(4, cc).
gamemode(5, exit).

handle_gamemode(exit) :- halt.
handle_gamemode(_).

% Board Size
choose_board_size(Width, Length) :-
  writeln('Turtles will be placed on a rectangular board. You can choose its size!'),
  display_board_width_menu,
  get_choice_ln('Option', Width, 2, 6),
  display_board_length_menu,
  get_choice_ln('Choose the board length', Length, 4, 8).

display_board_width_menu :-
  writeln('Choose the board width: 2, 3, 4, 5 or 6').

display_board_length_menu :-
  writeln('Choose the board length: 4, 5, 6, 7 or 8').

% Difficulty
%% Human vs. Human - no difficulty
choose_difficulty(hh, 0-0).
choose_difficulty(hc, 0-Level2) :-
  display_difficulty_menu(1),
  get_choice_ln('Option', DifficultyChoice, 1, 2),
  difficulty_level(DifficultyChoice, Level2).

choose_difficulty(ch, Level1-0) :-
  display_difficulty_menu(1),
  get_choice_ln('Option', DifficultyChoice, 1, 2),
  difficulty_level(DifficultyChoice, Level1).

choose_difficulty(cc, Level1-Level2) :-
  display_difficulty_menu(1),
  get_choice_ln('Option', DifficultyChoice1, 1, 2),
  difficulty_level(DifficultyChoice1, Level1),
  display_difficulty_menu(2),
  get_choice_ln('Option', DifficultyChoice2, 1, 2),
  difficulty_level(DifficultyChoice2, Level2).


display_difficulty_menu(ComputerNum) :-
  format('Choose Computer ~d\'s difficulty level: ~n', [ComputerNum]),
  writeln('1. Easy'), % AI plays random moves
  writeln('2. Hard'). % AI plays the best move


difficulty_level(1, easy).
difficulty_level(2, hard).


% TODO: Implement the following predicates

initial_state(+GameConfig, -GameState) :-
  format('Game started with ~w vs. ~w!~n', [Player1, Player2]).

display_game(+GameState).

move(+GameState, +Move, -NewGameState).

valid_moves(+GameState, -ListOfMoves).

game_over(+GameState, -Winner).

value(+GameState, +Player, -Value).

choose_move(+GameState, +Level, -Move).