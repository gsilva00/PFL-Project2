% MENUS

% banner/0
%% Display the game's banner
banner :-
  clear,
  write_ln('================================'),
  write_ln('  Welcome to the Turtles Game!  '),
  write_ln('         _____     ____'),
  write_ln('        /      \\  |  o |'),
  write_ln('       |        |/ ___\\|'),
  write_ln('       |_________/     '),
  write_ln('       |_|_| |_|_|'),
  write_ln('================================').

% display_main_menu/0
%% Display the main menu
display_gamemode_menu :-
  write_ln('Choose your game mode'),
  write_ln('1. Human vs. Human (H/H)'),
  write_ln('2. Human vs. Computer (H/PC)'),
  write_ln('3. Computer vs. Human (PC/H)'),
  write_ln('4. Computer vs. Computer (PC/PC)'),
  write_ln('5. Exit').

% display_first_player_menu(+Player1Name, +Player2Name)
%% Display the menu with the options to choose who plays first
display_first_player_menu(Player1Name, Player2Name) :-
  write('Choose who plays first: '), nl,
  format('1. ~w (white turtles)~n', [Player1Name]),
  format('2. ~w (black turtles)~n', [Player2Name]).

% display_difficulty_menu(+ComputerNum)
%% Display the menu with the options to choose the computer's difficulty level
display_difficulty_menu(ComputerNum) :-
  format('Choose Computer ~d\'s difficulty level: ~n', [ComputerNum]),
  write_ln('1. Easy'), % AI plays random moves
  write_ln('2. Hard'). % AI plays the best move

% display_board_width_menu/0
%% Display the menu with the options to choose the board's width
display_board_width_menu :-
  write_ln('Turtles will be placed on a rectangular board. You can choose its size!'),
  write_ln('Choose the board width: 2, 3, 4, 5 or 6').

% display_board_length_menu/0
%% Display the menu with the options to choose the board's length
display_board_length_menu :-
  write_ln('Choose the board length: 4, 5, 6, 7 or 8').



% GAME STATE

% display_winner(+Winner)
%% Display the winner of the game
display_winner(Winner) :-
  format('Congratulations, ~w! You won!~n', [Winner]).


% display_game(+GameState)
%% Display the game state
%% GameState is represented by the compound term - game_state(Turn, Nest1-Nest2, Board, Scored1-Scored2, Player1Name-Player1Level, Player2Name-Player2Level).
display_game(game_state(Turn, Nest1-Nest2, Board, Scored1-Scored2, Player1Name-Player1Level, Player2Name-Player2Level)) :-
  display_board(Board),
  display_score(Player1Name, Player2Name, Scored1, Scored2),
  display_nests(Player1Name, Player2Name, Nest1, Nest2),
  display_turn(Turn, Player1Name-Player1Level, Player2Name-Player2Level).

% display_board(+Board)
% TODO
display_board(Board).

% display_score(+Player1Name, +Player2Name, +Scored1, +Scored2)
%% Display the score of each player
display_score(Player1Name, Player2Name, Scored1, Scored2) :-
  length(Scored1, NumTurtles1),
  length(Scored2, NumTurtles2),
  format('Score:~n - ~w: ~d turtles scored~n - ~w: ~d turtles scored~n', [Player1Name, NumTurtles1, Player2Name, NumTurtles2]).

% display_nests(+Player1Name, +Player2Name, +Nest1, +Nest2)
%% Display the turtles in each player's nest
display_nests(Player1Name, Player2Name, Nest1, Nest2) :-
  format('Player ~w\'s nest has turtles:~n', [Player1Name]),
  display_list(false, Nest1),
  format('Player ~w\'s nest has turtles:~n', [Player2Name]),
  display_list(false, Nest2).

% display_list(+HasAtLeastOneElement, +List)
% Display a list of elements separated by a space (except for the last element)
display_list(true, []).
display_list(false, []) :-
  write_ln('None').
display_list(_, [H|T]) :-
  translate_turtle(H, Code),
  format('Turtle ~w~n', [Code]),
  display_list(true, T).

% display_turn(+Turn, +Player1Name, +Player2Name)
%% Display whose turn it is
display_turn(1, Player1Name-human, _) :-
  format('It is ~w\'s turn!~n', [Player1Name]).
display_turn(1, Player1Name-Player1Level, _) :-
  format('It is ~w\'s turn! (Difficulty: ~w)~n', [Player1Name, Player1Level]).
display_turn(2, _, Player2Name-human) :-
  format('It is ~w\'s turn!~n', [Player2Name]).
display_turn(2, _, Player2Name-Player2Level) :-
  format('It is ~w\'s turn! (Difficulty: ~w)~n', [Player2Name, Player2Level]).


% display_turtle_to_move(+Turtles)
%% Display the turtles that can be moved
display_turtle_to_move(Turtles) :-
  write_ln('Choose the turtle to move:'),
  display_list(Turtles).

% translate_turtle(+Turtle, -Code)
%% Translates the turtle's color and number to a displayable atom
translate_turtle(Color-Number, Code) :-
  sub_atom(Color, 0, 1, _, FirstLower),
  char_code(FirstLower, CodeLower),
  CodeUpper is CodeLower - 32,
  char_code(FirstUpper, CodeUpper),
  number_chars(Number, NumChars),
  atom_chars(NumAtom, NumChars),
  atom_concat(FirstUpper, NumAtom, Code).
