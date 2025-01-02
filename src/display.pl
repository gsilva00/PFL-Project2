% MENUS

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

display_gamemode_menu :-
  write_ln('Choose your game mode'),
  write_ln('1. Human vs. Human (H/H)'),
  write_ln('2. Human vs. Computer (H/PC)'),
  write_ln('3. Computer vs. Human (PC/H)'),
  write_ln('4. Computer vs. Computer (PC/PC)'),
  write_ln('5. Exit').

display_first_player_menu(Player1Name, Player2Name) :-
  write('Choose who plays first: '), nl,
  format('1. ~w~n', [Player1Name]),
  format('2. ~w~n', [Player2Name]).

display_difficulty_menu(ComputerNum) :-
  format('Choose Computer ~d\'s difficulty level: ~n', [ComputerNum]),
  write_ln('1. Easy'), % AI plays random moves
  write_ln('2. Hard'). % AI plays the best move

display_board_width_menu :-
  write_ln('Turtles will be placed on a rectangular board. You can choose its size!')
  write_ln('Choose the board width: 2, 3, 4, 5 or 6').

display_board_length_menu :-
  write_ln('Choose the board length: 4, 5, 6, 7 or 8').



% GAME STATE

% display_winner(+Winner)
display_winner(Winner) :-
  format('Congratulations, ~w! You won!~n', [Winner]).


% display_game(+GameState)
display_game(game_state(Turn, Nest1-Nest2, Board, Scored1-Scored2, Player1Name-Player1Level, Player2Name-Player2Level)) :-
  display_board(Board),
  display_score(Player1Name, Player2Name, Scored1, Scored2),
  display_nests(Player1Name, Player2Name, Nest1, Nest2).

% display_board(+Board)
% TODO
display_board(Board).

% display_score(+Player1Name, +Player2Name, +Scored1, +Scored2)
display_score(Player1Name, Player2Name, Scored1, Scored2) :-
  length(Scored1, NumTurtles1),
  length(Scored2, NumTurtles2),
  format('Score:~n - ~w: ~d turtles scored~n - ~w: ~d turtles scored~n', [Player1Name, NumTurtles1, Player2Name, NumTurtles2]).

% display_nests(+Player1Name, +Player2Name, +Nest1, +Nest2)
display_nests(Player1Name, Player2Name, Nest1, Nest2) :-
  format('Player ~w\'s nest has turtles: ', [Player1Name]),
  display_list(Nest1),
  format('Player ~w\'s nest has turtles: ', [Player2Name]),
  display_list(Nest2).

% display_list(+List)
% Display a list of elements separated by a space (except for the last element)
display_list([]) :-
  writeln('None').
display_list([H|T]) :-
  translate_turtle(H, Code),
  format('Turtle ~w~n', [Code]),
  display_list(T).

% display_turn(+Turn, +Player1Name, +Player2Name)
display_turn(1, Player1Name-human, _) :-
  format('It is ~w\'s turn!~n', [Player1Name]).
display_turn(1, Player1Name-Player1Level, _) :-
  format('It is ~w\'s turn! (Difficulty: ~w)~n', [Player1Name, Player1Level]).
display_turn(2, _, Player2Name-human) :-
  format('It is ~w\'s turn!~n', [Player2Name]).
display_turn(2, _, Player2Name-Player2Level) :-
  format('It is ~w\'s turn! (Difficulty: ~w)~n', [Player2Name, Player2Level]).


% display_turtle_to_move
display_turtle_to_move(Turtles) :-
  write_ln('Choose the turtle to move:'),
  display_list(Turtles).

% display_direction_to_move
display_direction_to_move :-
  write_ln('Choose the direction to move:'),
  write_ln('1. Up'),
  write_ln('2. Down'),
  write_ln('3. Left'),
  write_ln('4. Right').
