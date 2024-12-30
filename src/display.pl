% Menus
banner :-
  clear,
  writeln('================================'),
  writeln('  Welcome to the Turtles Game!  '),
  writeln('         _____     ____'),
  writeln('        /      \\  |  o |'),
  writeln('       |        |/ ___\\|'),
  writeln('       |_________/     '),
  writeln('       |_|_| |_|_|'),
  writeln('================================').

display_gamemode_menu :-
  writeln('Choose your game mode'),
  writeln('1. Human vs. Human (H/H)'),
  writeln('2. Human vs. Computer (H/PC)'),
  writeln('3. Computer vs. Human (PC/H)'),
  writeln('4. Computer vs. Computer (PC/PC)'),
  writeln('5. Exit').

display_first_player_menu(Player1Name, Player2Name) :-
  write('Choose who plays first: '), nl,
  format('1. ~w~n', [Player1Name]),
  format('2. ~w~n', [Player2Name]).

display_difficulty_menu(ComputerNum) :-
  format('Choose Computer ~d\'s difficulty level: ~n', [ComputerNum]),
  writeln('1. Easy'), % AI plays random moves
  writeln('2. Hard'). % AI plays the best move

display_board_width_menu :-
  writeln('Turtles will be placed on a rectangular board. You can choose its size!')
  writeln('Choose the board width: 2, 3, 4, 5 or 6').

display_board_length_menu :-
  writeln('Choose the board length: 4, 5, 6, 7 or 8').


% Game State
%% display_winner(+Winner)
display_winner(Winner) :-
  format('Congratulations, ~w! You won!~n', [Winner]).


%% display_game(+GameState)
display_game(game_state(Turn, Nest1-Nest2, Board, Scored1-Scored2, Player1Name-Player1Level, Player2Name-Player2Level)) :-
  display_board(Board),
  display_score(Player1Name, Player2Name, Scored1, Scored2),
  display_nests(Player1Name, Player2Name, Nest1, Nest2),
  display_turn(Turn, Player1Name, Player2Name).

%% display_board(+Board)
%% TODO
display_board(Board).

%% display_score(+Player1Name, +Player2Name, +Scored1, +Scored2)
display_score(Player1Name, Player2Name, Scored1, Scored2) :-
  length(Scored1, Turtle1Num),
  length(Scored2, Turtle2Num),
  format('Score:~n - ~w: ~d turtles scored~n - ~w: ~d turtles scored~n', [Player1Name, Turtle1Num, Player2Name, Turtle2Num]).

%% display_nests(+Player1Name, +Player2Name, +Nest1, +Nest2)
display_nests(Player1Name, Player2Name, Nest1, Nest2) :-
  format('Player ~w\'s nest has turtles: ', [Player1Name]),
  display_list(Nest1),
  nl,
  format('Player ~w\'s nest has turtles: ', [Player2Name]),
  display_list(Nest2),
  nl.

%% display_list(+List)
display_list([]) :-
  writeln('None').
display_list([H|T]) :-
  format('~w ', [H]),
  display_list(T).

%% display_turn(+Turn, +Player1Name, +Player2Name)
display_turn(1, Player1Name, _) :-
  format('It is ~w\'s turn!~n', [Player1Name]).
display_turn(2, _, Player2Name) :-
  format('It is ~w\'s turn!~n', [Player2Name]).