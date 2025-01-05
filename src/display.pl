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
  write_ln('================================'),
  display_nests(Player1Name, Player2Name, Nest1, Nest2),
  display_board(Board),
  display_score(Player1Name, Player2Name, Scored1, Scored2),
  display_turn(Turn, Player1Name-Player1Level, Player2Name-Player2Level),
  write_ln('================================').

% add_1_ifnecessary(+Size, -Max)
add_1_ifnecessary(0,1):- !.
add_1_ifnecessary(Size,Size).

% display_board(+Board)
%% Display the board
display_board(Board):-
  length(Board, Rows),
  max_cell_size(Board,MaxSize),
  add_1_ifnecessary(MaxSize,Max),
  display_wall_initially(Board,Max),
  display_rows(Board, Rows, Max),
  nl.

% display_rows(+Board, +Rows, +MaxSize)
display_rows([], _, _).
display_rows([Row|Rest], Rows, MaxSize) :-
  display_row_initially(Row,Rows, MaxSize),
  write('   '),
  display_wall(Row, MaxSize),
  NewRows is Rows - 1,
  display_rows(Rest, NewRows, MaxSize).

% display_row_initially(+Row, +Rows, +MaxSize)
display_row_initially([],_,_):-!.
display_row_initially(Row,Rows,MaxSize):-
  format(' ~w',[Rows]),
  display_row(Row,Rows,MaxSize).

% display_row(+Row, +Rows, +MaxSize)
display_row([],_,_) :-
  write(' | '),
  nl,
  !.

display_row([Cell|Rest],Rows,MaxSize) :-
  write(' | '),
  length(Cell,Len),
  get_extra_space(cell,Len,MaxSize,NewMaxSize),
  get_extra_space(cell2,Len,MaxSize,NewMaxSize2),
  display_wall_aux(space,NewMaxSize),
  display_cell(Cell),
  display_wall_aux(space,NewMaxSize2),
  display_row(Rest,Rows,MaxSize).

% display_wall_initially(+Board, +MaxSize)
display_wall_initially([],_):-!.
display_wall_initially([Row|_],MaxSize):-
  write('   '),
  length(Row,Len),
  display_wall_number(Len,1, MaxSize),
  write('   '),
  display_wall(Row,MaxSize).

% display_wall_number(+RowLen, +Acc, +MaxSize)
display_wall_number(RowLen, Acc, _) :-
  Acc > RowLen, nl, !.
display_wall_number(RowLen,Acc, MaxSize) :-
  get_extra_space(title,MaxSize,NewMaxSize),
  display_wall_aux(space,NewMaxSize),
  format(' ~w ',[Acc]),
  display_wall_aux(space,NewMaxSize),
  Acc1 is Acc + 1,
  display_wall_number(RowLen,Acc1, MaxSize).

% display_wall(+Row, +MaxSize)
display_wall([],_) :-
  write('+'),
  nl, !.
display_wall([_|Rest],MaxSize) :-
  write('+-'),
  display_wall_aux(hifen,MaxSize),
  display_wall(Rest,MaxSize).

display_wall_aux(_,0).
display_wall_aux(hifen,MaxSize):-
  MaxSize >= 1,
  NewMaxSize is MaxSize - 1,
  write('---'),
  display_wall_aux(hifen,NewMaxSize).

display_wall_aux(space,MaxSize):-
  NewMaxSize is MaxSize - 1,
  write(' '),
  display_wall_aux(space,NewMaxSize).

% display_cell(+Cell)
display_cell(Cell) :-
  var(Cell), !,
  write('    ').
display_cell([]).
display_cell([Turtle1 | []]) :-
  translate_turtle(Turtle1, TurtleCode),
  write(TurtleCode),!.
display_cell([Turtle1 | Rest]) :-
  translate_turtle(Turtle1, TurtleCode),
  write(TurtleCode),
  write('/'),
  display_cell(Rest).

get_extra_space(title, 1, 1).
get_extra_space(title, 2, 3).
get_extra_space(title, 3, 4).
get_extra_space(title, 4, 6).
get_extra_space(title, 5, 7).

get_extra_space(cell, X, X, 0).
get_extra_space(cell, Y, X, 1) :-
  X is Y + 1.
get_extra_space(cell, Y, X, 3) :-
  X is Y + 2.
get_extra_space(cell, Y, X, 4) :-
  X is Y + 3.
get_extra_space(cell, 0, 4, 5).
get_extra_space(cell, 1, 5, 6).
get_extra_space(cell, 0, 5, 6).

get_extra_space(cell2, X, X, 0).
get_extra_space(cell2, 0, 1, 1).
get_extra_space(cell2, Y, X, 2) :-
  X is Y + 1.
get_extra_space(cell2, 0, 2, 2).
get_extra_space(cell2, Y, X, 3) :-
  X is Y + 2.
get_extra_space(cell2, 0, 3, 4).
get_extra_space(cell2, Y, X, 5) :-
  X is Y + 3.
get_extra_space(cell2, Y, X, 6) :-
  X is Y + 4.
get_extra_space(cell2, 0, 5, 8).

% display_score(+Player1Name, +Player2Name, +Scored1, +Scored2)
%% Display the score of each player
display_score(Player1Name, Player2Name, Scored1, Scored2) :-
  length(Scored1, NumTurtles1),
  length(Scored2, NumTurtles2),
  format('Score:~n - ~w: ~d turtles scored~n - ~w: ~d turtles scored~n~n', [Player1Name, NumTurtles1, Player2Name, NumTurtles2]).

% display_nests(+Player1Name, +Player2Name, +Nest1, +Nest2)
%% Display the turtles in each player's nest
display_nests(Player1Name, Player2Name, Nest1, Nest2) :-
  format('Player ~w\'s nest has turtles:~n', [Player1Name]),
  display_list(false, Nest1),
  nl,
  format('Player ~w\'s nest has turtles:~n', [Player2Name]),
  display_list(false, Nest2),
  nl,nl.

% display_moves(+ListOfMoves)
% Display a list of possible moves
display_moves(ListOfMoves, Length) :-
  write_ln('Possible moves:'),
  display_moves_aux(ListOfMoves, Length, 1).

% display_moves_aux(+ListOfMoves, +Index)
% Helper predicate to display moves with their corresponding index
display_moves_aux([], _, _):- write_ln('').

display_moves_aux([Turtle-Direction|Rest], Length, Index) :-
  translate_turtle(Turtle, TurtleCode),
  format('~d. ', [Index]),
  decide_movement(Turtle,TurtleCode,Direction, Length),
  NextIndex is Index + 1,
  display_moves_aux(Rest, Length, NextIndex).

decide_movement(Color-_,TurtleCode,hatch-ColNum, Length):-
  get_hatch_row(Color, Length, RowNum),
  format('Hatch turtle ~w to (~w,~w)~n', [TurtleCode, ColNum, RowNum ]), !.

decide_movement(TurtleCode, DirNum, _):-
  normal_direction(DirNum,Arrow),
  format('Move turtle ~w ~w~n', [TurtleCode, Arrow ]).

get_hatch_row(white, _,1).
get_hatch_row(black, Length, Length).


% display_list(+HasAtLeastOneElement, +List)
% Display a list of elements separated by a space (except for the last element)
display_list(true, []).
display_list(false, []) :-
  write_ln('None').
display_list(_, [H|T]) :-
  translate_turtle(H, Code),
  format(' - Turtle ~w~n', [Code]),
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
  display_list(false, Turtles).

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
