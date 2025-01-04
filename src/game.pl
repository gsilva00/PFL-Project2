:- consult(io).
:- consult(display).
:- consult(auxiliary).

:- use_module(library(random)).


config(Width, Length, Player1Level, Player2Level).
game_state(Turn, Nest1-Nest2, Board, Scored1-Scored2, Player1Name-Player1Level, Player2Name-Player2Level).
% Turtles
%% Represented as pair Color-Number
%% Color: Black and White
%% Number: 1, 2, 3, 4, 5
%% - Represents the strength, size and weight of the turtle (SSW)


% START

% play/0
%% Starts the game (main predicate)
play :-
  banner,
  choose_gamemode(Gamemode),
  choose_difficulty(Gamemode, Player1Level, Player2Level),
  choose_players_names(Gamemode, Player1Name, Player2Name),
  choose_first_player(Gamemode, Player1Name, Player2Name, Choice),
  choose_board_size(Width, Length),
  initial_state(config(Width, Length, Player1Name-Player1Level, Player2Name-Player2Level, Choice), game_state(Turn, Nest1-Nest2, Board, Scored1-Scored2, Player1Name-Player1Level, Player2Name-Player2Level)),
  game_loop(game_state(Turn, Nest1-Nest2, Board, Scored1-Scored2, Player1Name-Player1Level, Player2Name-Player2Level)).



% CONFIGURING THE GAME

% Gamemode
%% choose_gamemode(-Gamemode)
choose_gamemode(Gamemode) :-
  display_gamemode_menu,
  get_menu_choice_ln('Option', 1, 5, Choice),
  gamemode(Choice, Gamemode),
  handle_gamemode(Gamemode).

%% handle_gamemode(+Choice, -Gamemode)
gamemode(1, hh).
gamemode(2, hc).
gamemode(3, ch).
gamemode(4, cc).
gamemode(5, exit).

%% handle_gamemode(+Gamemode)
handle_gamemode(exit) :- halt.
handle_gamemode(_).


% Player Names
%% choose_players_names(+Gamemode, -Player1Name, -Player2Name)
choose_players_names(hh, Player1Name, Player2Name) :-
  get_string_ln('Choose Player 1\'s name (white turtles)', Player1Name),
  get_string_ln('Choose Player 2\'s name (black turtles)', Player2Name).
choose_players_names(hc, Player1Name, 'Computer') :-
  get_string_ln('Choose Player\'s name (white turtles)', Player1Name).
choose_players_names(ch, 'Computer', Player2Name) :-
  get_string_ln('Choose Player\'s name (black turtles)', Player2Name).
choose_players_names(cc, 'Computer 1', 'Computer 2').

% First Player
%% choose_first_player(+Gamemode, +Player1Name, +Player2Name, -Choice)
choose_first_player(hh, Player1Name, Player2Name, Choice) :-
  display_first_player_menu(Player1Name, Player2Name),
  get_menu_choice_ln('Option', 1, 2, Choice).
choose_first_player(hc, Player1Name, 'Computer', Player1Name).
choose_first_player(ch, 'Computer', Player2Name, Player2Name).
choose_first_player(cc, 'Computer 1', 'Computer 2', 'Computer 1').


% Difficulty
%% choose_difficulty(+Gamemode, -Player1Level, -Player2Level)
choose_difficulty(hh, Player1Level, Player2Level) :-
  difficulty_level(0, Player1Level),
  difficulty_level(0, Player2Level).
choose_difficulty(hc, Player1Level, Player2Level) :-
  display_difficulty_menu(1),
  get_menu_choice_ln('Option', 1, 2, DifficultyChoice),
  difficulty_level(0, Player1Level),
  difficulty_level(DifficultyChoice, Player2Level).
choose_difficulty(ch, Player1Level, Player2Level) :-
  display_difficulty_menu(1),
  get_menu_choice_ln('Option', 1, 2, DifficultyChoice),
  difficulty_level(DifficultyChoice, Player1Level),
  difficulty_level(0, Player2Level).
choose_difficulty(cc, Player1Level, Player2Level) :-
  display_difficulty_menu(1),
  get_menu_choice_ln('Option', 1, 2, DifficultyChoice1),
  difficulty_level(DifficultyChoice1, Player1Level),
  display_difficulty_menu(2),
  get_menu_choice_ln('Option', 1, 2, DifficultyChoice2),
  difficulty_level(DifficultyChoice2, Player2Level).

%% Difficulty Levels
%%% difficulty_level(+Level, -LevelName)
%%% - 0: Human, 1: Easy, 2: Hard
difficulty_level(0, human).
difficulty_level(1, easy).
difficulty_level(2, hard).


% Board Size
%% choose_board_size(-Width, -Length)
choose_board_size(Width, Length) :-
  display_board_width_menu,
  get_menu_choice_ln('Option', 2, 6, Width),
  display_board_length_menu,
  get_menu_choice_ln('Option', 4, 8, Length).



% GAME LOGIC

% initial_state(+GameConfig, -GameState)
initial_state(config(Width, Length, Player1Name-Player1Level, Player2Name-Player2Level, Choice), game_state(Choice, [white-1,white-2,white-3,white-4,white-5]-[black-1,black-2,black-3,black-4,black-5], Board, []-[], Player1Name-Player1Level, Player2Name-Player2Level)):-
  init_board(Width, Length, Board),
  format('Game started with ~w vs. ~w!~n', [Player1Name, Player2Name]).


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


% game_loop(+GameState)
game_loop(game_state(Turn, Nest1-Nest2, Board, Scored1-Scored2, Player1Name-Player1Level, Player2Name-Player2Level)) :-
  game_over(game_state(Turn, Nest1-Nest2, Board, Scored1-Scored2, Player1Name-Player1Level, Player2Name-Player2Level), WinnerName), !,
  display_winner(WinnerName).
game_loop(game_state(Turn, Nest1-Nest2, Board, Scored1-Scored2, Player1Name-Player1Level, Player2Name-Player2Level)) :-
  display_game(game_state(Turn, Nest1-Nest2, Board, Scored1-Scored2, Player1Name-Player1Level, Player2Name-Player2Level)),
  nth1(Turn, [Player1Level, Player2Level], CurrPlayerLevel),
  choose_move(game_state(Turn, Nest1-Nest2, Board, Scored1-Scored2, Player1Name-Player1Level, Player2Name-Player2Level), CurrPlayerLevel, Turtle-Direction),
  move(game_state(Turn, Nest1-Nest2, Board, Scored1-Scored2, Player1Name-Player1Level, Player2Name-Player2Level), Turtle-Direction, NewGameState),
  !,
  game_loop(NewGameState).


% game_over(+GameState, -WinnerName)
game_over(game_state(_, _-_, _, Scored1-Scored2, Player1Name-_, Player2Name-_), WinnerName) :-  
  length(Scored1, Len),
  length(Scored2, Len2),!,
  ((Len = 3 , WinnerName is Player1Name);
  (Len2 = 3, WinnerName is Player2Name)).

% choose_move(+GameState, +Level, -Move)
%% Choose human player's move
choose_move(game_state(Turn, Nest1-Nest2, Board, Scored1-Scored2, Player1Name-Player1Level, Player2Name-Player2Level), human, Turtle-Direction) :-
  valid_moves(game_state(Turn, Nest1-Nest2, Board, Scored1-Scored2, Player1Name-Player1Level, Player2Name-Player2Level), ListOfMoves),
  board_sizes(Board, _, Length),
  display_moves(ListOfMoves,Length),
  length(ListOfMoves, NumOfMoves),
  get_menu_choice_ln('Option', 1, NumOfMoves, Input),
  nth1(Input, ListOfMoves, Turtle-Direction), !.

% Choose easy computer player's move
choose_move(game_state(Turn, Nest1-Nest2, Board, Scored1-Scored2, Player1Name-Player1Level, Player2Name-Player2Level), easy, Turtle-Direction) :-
  valid_moves(game_state(Turn, Nest1-Nest2, Board, Scored1-Scored2, Player1Name-Player1Level, Player2Name-Player2Level), ListOfMoves),
  random_member(Turtle-Direction, ListOfMoves), !.

% Choose hard computer player's move
% choose_move(game_state(Turn, Nest1-Nest2, Board, Scored1-Scored2, Player1Name-Player1Level, Player2Name-Player2Level), hard, Turtle-Direction) :-
%  valid_moves(game_state(Turn, Nest1-Nest2, Board, Scored1-Scored2, Player1Name-Player1Level, Player2Name-Player2Level), ListOfMoves),
%  value(game_state(Turn, Nest1-Nest2, Board, Scored1-Scored2, Player1Name-Player1Level, Player2Name-Player2Level), Player2Name, Number),
%  nth1(Turn, [Player1Name, Player2Name], CurrPlayerName),
%  %% TODO: Implement minimax
%  minimax(game_state(Turn, Nest1-Nest2, Board, Scored1-Scored2, Player1Name-Player1Level, Player2Name-Player2Level), CurrPlayerName, Type, Number, Turtle-Direction).



%% move_direction(+MoveType, +BoardWidth, +MoveId, -DirectionAtom)
%%% Wrapper that translates the move id to a displayable/handlable atom
move_direction(normal, _, DirId, Direction) :-
  normal_direction(DirId, Direction).
move_direction(hatch, Width, ColNum, Direction) :-
  hatching_direction(Width, ColNum, Direction).

%% direction(+DirectionNumber, -Direction)
%%% 1 - up, 2 - down, 3 - left, 4 - right
normal_direction(1, up).
normal_direction(2, down).
normal_direction(3, left).
normal_direction(4, right).
%% hatching_direction(+Width, +ColumnNumber, -Direction)
%%% Direction pair hatch-n -> n-th Column (starting on 1) in the closest row to the nest
hatching_direction(Width, ColNum, (hatch-ColNum)) :-
  ColNum > 0,
  ColNum =< Width.

%% dir_displacement(+Direction, -RowDisplacement, -ColDisplacement)
%%% Translation of the direction atom to the row and column displacement
dir_displacement(up, -1, 0).
dir_displacement(down, 1, 0).
dir_displacement(left, 0, -1).
dir_displacement(right, 0, 1).

%% target_coords(+RowNumber, +ColumnNumber, +Direction, -DestinationRowNumber, -DestinationColumnNumber)
%%% Calculate the destination coordinates based on the direction
target_coords(RowNum, ColNum, Direction, DestRowNum, DestColNum) :-
  dir_displacement(Direction, RowDisplacement, ColDisplacement),
  DestRowNum is RowNum + RowDisplacement,
  DestColNum is ColNum + ColDisplacement.


% valid_moves(+GameState, -ListOfMoves)
valid_moves(GameState, ListOfMoves) :-
  findall(Turtle-Direction, valid_move(GameState, Turtle-Direction), ListOfMoves).

% valid_move(+GameState, -Move)
%% Valid hatching moves
valid_move(game_state(Turn, Nest1-Nest2, Board, _-_, _-_, _-_), (Color-Number)-Direction) :-
  nth1(Turn, [Nest1, Nest2], CurrNest),
  member((Color-Number), CurrNest),
  board_sizes(Board, Width, _),
  move_direction(hatch, Width, Number, Direction),
  valid_hatch(Turn, Board, (Color-Number)-Direction).

%% Valid normal moves
valid_move(game_state(Turn, Nest1-Nest2, Board, Scored1-Scored2, _-_, _-_), (Color-Number)-Direction) :-
  nth1(Turn, [Nest1, Nest2], CurrNest),
  nth1(Turn, [Scored1, Scored2], CurrScored),
  \+member((Color-Number), CurrNest),
  \+member((Color-Number), CurrScored),
  move_direction(normal, _, Number, Direction),
  valid_normal(Board, (Color-Number)-Direction).



% valid_hatch(+Turn, +Board, +Move)
%% Check if the hatching square is empty
valid_hatch(Turn, Board, _-(hatch-ColNum)) :-
  board_sizes(Board, _, Length),
  nth1(Turn, [1, Length], RowNum), % Hatching row is the row closest to the player/his nest
  cell_empty(Board, RowNum, ColNum),
  !.

%% Check if the hatching square has turtle(s) and is climbable
%% - Turtle is lighter than top of the stack and IS ABLE to climb on top of it
valid_hatch(Turn, Board, Turtle-(hatch-ColNum)) :-
  board_sizes(Board, _, Length),
  nth1(Turn, [1, Length], RowNum),
  cell_can_climb(Board, RowNum, ColNum, Turtle),
  !.

%% Check if the hatching square has turtle(s) and is pushable
%% - Turtle is stronger than the stack and IS ABLE to push it
valid_hatch(Turn, Board, Turtle-(hatch-ColNum)) :-
  board_sizes(Board, _, Length),
  nth1(Turn, [1, Length], RowNum),
  cell_can_push(Board, RowNum, ColNum, Turtle),
  !.

%% Check if the hatching square has turtle(s) and is climbable and pushable
%% - Turtle to hatch is lighter than some turtle in the stack and IS ABLE to climb on top of it, pushing what's above off the stack
valid_hatch(Turn, Board, Turtle-(hatch-ColNum)) :-
  board_sizes(Board, _, Length),
  nth1(Turn, [1, Length], RowNum),
  cell_can_climb_push(Board, RowNum, ColNum, Turtle),
  !.


%% valid_normal(+Board, +Move)
%% Check if the normal square is empty
valid_normal(Board, Turtle-Direction) :-
  find_turtle(Board, Turtle, RowNum, ColNum),
  can_move(Board, RowNum, ColNum, Turtle),
  target_coords(RowNum, ColNum, Direction, DestRowNum, DestColNum),
  cell_empty(Board, DestRowNum, DestColNum),
  !.

%% Check if the normal square has turtle(s) and is climbable
%% - Turtle can move, is lighter than top of the stack and IS ABLE to climb on top of it
valid_normal(Board, Turtle-Direction) :-
  find_turtle(Board, Turtle, RowNum, ColNum),
  can_move(Board, RowNum, ColNum, Turtle),
  target_coords(RowNum, ColNum, Direction, DestRowNum, DestColNum),
  cell_can_climb(Board, DestRowNum, DestColNum, Turtle),
  !.


%% Check if the normal square has turtle(s) and is pushable
%% - Turtle can move, is stronger than the stack and IS ABLE to push it
valid_normal(Board, Turtle-Direction) :-
  find_turtle(Board, Turtle, RowNum, ColNum),
  can_move(Board, RowNum, ColNum, Turtle),
  target_coords(RowNum, ColNum, Direction, DestRowNum, DestColNum),
  cell_can_push(Board, DestRowNum, DestColNum, Turtle),
  !.

%% Check if the normal square has turtle(s) and is climbable and pushable
%% - Turtle can move, is lighter than some turtle in the stack and IS ABLE to climb on top of it, pushing what's above off the stack
valid_normal(Board, Turtle-Direction) :-
  find_turtle(Board, Turtle, RowNum, ColNum),
  can_move(Board, RowNum, ColNum, Turtle),
  target_coords(RowNum, ColNum, Direction, DestRowNum, DestColNum),
  cell_can_climb_push(Board, DestRowNum, DestColNum, Turtle),
  !.


% move(+GameState, +Move, -NewGameState)
%% TODO
move(game_state(Turn, Nest1-Nest2, Board, Scored1-Scored2, Player1Name-Player1Level, Player2Name-Player2Level), Turtle-Direction, NewGameState).


  %Turn1 is Turn rem 2 + 1 % Change turn



% value(+GameState, +Player, -Value)
%% TODO
% value(GameState, Player, Value).

% minimax(+GameState, +Player, +Type, +Value, -Move)
%% TODO
% minimax(GameState, Player, Type, Value, Turtle-Direction).
