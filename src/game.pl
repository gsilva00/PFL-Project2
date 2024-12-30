:- consult(io).
:- consult(display).

:- use_module(library(random)).


config(Width, Length, Player1Level, Player2Level).
game_state(Turn, Nest1-Nest2, Board, Scored1-Scored2, Player1Name-Player1Level, Player2Name-Player2Level).


% START

% play/0
%% Starts the game (main predicate)
play :-
  banner,
  choose_gamemode(Gamemode),
  choose_difficulty(Gamemode, Player1Level-Player2Level),
  choose_players_names(Gamemode, Player1Name, Player2Name),
  choose_first_player(Gamemode, Player1Name, Player2Name, Turn),
  choose_board_size(Width, Length),
  initial_state(config(Width, Length, Player1Name-Player1Level, Player2Name-Player2Level, Choice), game_state(Turn, Nest1-Nest2, Board, Scored1-Scored2, Player1Name-Player1Level, Player2Name-Player2Level)),
  game_loop(game_state(Turn, Nest1-Nest2, Board, Scored1-Scored2, Player1Name-Player1Level, Player2Name-Player2Level)).



% CONFIGURING THE GAME

% Gamemode
choose_gamemode(Gamemode) :-
  display_gamemode_menu,
  get_menu_choice_ln('Option', 1, 5, Choice),
  gamemode(Choice, Gamemode),
  handle_gamemode(Gamemode).

gamemode(1, hh).
gamemode(2, hc).
gamemode(3, ch).
gamemode(4, cc).
gamemode(5, exit).

handle_gamemode(exit) :- halt.
handle_gamemode(_).


% Player Names
choose_players_names(hh, Player1Name, Player2Name) :-
  get_string_ln('Choose Player 1\'s name (white turtles)', Player1Name),
  get_string_ln('Choose Player 2\'s name (black turtles)', Player2Name).
choose_players_names(hc, Player1Name, 'Computer') :-
  get_string_ln('Choose Player\'s name (white turtles)', Player1Name).
choose_players_names(ch, 'Computer', Player2Name) :-
  get_string_ln('Choose Player\'s name (black turtles)', Player2Name).
choose_players_names(cc, 'Computer 1', 'Computer 2').

% First Player
choose_first_player(hh, Player1Name, Player2Name, Choice) :-
  display_first_player_menu(Player1Name, Player2Name),
  get_menu_choice_ln('Option', 1, 2, Choice).
choose_first_player(hc, Player1Name, 'Computer', Player1Name).
choose_first_player(ch, 'Computer', Player2Name, Player2Name).
choose_first_player(cc, 'Computer 1', 'Computer 2', 'Computer 1').


% Difficulty
choose_difficulty(hh, Player1Level-Player2Level) :-
  difficulty_level(0, Player1Level),
  difficulty_level(0, Player2Level).
choose_difficulty(hc, Player1Level-Player2Level) :-
  display_difficulty_menu(1),
  get_menu_choice_ln('Option', 1, 2, DifficultyChoice),
  difficulty_level(0, Player1Level),
  difficulty_level(DifficultyChoice, Player2Level).
choose_difficulty(ch, Player1Level-Player2Level) :-
  display_difficulty_menu(1),
  get_menu_choice_ln('Option', 1, 2, DifficultyChoice),
  difficulty_level(DifficultyChoice, Player1Level),
  difficulty_level(0, Player2Level).
choose_difficulty(cc, Player1Level-Player2Level) :-
  display_difficulty_menu(1),
  get_menu_choice_ln('Option', 1, 2, DifficultyChoice1),
  difficulty_level(DifficultyChoice1, Player1Level),
  display_difficulty_menu(2),
  get_menu_choice_ln('Option', 1, 2, DifficultyChoice2),
  difficulty_level(DifficultyChoice2, Player2Level).

%% Difficulty Levels
%%%  0 - Human, 1 - Easy, 2 - Hard
difficulty_level(0, human).
difficulty_level(1, easy).
difficulty_level(2, hard).


% Board Size
choose_board_size(Width, Length) :-
  display_board_width_menu,
  get_menu_choice_ln('Option', 2, 6, Width),
  display_board_length_menu,
  get_menu_choice_ln('Option', 4, 8, Length).



% GAME LOGIC

% initial_state(+GameConfig, -GameState)
initial_state(config(Width, Length, Player1Name-Player1Level, Player2Name-Player2Level, Choice), game_state(Choice, [[1],[2],[3],[4],[5]]-[[1],[2],[3],[4],[5]], Board, []-[], Player1Name-Player1Level, Player2Name-Player2Level)) :-
  init_board(Width, Length, Board),
  format('Game started with ~w vs. ~w!~n', [Player1, Player2]).

% init_board(+Width, +Length, -Board)
init_board(Width, Length, Board) :-
  length(Board, Length),
  maplist(init_row(Width), Board).
% init_row(+Width, -Row)
init_row(Width, Row) :-
  length(Row, Width),
  maplist(init_cell, Row).
% init_cell(-Cell)
init_cell([]).


% game_loop(+GameState)
game_loop(game_state(_, _-_, _, Scored1-Scored2, Player1Name-_, Player2Name-_)) :-
  game_over(game_state(_, _-_, _, Scored1-Scored2, Player1Name-_, Player2Name-_), WinnerName),
  !,
  display_winner(WinnerName).
game_loop(game_state(Turn, Nest1-Nest2, Board, Scored1-Scored2, Player1Name-Player1Level, Player2Name-Player2Level)) :-
  display_game(game_state(Turn, Nest1-Nest2, Board, Scored1-Scored2, Player1Name-Player1Level, Player2Name-Player2Level)),
  nth1(Turn, [Player1Level, Player2Level], CurrPlayerLevel),
  choose_move(game_state(Turn, Nest1-Nest2, Board, Scored1-Scored2, Player1Name-Player1Level, Player2Name-Player2Level), CurrPlayerLevel, Turtle-Direction),
  move(game_state(Turn, Nest1-Nest2, Board, Scored1-Scored2, Player1Name-Player1Level, Player2Name-Player2Level), Turtle-Direction, NewGameState),
  !,
  game_loop(NewGameState).


% game_over(+GameState, -WinnerName)
game_over(game_state(_, _-_, _, Scored1-Scored2, Player1Name-_, Player2Name-_), Player1Name) :-
  length(Scored1, 3).
game_over(game_state(_, _-_, _, Scored1-Scored2, Player1Name-_, Player2Name-_), Player2Name) :-
  length(Scored2, 3).

% display_winner(+WinnerName)
display_winner(WinnerName) :-
  format('Congratulations, ~w! You won!~n', [WinnerName]).

% display_game(+GameState)
display_game(game_state(Turn, Nest1-Nest2, Board, Scored1-Scored2, Player1Name-Player1Level, Player2Name-Player2Level)) :-
  display_board(Board),
  display_score(Scored1, Scored2),
  display_nests(Nest1, Nest2),
  display_turn(Turn, Player1Name, Player2Name).


% choose_move(+GameState, +Level, -Move)
%% Choose human player's move
choose_move(game_state(Turn, Nest1-Nest2, Board, Scored1-Scored2, Player1Name-_, Player2Name-_), human, Turtle-Direction) :-
  display_turn(Turn, Player1Name, Player2Name),
  valid_moves(game_state(_, Nest1-Nest2, Board, Scored1-Scored2, _-_, _-_), ListOfMoves),
  display_moves(ListOfMoves),
  get_playable_turtles(ListOfMoves, Turtles),
  display_turtle_to_move(Turtles),
  get_turtle_choice_ln('Option', Turtles, Turtle),
  display_direction_to_move,
  get_menu_choice_ln('Option', 1, 4, Input),
  direction(Input, Direction).

% Choose easy computer player's move
%% TODO
choose_move(game_state(Turn, Nest1-Nest2, Board, Scored1-Scored2, Player1Name-_, Player2Name-_), easy, Turtle-Direction) :-
  display_turn(Turn, Player1Name, Player2Name),
  valid_moves(game_state(_, Nest1-Nest2, Board, Scored1-Scored2, _-_, Player2Name-_), ListOfMoves),
  random_member(Turtle-Direction, ListOfMoves).

% Choose hard computer player's move
%% TODO
choose_move(game_state(Turn, Nest1-Nest2, Board, Scored1-Scored2, Player1Name-_, Player2Name-_), hard, Turtle-Direction) :-
  display_turn(Turn, Player1Name, Player2Name),
  valid_moves(game_state(_, Nest1-Nest2, Board, Scored1-Scored2, _-_, _-_), ListOfMoves),
  value(game_state(_, Nest1-Nest2, Board, Scored1-Scored2, _-_, _-_), Player2Name, Value),
  nth1(Turn, [Player1Name, Player2Name], CurrPlayerName),
  minimax(game_state(_, Nest1-Nest2, Board, Scored1-Scored2, _-_, _-_), CurrPlayerName, Type, Value, Turtle-Direction).

%% Move's direction
direction(1, up).
direction(2, down).
direction(3, left).
direction(4, right).

% get_playable_turtles(+ListOfMoves, -Turtles)
get_playable_turtles([], []).
get_playable_turtles([Turtle-Direction|T], [Turtle|Turtles]) :-
  get_playable_turtles(T, Turtles).


% move(+GameState, +Move, -NewGameState)
%% TODO
move(+GameState, +Turtle-Direction, -NewGameState).

% valid_moves(+GameState, -ListOfMoves)
valid_moves(GameState, ListOfMoves) :-
  findall(Turtle-Direction, valid_move(GameState, Turtle-Direction), ListOfMoves).

% value(+GameState, +Player, -Value)
%% TODO
value(GameState, Player, Value).

% minimax(+GameState, +Player, +Type, +Value, -Move)
%% TODO
minimax(GameState, Player, Type, Value, Turtle-Direction).
