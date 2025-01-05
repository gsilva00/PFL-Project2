:- consult(io).
:- consult(display).
:- consult(auxiliary).

:- use_module(library(random)).


% Turtles
%% Represented as pair Color-Number
%% Color: Black and White
%% Number: 1, 2, 3, 4, 5
%% - Represents the strength, size and weight of the turtle (SSW)


% play/0
%% Starts the game (main predicate)
play :-
  banner,
  choose_gamemode(Gamemode),
  choose_difficulty(Gamemode, Player1Level, Player2Level),
  choose_players_names(Gamemode, Player1Name, Player2Name),
  choose_first_player(Gamemode, Player1Name, Player2Name, FirstPlayerChoice),
  choose_board_size(Width, Length),
  initial_state(game_config(Width, Length, Player1Name-Player1Level, Player2Name-Player2Level, FirstPlayerChoice), GameState),
  game_loop(GameState).



% CONFIGURING THE GAME

%% choose_gamemode(-Gamemode)
%% Prompts the user to choose a gamemode
choose_gamemode(Gamemode) :-
  display_gamemode_menu,
  get_menu_choice_ln('Option', 1, 5, Choice),
  gamemode(Choice, Gamemode),
  handle_gamemode(Gamemode).

%% gamemode(+Choice, -Gamemode)
%% Auxiliary predicate to choose_gamemode/1
%% Translates the inputted choice to the respective gamemode
gamemode(1, hh).
gamemode(2, hc).
gamemode(3, ch).
gamemode(4, cc).
gamemode(5, exit).

%% handle_gamemode(+Gamemode)
%% Auxiliary predicate to choose_gamemode/1
%% Handles the exit option
handle_gamemode(exit) :- halt.
handle_gamemode(_).


%% choose_players_names(+Gamemode, -Player1Name, -Player2Name)
%% Prompts the user to choose the players' names (if not a computer)
choose_players_names(hh, Player1Name, Player2Name) :-
  get_string_ln('Choose Player 1\'s name (white turtles)', Player1Name),
  get_string_ln('Choose Player 2\'s name (black turtles)', Player2Name).
choose_players_names(hc, Player1Name, 'Computer') :-
  get_string_ln('Choose Player\'s name (white turtles)', Player1Name).
choose_players_names(ch, 'Computer', Player2Name) :-
  get_string_ln('Choose Player\'s name (black turtles)', Player2Name).
choose_players_names(cc, 'Computer 1', 'Computer 2').


%% choose_first_player(+Gamemode, +Player1Name, +Player2Name, -Choice)
%% Prompts the user to choose the player that plays first
choose_first_player(hh, Player1Name, Player2Name, Choice) :-
  display_first_player_menu(Player1Name, Player2Name),
  get_menu_choice_ln('Option', 1, 2, Choice).
choose_first_player(hc, Player1Name, 'Computer', 1).
choose_first_player(ch, 'Computer', Player2Name, 1).
choose_first_player(cc, 'Computer 1', 'Computer 2', 1).


%% choose_difficulty(+Gamemode, -Player1Level, -Player2Level)
%% Prompts the user to choose the difficulty level for the computer player(s)
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

%% difficulty_level(+Level, -LevelName)
%% Auxiliary predicate to choose_difficulty/2
%% Translates the inputted level to the respective level name (atom)
difficulty_level(0, human).
difficulty_level(1, easy).
difficulty_level(2, hard).


%% choose_board_size(-Width, -Length)
%% Prompts the user to choose the board size
choose_board_size(Width, Length) :-
  display_board_width_menu,
  get_menu_choice_ln('Option', 2, 6, Width),
  display_board_length_menu,
  get_menu_choice_ln('Option', 4, 8, Length).



% GAME LOGIC

% initial_state(+GameConfig, -GameState)
%% Initializes the game state based on the game configuration
%% GameConfig is represented by the compound term - game_config(Width, Length, Player1Name-Player1Level, Player2Name-Player2Level, FirstPlayerChoice).
%% GameState is represented by the compound term - game_state(Turn, Nest1-Nest2, Board, Scored1-Scored2, Player1Name-Player1Level, Player2Name-Player2Level).
initial_state(game_config(Width, Length, Player1Name-Player1Level, Player2Name-Player2Level, Choice), game_state(Choice, [white-1,white-2,white-3,white-4,white-5]-[black-1,black-2,black-3,black-4,black-5], Board, []-[], Player1Name-Player1Level, Player2Name-Player2Level)) :-
  init_board(Width, Length, [], Board),
  format('Game started with ~w vs. ~w!~n', [Player1Name, Player2Name]).


% game_loop(+GameState)
%% Main game loop
%% GameState is represented by the compound term - game_state(Turn, Nest1-Nest2, Board, Scored1-Scored2, Player1Name-Player1Level, Player2Name-Player2Level).
game_loop(GameState) :-
  game_over(GameState, WinnerName),
  !,
  display_result(WinnerName).
game_loop(game_state(Turn, Nest1-Nest2, Board, Scored1-Scored2, Player1Name-Player1Level, Player2Name-Player2Level)) :-
  display_game(game_state(Turn, Nest1-Nest2, Board, Scored1-Scored2, Player1Name-Player1Level, Player2Name-Player2Level)),
  nth1(Turn, [Player1Level, Player2Level], CurrPlayerLevel),
  choose_move(game_state(Turn, Nest1-Nest2, Board, Scored1-Scored2, Player1Name-Player1Level, Player2Name-Player2Level), CurrPlayerLevel, Turtle-Direction),
  move(game_state(Turn, Nest1-Nest2, Board, Scored1-Scored2, Player1Name-Player1Level, Player2Name-Player2Level), Turtle-Direction, NewGameState),
  !,
  game_loop(NewGameState).


% game_over(+GameState, -Winner)
%% Check if the game is over
%% GameState is represented by the compound term - game_state(Turn, Nest1-Nest2, Board, Scored1-Scored2, Player1Name-Player1Level, Player2Name-Player2Level).
%% When there are no valid moves for the current player, and the number of scored turtles is equal, the game ends in a draw
%% When there are no valid moves for the current player, the player who has scored more turtles wins
game_over(game_state(Turn, Nest1-Nest2, Board, Scored1-Scored2, Player1Name-Player1Level, Player2Name-Player2Level), 'Draw') :-
  valid_moves(game_state(Turn, Nest1-Nest2, Board, Scored1-Scored2, Player1Name-Player1Level, Player2Name-Player2Level), []),
  length(Scored1, Length1),
  length(Scored2, Length1),
  !.
game_over(game_state(Turn, Nest1-Nest2, Board, Scored1-Scored2, Player1Name-Player1Level, Player2Name-Player2Level), Player1Name) :-
  valid_moves(game_state(Turn, Nest1-Nest2, Board, Scored1-Scored2, Player1Name-Player1Level, Player2Name-Player2Level), []),
  length(Scored1, Length1),
  length(Scored2, Length2),
  Length1 > Length2,
  !.
game_over(game_state(Turn, Nest1-Nest2, Board, Scored1-Scored2, Player1Name-Player1Level, Player2Name-Player2Level), Player2Name) :-
  valid_moves(game_state(Turn, Nest1-Nest2, Board, Scored1-Scored2, Player1Name-Player1Level, Player2Name-Player2Level), []),
  length(Scored1, Length1),
  length(Scored2, Length2),
  Length1 < Length2,
  !.

%% When a player has 3 turtles in the scored area, he wins
game_over(game_state(_, _-_, _, Scored1-Scored2, Player1Name-_, _-_), Player1Name) :-
  length(Scored1, Length),
  Length >= 3,
  !.
game_over(game_state(_, _-_, _, Scored1-Scored2, _-_, Player2Name-_), Player2Name) :-
  length(Scored2, Length),
  Length >= 3,
  !.

% choose_move(+GameState, +Level, -Move)
%% Choose human player's move
%% GameState is represented by the compound term - game_state(Turn, Nest1-Nest2, Board, Scored1-Scored2, Player1Name-Player1Level, Player2Name-Player2Level).
choose_move(game_state(Turn, Nest1-Nest2, Board, Scored1-Scored2, Player1Name-Player1Level, Player2Name-Player2Level), human, Turtle-Direction) :-
  valid_moves(game_state(Turn, Nest1-Nest2, Board, Scored1-Scored2, Player1Name-Player1Level, Player2Name-Player2Level), ListOfMoves),
  board_sizes(Board, _, Length),
  display_moves(ListOfMoves,Length),
  length(ListOfMoves, NumOfMoves),
  get_menu_choice_ln('Option', 1, NumOfMoves, Input),
  nth1(Input, ListOfMoves, Turtle-Direction),
  !.
%% Choose easy-computer player's move
choose_move(GameState, easy, Turtle-Direction) :-
  valid_moves(GameState, ListOfMoves),
  random_member(Turtle-Direction, ListOfMoves),
  !.
%% Choose hard-computer player's move
choose_move(game_state(Turn, Nest1-Nest2, Board, Scored1-Scored2, Player1Name-Player1Level, Player2Name-Player2Level), hard, Turtle-Direction) :-
  valid_moves(game_state(Turn, Nest1-Nest2, Board, Scored1-Scored2, Player1Name-Player1Level, Player2Name-Player2Level), ListOfMoves),
  nth1(Turn, [Player1Name-Player1Level, Player2Name-Player2Level], Player),
  setof(Value-Mv, NewState^(
    member(Mv, ListOfMoves),
    move(GameState, Mv, NewState),
    value(NewState, Player, Value)
  ), MovesValues),
  reverse(MovesValues, RevMovesValues),
  select_random_best(RevMovesValues, Turtle-Direction),
  !.


% valid_moves(+GameState, -ListOfMoves)
%% Find all valid moves for the current player
%% GameState is represented by the compound term - game_state(Turn, Nest1-Nest2, Board, Scored1-Scored2, Player1Name-Player1Level, Player2Name-Player2Level).
valid_moves(GameState, ListOfMoves) :-
  findall(Turtle-Direction, valid_move(GameState, Turtle-Direction), ListOfMoves).

% valid_move(+GameState, -Move)
%% Valid hatching moves (for turtles in the nests)
%% GameState is represented by the compound term - game_state(Turn, Nest1-Nest2, Board, Scored1-Scored2, Player1Name-Player1Level, Player2Name-Player2Level).
valid_move(game_state(Turn, Nest1-Nest2, Board, _-_, _-_, _-_), Turtle-Direction) :-
  nth1(Turn, [Nest1, Nest2], CurrNest),
  member(Turtle, CurrNest),
  board_sizes(Board, Width, _),
  move_directions(hatch, Width, Direction),
  valid_hatch(Turn, Board, Turtle-Direction).
%% Valid normal moves (for turtles on the board - not in the nests or scored)
%% GameState is represented by the compound term - game_state(Turn, Nest1-Nest2, Board, Scored1-Scored2, Player1Name-Player1Level, Player2Name-Player2Level).
valid_move(game_state(Turn, Nest1-Nest2, Board, Scored1-Scored2, _-_, _-_), (Color-Number)-Direction) :-
  nth1(Turn, [Nest1, Nest2], CurrNest),
  nth1(Turn, [Scored1, Scored2], CurrScored),
  nth1(Turn, [white, black], Color), % Get the color of the current player, to check if the turtle on the board is his
  turtles_on_board(Board, TurtlesOnBoard),
  member(Color-Number, TurtlesOnBoard),
  \+member(Color-Number, CurrNest),
  \+member(Color-Number, CurrScored),
  move_directions(normal, _, Direction),
  valid_normal(Board, (Color-Number)-Direction).


% valid_hatch(+Turn, +Board, +Move)
%% Check if the hatching cell is empty
valid_hatch(Turn, Board, _-(hatch-ColIdx)) :-
  board_sizes(Board, _, Length),
  nth1(Turn, [Length, 1], RowIdx), % Hatching row is the row closest to the player/his nest (Bottom row for white, top row for black)
  cell_empty(Board, RowIdx, ColIdx),
  !.
%% Check if the hatching cell has turtle(s) and is climbable
%% - Turtle is lighter than top of the stack and IS ABLE to climb on top of it
valid_hatch(Turn, Board, Turtle-(hatch-ColIdx)) :-
  board_sizes(Board, _, Length),
  nth1(Turn, [Length, 1], RowIdx),
  cell_can_climb(Board, RowIdx, ColIdx, [Turtle]),
  !.
%% Check if the hatching cell has turtle(s) and is pushable
%% - Turtle is stronger than the stack and IS ABLE to push it
valid_hatch(Turn, Board, Turtle-(hatch-ColIdx)) :-
  board_sizes(Board, _, Length),
  nth1(Turn, [Length, 1], RowIdx),
  cell_can_push(Board, RowIdx, ColIdx, [Turtle]),
  !.
%% Check if the hatching cell has turtle(s) and is climbable and pushable
%% - Turtle to hatch is lighter than some turtle in the stack and IS ABLE to climb on top of it, pushing what's above off the stack
valid_hatch(Turn, Board, Turtle-(hatch-ColIdx)) :-
  board_sizes(Board, _, Length),
  nth1(Turn, [Length, 1], RowIdx),
  cell_can_climb_push(Board, RowIdx, ColIdx, [Turtle]),
  !.


% valid_normal(+Board, +Move)
%% Check if it is a scoring move
valid_normal(Board, (Color-Number)-Direction) :-
  find_stack_to_move(Board, Color-Number, RowIdx, ColIdx, TurtleStack),
  stack_can_move(Board, RowIdx, ColIdx, TurtleStack),
  dest_coords(RowIdx, ColIdx, Direction, DestRowIdx, DestColIdx),
  valid_coords(Board, DestRowIdx, DestColIdx, Color),
  cell_can_score(Board, DestRowIdx, DestColIdx, Color),
  !.
%% Check if the normal cell is empty
%% - Turtle can move to the destination coords (empty cell)
valid_normal(Board, (Color-Number)-Direction) :-
  find_stack_to_move(Board, Color-Number, RowIdx, ColIdx, TurtleStack),
  stack_can_move(Board, RowIdx, ColIdx, TurtleStack),
  dest_coords(RowIdx, ColIdx, Direction, DestRowIdx, DestColIdx),
  valid_coords(Board, DestRowIdx, DestColIdx, Color),
  cell_empty(Board, DestRowIdx, DestColIdx),
  !.
%% Check if the normal cell has turtle(s) and is climbable
%% - Turtle can move to the destination coords, is lighter than the top of the stack in that cell and IS ABLE to climb on top of it
valid_normal(Board, (Color-Number)-Direction) :-
  find_stack_to_move(Board, Color-Number, RowIdx, ColIdx, TurtleStack),
  stack_can_move(Board, RowIdx, ColIdx, TurtleStack),
  dest_coords(RowIdx, ColIdx, Direction, DestRowIdx, DestColIdx),
  valid_coords(Board, DestRowIdx, DestColIdx, Color),
  cell_can_climb(Board, DestRowIdx, DestColIdx, TurtleStack),
  !.
%% Check if the normal cell has turtle(s) and is pushable
%% - Turtle can move to the destination coords, is stronger than the stack in that cell and IS ABLE to push it
valid_normal(Board, (Color-Number)-Direction) :-
  find_stack_to_move(Board, Color-Number, RowIdx, ColIdx, TurtleStack),
  stack_can_move(Board, RowIdx, ColIdx, TurtleStack),
  dest_coords(RowIdx, ColIdx, Direction, DestRowIdx, DestColIdx),
  valid_coords(Board, DestRowIdx, DestColIdx, Color),
  cell_can_push(Board, DestRowIdx, DestColIdx, TurtleStack),
  !.
%% Check if the normal cell has turtle(s) and is climbable and pushable
%% - Turtle can move to the destination coords, is lighter than some turtle in the stack in that cell and IS ABLE to climb on top of it, pushing what's above that turtle off the stack
valid_normal(Board, (Color-Number)-Direction) :-
  find_stack_to_move(Board, Color-Number, RowIdx, ColIdx, TurtleStack),
  stack_can_move(Board, RowIdx, ColIdx, TurtleStack),
  dest_coords(RowIdx, ColIdx, Direction, DestRowIdx, DestColIdx),
  valid_coords(Board, DestRowIdx, DestColIdx, Color),
  cell_can_climb_push(Board, DestRowIdx, DestColIdx, TurtleStack),
  !.


%% move_directions(+MoveType, +BoardWidth, -DirectionAtom)
%%% Wrapper to translate the move type to all possible direction atoms
move_directions(hatch, Width, Direction) :-
  hatching_directions(Width, Direction).
move_directions(normal, _, Direction) :-
  normal_directions(Direction).

%% hatching_directions(+Width, -Direction)
%%% Direction pair hatch-n, where n is the n-th Column (starting on 1) in the closest row to the nest
hatching_directions(Width, (hatch-ColIdx)) :-
  between(1, Width, ColIdx).
%% direction(+DirectionNumber, -Direction)
%%% 1 - up, 2 - down, 3 - left, 4 - right
normal_directions(up).
normal_directions(down).
normal_directions(left).
normal_directions(right).



% move(+GameState, +Move, -NewGameState)
%% Make a hatching move
%% Move is already validated because it was chosen from the list of valid moves. Still, most of the validations are repeated in move_hatch/11
%% GameState and NewGameState are represented by the compound term - game_state(Turn, Nest1-Nest2, Board, Scored1-Scored2, Player1Name-Player1Level, Player2Name-Player2Level).
move(game_state(Turn, Nest1-Nest2, Board, Scored1-Scored2, Player1Name-Player1Level, Player2Name-Player2Level), Turtle-(hatch-ColIdx), game_state(NewTurn, NewNest1-NewNest2, NewBoard, NewScored1-NewScored2, Player1Name-Player1Level, Player2Name-Player2Level)) :-
  board_sizes(Board, Width, Length),
  nth1(Turn, [Length, 1], RowIdx),
  move_hatch(false, Turn, Nest1-Nest2, Board, Scored1-Scored2, RowIdx, ColIdx, [Turtle], NewBoard, UpdatedNest1-UpdatedNest2, NewScored1-NewScored2),
  remove_from_nest(Turn, UpdatedNest1-UpdatedNest2, Turtle, NewNest1-NewNest2),
  NewTurn is Turn rem 2 + 1, % Change turn
  !.
%% Make a normal move
%% Move is already validated because it was chosen from the list of valid moves. Still, most of the validations are repeated in move_normal/12
move(game_state(Turn, Nest1-Nest2, Board, Scored1-Scored2, Player1Name-Player1Level, Player2Name-Player2Level), Turtle-Direction, game_state(NewTurn, NewNest1-NewNest2, NewBoard, NewScored1-NewScored2, Player1Name-Player1Level, Player2Name-Player2Level)) :-
  find_stack_to_move(Board, Turtle, RowIdx, ColIdx, TurtleStack),
  move_normal(false, Turn, Nest1-Nest2, Board, Scored1-Scored2, RowIdx, ColIdx, Direction, Turtle, TurtleStack, NewBoard, NewNest1-NewNest2, NewScored1-NewScored2),
  NewTurn is Turn rem 2 + 1,
  !.


% move_hatch(+IsChainReaction, +Turn, +(Nest1-Nest2), +Board, +(Scored1-Scored2), +RowIndex, +ColumnIndex, +TurtleStack, -NewBoard, -(UpdatedNest1-UpdatedNest2), -(NewScored1-NewScored2))
%% (RowIdx, ColIdx) - Coordinates of the hatching cell (destination)
%% Handle chain reactions from pushing turtles
%% No more turtles to push (no more chain reactions from pushing)
move_hatch(true, _, Nest1-Nest2, _, Scored1-Scored2, _, _, [], _, Nest1-Nest2, Scored1-Scored2).
%% Handle chain reactions from pushing turtles
%% Move previously displaced TurtleStack off the board to the sides - return turtles to the nests and stop chain reaction
%% Falling off the sides or the turtle's own end is allowed only for chain reactions from pushing turtles - these are the only moves that fail X coordinate validation
move_hatch(true, _, Nest1-Nest2, Board, Scored1-Scored2, RowIdx, ColIdx, Direction, TurtleStack, NewBoard, NewNest1-NewNest2, NewScored1-NewScored2) :-
  last(TurtleStack, Color-Number),
  \+valid_x(Board, RowIdx, Color),
  move_outside_board(sides, Color, Board, RowIdx, ColIdx, Color-Number, TurtleStack, Nest1-Nest2, Scored1-Scored2, NewNest1-NewNest2, NewBoard, NewScored1-NewScored2),
  !.
%% Handle chain reactions from pushing turtles
%% Move previously displaced TurtleStack off the board to the end with the base turtle's own color - return turtles to respective nests/scored and stop chain reaction
%% Falling off the turtle's own end is allowed only for chain reactions from pushing turtles - these are the only moves that fail Y coordinate validation
move_hatch(true, _, Nest1-Nest2, Board, Scored1-Scored2, RowIdx, ColIdx, Direction, TurtleStack, NewBoard, NewNest1-NewNest2, NewScored1-NewScored2) :-
  last(TurtleStack, Color-Number),
  \+valid_y(Board, ColIdx, Color),
  move_outside_board(ends, Color, Board, RowIdx, ColIdx, Color-Number, TurtleStack, Nest1-Nest2, Scored1-Scored2, NewNest1-NewNest2, NewBoard, NewScored1-NewScored2),
  !.
%% Handle chain reactions from pushing turtles
%% Move previously displaced TurtleStack because it is able to move the opposite end of the board (scoring move) - return turtles to the respective nests/scored and stop chain reaction
move_hatch(true, Turn, Nest1-Nest2, Board, Scored1-Scored2, RowIdx, ColIdx, TurtleStack, NewBoard, UpdatedNest1-UpdatedNest2, NewScored1-NewScored2) :-
  last(TurtleStack, Color-Number),         % Get the color of the base turtle in the stack
  cell_can_score(Board, RowIdx, ColIdx, Color),
  opposite_color(Color, OppositeColor), % If the turtle is able to score, it is on the opposite end of the board
  move_outside_board(ends, OppositeColor, UpdatedBoard, RowIdx, ColIdx, Color-Number, TurtleStack, Nest1-Nest2, Scored1-Scored2, UpdatedNest1-UpdatedNest2, NewBoard, NewScored1-NewScored2),
  !.
%% Move the turtle to the hatching cell (empty cell)
%% OR Move previously displaced TurtleStack because it is able to move to the empty cell - continue chain reaction
%% For chain reaction handling see move_hatch(true, ...) predicates
move_hatch(_, Turn, Nest1-Nest2, Board, Scored1-Scored2, RowIdx, ColIdx, TurtleStack, NewBoard, Nest1-Nest2 , Scored1-Scored2) :-
  cell_empty(Board, RowIdx, ColIdx),
  move_empty(Board, RowIdx, ColIdx, TurtleStack, NewBoard),
  !.
%% Move the turtle to the hatching cell (occupied cell - climb top of stack)
%% OR Move previously displaced TurtleStack because it is able to climb the top of the next turtle stack - continue chain reaction
move_hatch(_, Turn, Nest1-Nest2, Board, Scored1-Scored2, RowIdx, ColIdx, TurtleStack, NewBoard, Nest1-Nest2 , Scored1-Scored2) :-
  cell_can_climb(Board, RowIdx, ColIdx, TurtleStack),
  move_climb(Board, RowIdx, ColIdx, TurtleStack, NewBoard),
  !.
%% Move the turtle to the hatching cell (occupied cell - push stack)
%% OR Move previously displaced TurtleStack because it is able to push next turtle stack - continue chain reaction
move_hatch(_, Turn, Nest1-Nest2, Board, Scored1-Scored2, RowIdx, ColIdx, TurtleStack, NewBoard, UpdatedNest1-UpdatedNest2, NewScored1-NewScored2) :-
  cell_can_push(Board, RowIdx, ColIdx, TurtleStack),
  move_push(Board, RowIdx, ColIdx, TurtleStack, UpdatedBoard, DisplacedTurtleStack),
  !,
  UpMove is RowIdx - 1,
  DownMove is RowIdx + 1,
  nth1(Turn, [UpMove, DownMove], NewRowIdx),
  move_hatch(true, Turn, Nest1-Nest2, UpdatedBoard, Scored1-Scored2, NewRowIdx, ColIdx, DisplacedTurtleStack, NewBoard, UpdatedNest1-UpdatedNest2, NewScored1-NewScored2).
%% Move the turtle to the hatching cell (occupied cell - climb and push stack)
%% OR Move previously displaced TurtleStack because it is able to climb and push one of the turtles in the next turtle stack - continue chain reaction
move_hatch(_, Turn, Nest1-Nest2, Board, Scored1-Scored2, RowIdx, ColIdx, TurtleStack, NewBoard, UpdatedNest1-UpdatedNest2, NewScored1-NewScored2) :-
  cell_can_climb_push(Board, RowIdx, ColIdx, TurtleStack),
  move_climb_push(Board, RowIdx, ColIdx, TurtleStack, UpdatedBoard, DisplacedTurtleStack),
  !,
  UpMove is RowIdx - 1,
  DownMove is RowIdx + 1,
  nth1(Turn, [UpMove, DownMove], NewRowIdx),
  move_hatch(true, Turn, Nest1-Nest2, UpdatedBoard, Scored1-Scored2, NewRowIdx, ColIdx, DisplacedTurtleStack, NewBoard, UpdatedNest1-UpdatedNest2, NewScored1-NewScored2).
%% Handle chain reactions from pushing turtles
%% Previously displaced TurtleStack is not able to climb, push, or climb and push the next turtle - return turtles to the nests and stop chain reaction
move_hatch(true, _, Nest1-Nest2, Board, Scored1-Scored2, RowIdx, ColIdx, TurtleStack, Board, UpdatedNest1-UpdatedNest2, _-_) :-
  \+cell_empty(Board, RowIdx, ColIdx),
  \+cell_can_climb(Board, RowIdx, ColIdx, TurtleStack),
  \+cell_can_push(Board, RowIdx, ColIdx, TurtleStack),
  \+cell_can_climb_push(Board, RowIdx, ColIdx, TurtleStack),
  add_to_lists(Nest1-Nest2, TurtleStack, UpdatedNest1-UpdatedNest2),
  !.


% move_normal(+IsChainReaction, +Turn, +(Nest1-Nest2), +Board, +(Scored1-Scored2), +RowIndex, +ColumnIndex, +Direction, +Turtle, +TurtleStack, -NewBoard, -(NewNest1-NewNest2), -(NewScored1-NewScored2))
%% (RowIdx, ColIdx) - Coordinates of the starting cell, where TurtleStack is located (source)
%% Handle chain reactions from pushing turtles
%% No more turtles to push (no more chain reactions from pushing)
move_normal(true, _, Nest1-Nest2, _, Scored1-Scored2, _, _, _, _, [], _, Nest1-Nest2, Scored1-Scored2).
%% Handle chain reactions from pushing turtles
%% Move TurtleStack off the board to the sides - return turtles to the nests and stop chain reaction
%% Falling off the sides or the turtle's own end is allowed only for chain reactions from pushing turtles - these are the only moves that fail X coordinate validation
move_normal(true, _, Nest1-Nest2, Board, Scored1-Scored2, RowIdx, ColIdx, Direction, Turtle, TurtleStack, NewBoard, NewNest1-NewNest2, NewScored1-NewScored2) :-
  last(TurtleStack, (Color-_)),
  dest_coords(RowIdx, ColIdx, Direction, DestRowIdx, DestColIdx),
  \+valid_x(Board, DestRowIdx, Color),
  move_outside_board(sides, Color, Board, RowIdx, ColIdx, Turtle, TurtleStack, Nest1-Nest2, Scored1-Scored2, NewNest1-NewNest2, NewBoard, NewScored1-NewScored2),
  !.
%% Handle chain reactions from pushing turtles
%% Move DisplacedTurtleStack off the board to the end with the base turtle's own color - return turtles to respective nests/scored and stop chain reaction
%% Falling off the turtle's own end is allowed only for chain reactions from pushing turtles - these are the only moves that fail Y coordinate validation
move_normal(true, _, Nest1-Nest2, Board, Scored1-Scored2, RowIdx, ColIdx, Direction, Turtle, TurtleStack, NewBoard, NewNest1-NewNest2, NewScored1-NewScored2) :-
  last(TurtleStack, (Color-_)),
  dest_coords(RowIdx, ColIdx, Direction, DestRowIdx, DestColIdx),
  \+valid_y(Board, DestColIdx, Color),
  move_outside_board(ends, Color, Board, RowIdx, ColIdx, Turtle, TurtleStack, Nest1-Nest2, Scored1-Scored2, NewNest1-NewNest2, NewBoard, NewScored1-NewScored2),
  !.
%% Move TurtleStack, making a scoring move
%% OR Move previously displaced TurtleStack because it is able to make a scoring move - return turtles to the respective nests/scored and stop chain reaction
move_normal(_, Turn, Nest1-Nest2, Board, Scored1-Scored2, RowIdx, ColIdx, Direction, Turtle, TurtleStack, NewBoard, NewNest1-NewNest2, NewScored1-NewScored2) :-
  last(TurtleStack, (Color-_)),
  dest_coords(RowIdx, ColIdx, Direction, DestRowIdx, DestColIdx),
  cell_can_score(Board, DestRowIdx, DestColIdx, Color),
  opposite_color(Color, OppositeColor),
  move_outside_board(ends, OppositeColor, Board, RowIdx, ColIdx, Turtle, TurtleStack, Nest1-Nest2, Scored1-Scored2, NewNest1-NewNest2, NewBoard, NewScored1-NewScored2),
  !.
%% Move TurtleStack to the normal cell (empty cell)
%% OR Move previously displaced TurtleStack because it is able to move to the empty cell - continue chain reaction
move_normal(_, Turn, Nest1-Nest2, Board, Scored1-Scored2, RowIdx, ColIdx, Direction, Turtle, TurtleStack, NewBoard, Nest1-Nest2, Scored1-Scored2) :-
  dest_coords(RowIdx, ColIdx, Direction, DestRowIdx, DestColIdx),
  cell_empty(Board, DestRowIdx, DestColIdx),
  move_empty(Board, RowIdx, ColIdx, DestRowIdx, DestColIdx, Turtle, TurtleStack, NewBoard),
  !.
%% Move TurtleStack to the normal cell (occupied cell - climb top of stack)
%% OR Move previously displaced TurtleStack because it is able to climb the top of next turtle stack - continue chain reaction
move_normal(_, Turn, Nest1-Nest2, Board, Scored1-Scored2, RowIdx, ColIdx, Direction, Turtle, TurtleStack, NewBoard, Nest1-Nest2, Scored1-Scored2) :-
  dest_coords(RowIdx, ColIdx, Direction, DestRowIdx, DestColIdx),
  cell_can_climb(Board, DestRowIdx, DestColIdx, TurtleStack),
  move_climb(Board, RowIdx, ColIdx, DestRowIdx, DestColIdx, Turtle, TurtleStack, NewBoard),
  !.
%% Move TurtleStack to the normal cell (occupied cell - push stack)
%% OR Move previously displaced TurtleStack because it is able to push the next turtle stack - continue chain reaction
move_normal(_, Turn, Nest1-Nest2, Board, Scored1-Scored2, RowIdx, ColIdx, Direction, Turtle, TurtleStack, NewBoard, NewNest1-NewNest2, NewScored1-NewScored2) :-
  dest_coords(RowIdx, ColIdx, Direction, DestRowIdx, DestColIdx),
  cell_can_push(Board, DestRowIdx, DestColIdx, TurtleStack),
  move_push(Board, RowIdx, ColIdx, DestRowIdx, DestColIdx, Turtle, TurtleStack, UpdatedBoard, DisplacedTurtleStack),
  !,
  % Initial DisplacedTurtleStack coords are TurtleStack's destination coord. Direction of chain reactions is the same as the initial move
  last(DisplacedTurtleStack, NewBaseTurtle),
  move_normal(true, Turn, Nest1-Nest2, UpdatedBoard, Scored1-Scored2, DestRowIdx, DestColIdx, NewBaseTurtle, DisplacedTurtleStack, NewBoard, NewNest1-NewNest2, NewScored1-NewScored2).
%% Move TurtleStack to the normal cell (occupied cell - climb and push stack)
%% OR Move previously displaced TurtleStack because it is able to climb and push one of the turtles in the next turtle stack - continue chain reaction
move_normal(_, Turn, Nest1-Nest2, Board, Scored1-Scored2, RowIdx, ColIdx, Direction, Turtle, TurtleStack, NewBoard, NewNest1-NewNest2, NewScored1-NewScored2) :-
  dest_coords(RowIdx, ColIdx, Direction, DestRowIdx, DestColIdx),
  cell_can_climb_push(Board, DestRowIdx, DestColIdx, TurtleStack),
  move_climb_push(Board, RowIdx, ColIdx, DestRowIdx, DestColIdx, BaseTurtle, Turtle, TurtleStack, UpdatedBoard, DisplacedTurtleStack),
  !,
  last(DisplacedTurtleStack, NewBaseTurtle),
  move_normal(true, Turn, Nest1-Nest2, UpdatedBoard, Scored1-Scored2, DestRowIdx, DestColIdx, NewBaseTurtle, DisplacedTurtleStack, NewBoard, NewNest1-NewNest2, NewScored1-NewScored2).
%% Handles chain reactions from pushing turtles
%% DisplacedTurtleStack is not able to climb, push, or climb and push the next turtle - return turtles to the nests and stop chain reaction
move_normal(true, Nest1-Nest2, Board, Scored1-Scored2, RowIdx, ColIdx, Direction, _, TurtleStack, NewBoard, NewNest1-NewNest2, NewScored1-NewScored2) :-
  add_to_lists(Nest1-Nest2, TurtleStack, NewNest1-NewNest2),
  !.


% value(+GameState, +Player, -Value)
%% Obtains the Value of the current GameState based on the Player
%% GameState is represented by the compound term - game_state(Turn, Nest1-Nest2, Board, Scored1-Scored2, Player1Name-Player1Level, Player2Name-Player2Level).
%% Player is a compoud term - PlayerName-PlayerLevel
%% Value is a float number - Value = (NumTurtlesScoredPlayer - NumTurtlesScoredOther) + 0.5*(NumTurtlesAboutScore-OtherNumTurtlesAboutScore) + 0.1*(TotalBoardTurtles - OtherTotalBoardTurtles)
value(game_state(_, Nest1-Nest2, Board, Scored1-Scored2, Player1Name-Player1Level, Player2Name-Player2Level), PlayerXName-PlayerXLevel, Value):-
  obtain_player_number(PlayerXName, Player1Name, Player2Name, PlayerNum, OtherPlayerNum),
  nth1(PlayerNum, [Scored1, Scored2], ScoreList),
  length(ScoreList, NumTurtlesScoredPlayer),
  nth1(OtherPlayerNum, [Scored1, Scored2], OtherScoreList),
  length(OtherScoreList, NumTurtlesScoredOther),
  DiffOfScore is NumTurtlesScoredPlayer - NumTurtlesScoredOther,
  turtles_about_to_score(Board, PlayerNum, NumTurtlesAboutScore),
  turtles_about_to_score(Board, OtherPlayerNum, OtherNumTurtlesAboutScore),
  DiffTurtlesToScore is 0.5 * (NumTurtlesAboutScore - OtherNumTurtlesAboutScore),
  get_board_turtles_of(Board, PlayerNum, TotalBoardTurtles),
  get_board_turtles_of(Board, OtherPlayerNum, OtherTotalBoardTurtles),
  DiffTurtlesBoard is 0.1 * (TotalBoardTurtles - OtherTotalBoardTurtles),
  Value is DiffOfScore + DiffTurtlesToScore + DiffTurtlesBoard.


% obtain_player_number(+PlayerName, +FirstPlayerName, +SecondPlayerName, -NumberOfPlayer, -NumberOfOtherPlayer)
%% Gets the corresponding player number (1 or 2) and its oponent's number 
obtain_player_number(Player1Name, Player1Name, Player2Name, 1, 2).
obtain_player_number(Player2Name, Player1Name, Player2Name, 2, 1).

% turtles_about_to_score(+Board, +NumberOfPlayer, -NumTurtlesAboutScore)
%% Returns the total number of the player's turtles that are on the oponent's border.
turtles_about_to_score(Board, PlayerNum, NumTurtlesAboutScore):-
  length(Board, BoardLen),
  nth1(PlayerNum, [1, BoardLen], RowIdx),
  nth1(RowIdx, Board, Row),
  append(Row, FlatRow),
  count_turtles(FlatRow, PlayerNum, [], NumTurtlesAboutScore).


% count_turtles(+ListOfTurtles, +NumberOfPlayer, +Acc, -TotalOfTurtles)
%% Counts the number of the player's turtles that are on the list 
count_turtles([],_,Acc,Final):-
  length(Acc,Final),!.
count_turtles([(white-_)|Tail], 1, Acc, Final):-
  count_turtles(Tail, 1, [white|Acc], Final),!.
count_turtles([(white-_)|Tail], 2, Acc, Final):-
  count_turtles(Tail, 2, Acc, Final),!.
count_turtles([(black-_)|Tail], 2, Acc, Final):-
  count_turtles(Tail, 2, [black|Acc], Final),!.
count_turtles([(black-_)|Tail], 1, Acc, Final):-
  count_turtles(Tail, 1, Acc, Final),!.

% get_board_turtles_of(+Board, +NumberOfPlayer, -TotalBoardTurtlesBelongingToPlayer)
%% Obtain the total amount of turtles on the board that belong to the player
get_board_turtles_of(Board, PlayerNum, TotalBoardTurtles):-
  turtles_on_board(Board, TurtlesOnBoard),
  count_turtles(TurtlesOnBoard, PlayerNum, [], TotalBoardTurtles).