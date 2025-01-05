% TURTLE MANIPULATION

%% 1. Stack Logic
%%% Stack is represented as a list of elements (list operations in Prolog are efficient and exactly what we need)

% push(+Element, +Stack, -NewStack)
%% Add an element to the top of the stack
push(Elem, Stack, [Elem|Stack]).

% pop(+Stack, -TopElement, -NewStack)
%% Get the top element of the stack and remove it
pop([Top|Rest], Top, Rest).

% top(+Stack, -TopElement)
%% Get the top element of the stack without removing it
top([Top|_], Top).

% split_stack(?Stack, ?Element, ?StackAbove, ?StackBelow)
%% Split the stack at the specified element
split_stack(Stack, Elem, Above, Below) :-
  append(Above, [Elem|Below], Stack).

% push_stack(+Stack, +DestinationStack, -NewDestinationStack)
%% Push the stack to the destination stack
push_stack(Stack, DestStack, NewDestStack) :-
  append(Stack, DestStack, NewDestStack).


%% 2. Move Logic

% combined_weight(+TurtleStack, -Weight)
%% Calculate the weight of the turtle stack
%% Use of accumulator for the increased efficiency of tail recursion
combined_weight(TurtleStack, Weight) :-
  combined_weight_aux(TurtleStack, 0, Weight).

% combined_weight_aux(+TurtleStack, +Acc, -Weight)
%% Auxiliary predicate for combined_weight/2
combined_weight_aux([], Acc, Acc).
combined_weight_aux([(_-Number)|Rest], Acc, Weight) :-
  NewAcc is Acc + Number,
  combined_weight_aux(Rest, NewAcc, Weight).


% split_stack_by_color(+Stack, +Color, -SameColorStack, -OtherColorStack)
%% Splits the stack into two stacks: one with turtles of the given Color and one with turtles of the other color
split_stack_by_color([], _, [], []).
split_stack_by_color([(Color-Number)|Rest], Color, [(Color-Number)|SameColorStack], OtherColorStack) :-
  split_stack_by_color(Rest, Color, SameColorStack, OtherColorStack).
split_stack_by_color([(OtherColor-Number)|Rest], Color, SameColorStack, [(OtherColor-Number)|OtherColorStack]) :-
  Color \= OtherColor,
  split_stack_by_color(Rest, Color, SameColorStack, OtherColorStack).



% BOARD MANIPULATION

%% 1. Board Logic

% init_board(+Width, +Length, +Content, -Board)
%% Initialize the board with the specified dimensions and cell content
%% In the case of our game, the Content is a stack of turtles
init_board(Width, Length, Content, Board) :-
  length(Board, Length),
  maplist(init_row(Width, Content), Board).
% init_row(+Width, +Content, -Row)
%% Initialize a row with the specified width and cell content
init_row(Width, Content, Row) :-
  length(Row, Width),
  maplist(init_cell(Content), Row).
% init_cell(+Content, -Cell)
%% Initialize a cell with the specified content
init_cell(Content, Content).

% board_sizes(+Board, -Width, -Length)
%% Get the width and length of the board
board_sizes(Board, Width, Length) :-
  length(Board, Length),
  nth1(1, Board, Row),
  length(Row, Width).

% max_cell_size(+Board, -MaxSize)
%% Get the maximum size of all the cells on the board
max_cell_size(Board, MaxSize) :-
  findall(Size, (member(Row, Board), member(Cell, Row), length(Cell, Size)), Sizes),
  max_member(MaxSize, Sizes).

% cell_at(+Board, +RowIndex, +ColumnIndex, ?Content)
%% Return the content at the specified cell or check if the cell is empty (depending of the presence of Content)
%% In the case of our game, the Content is a stack of turtles
cell_at(Board, RowIdx, ColIdx, Content) :-
  nth1(RowIdx, Board, Row),
  nth1(ColIdx, Row, Content).

% set_cell(+Board, +RowIndex, +ColumnIndex, +Content, -NewBoard)
%% Set the content at the specified cell in the board
%% In the case of our game, the Content is a stack of turtles
set_cell(Board, RowIdx, ColIdx, Content, NewBoard) :-
  nth1(RowIdx, Board, Row, RestBoard),
  nth1(ColIdx, Row, _, RestRow),
  nth1(ColIdx, NewRow, Content, RestRow),
  nth1(RowIdx, NewBoard, NewRow, RestBoard).

% cell_empty(+Board, +RowIndex, +ColumnIndex)
%% Check if the cell is an empty stack
cell_empty(Board, RowIdx, ColIdx) :-
  cell_at(Board, RowIdx, ColIdx, []). % Empty stack is represented by an empty list


% valid_coords(+Board, +RowIndex, +ColumnIndex, +TurtleColor)
%% Check if the coordinates are valid for hatching and normal moves
%% - Y is valid depending on the turtle's color (scoring is out-of-bounds different for each player)
valid_coords(Board, RowIdx, ColIdx, Color) :-
  valid_x(Board, ColIdx),
  valid_y(Board, RowIdx, Color).

% valid_x(+Board, +ColumnNumber)
%% Check if the X coordinate is within the board
valid_x(Board, ColIdx) :-
  board_sizes(Board, Width, _),
  between(1, Width, ColIdx).

% valid_y(+Board, +RowIndex, +TurtleColor)
%% Check if the Y coordinate is within the board
%% White turtles score when they reach above the top row (RowIdx 0) - black turtles' side
valid_y(Board, RowIdx, white) :-
  board_sizes(Board, _, Length),
  between(0, Length, RowIdx).
%% Black turtles score when they reach below the bottom row (RowIdx Length+1) - white turtles' side
valid_y(Board, RowIdx, black) :-
  board_sizes(Board, _, Length),
  BelowLength is Length+1,
  between(1, BelowLength, RowIdx).



%% 2. Move Logic

%%% 2.1. Miscellaneous

% find_stack_to_move(+Board, +Turtle, -RowIdx, -ColIdx, -TurtleStack)
%% Find the turtle on the board and return the stack of turtles above it, including the turtle itself - stack-to-move
%% Return the position of the TurtleStack on the board
find_stack_to_move(Board, (Color-Number), RowIdx, ColIdx, TurtleStack) :-
  find_turtle(Board, (Color-Number), RowIdx, ColIdx),
  cell_at(Board, RowIdx, ColIdx, TurtleStackInCell),
  split_stack(TurtleStackInCell, (Color-Number), StackAbove, _),
  append(StackAbove, [(Color-Number)], TurtleStack).

% find_turtle(+Board, +Turtle, -RowIdx, -ColIdx)
%% Return the position of the turtle on the board
find_turtle(Board, Turtle, RowIdx, ColIdx) :-
  board_sizes(Board, Width, Length),
  between(1, Length, RowIdx),
  between(1, Width, ColIdx),
  turtle_in_cell(Board, RowIdx, ColIdx, Turtle).

% turtles_on_board(+Board, +TurtlesOnBoard)
%% Return all the turtles on the board
turtles_on_board(Board, TurtlesOnBoard) :-
  findall(Turtle,(
    member(Row, Board),
    member(TurtleStack, Row),
    member(Turtle, TurtleStack)
  ), TurtlesOnBoard).

% turtle_in_cell(+Board, +Turtle, +RowIndex, +ColumnIndex)
%% Check if the turtle is in the specified cell
turtle_in_cell(Board, RowIdx, ColIdx, Turtle) :-
  cell_at(Board, RowIdx, ColIdx, TurtleStack),
  member(Turtle, TurtleStack).


% dest_coords(+RowIndexber, +ColumnNumber, +Direction, -DestinationRowIdxber, -DestinationColumnNumber)
%% Calculate the destination coordinates based on the direction of a normal move
dest_coords(RowIdx, ColIdx, Direction, DestRowIdx, DestColIdx) :-
  dir_displacement(Direction, RowDisplacement, ColDisplacement),
  DestRowIdx is RowIdx + RowDisplacement,
  DestColIdx is ColIdx + ColDisplacement.

%% dir_displacement(+Direction, -RowDisplacement, -ColDisplacement)
%%% Translate the direction atom to the row and column displacement
dir_displacement(up, -1, 0).
dir_displacement(down, 1, 0).
dir_displacement(left, 0, -1).
dir_displacement(right, 0, 1).


% opposite_color(+Color, -OppositeColor)
%% Get the opposite color of the given color
opposite_color(white, black).
opposite_color(black, white).

%% max(+X, +Y, -Max)
%% Get the maximum of two numbers
max(X, Y, X) :-
  X >= Y,
  !.
max(_, Y, Y).


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
  count_turtles(Tail, 1, [white|Acc], Final),
  !.
count_turtles([(white-_)|Tail], 2, Acc, Final):-
  count_turtles(Tail, 2, Acc, Final),
  !.
count_turtles([(black-_)|Tail], 2, Acc, Final):-
  count_turtles(Tail, 2, [black|Acc], Final),
  !.
count_turtles([(black-_)|Tail], 1, Acc, Final):-
  count_turtles(Tail, 1, Acc, Final),
  !.

% get_board_turtles_of(+Board, +NumberOfPlayer, -TotalBoardTurtlesBelongingToPlayer)
%% Obtain the total amount of turtles on the board that belong to the player
get_board_turtles_of(Board, PlayerNum, TotalBoardTurtles):-
  turtles_on_board(Board, TurtlesOnBoard),
  count_turtles(TurtlesOnBoard, PlayerNum, [], TotalBoardTurtles).


% select_random_best(+MovesWithValues, -BestMove)
%% Selects a random move among those with the highest value
select_random_best([Value-Move|Rest], BestMove) :-
  highest_val_moves([Value-Move|Rest], BestMoves),
  random_member(BestMove, BestMoves).

% highest_val_moves(+MovesWithValues, -HighestValMoves)
%% Selects all moves with the highest value
highest_val_moves([Value-Move|Rest], HighestValMoves) :-
  highest_val_moves(Rest, Value, [Move], HighestValMoves).
% highest_val_moves_aux(+MovesWithValues, +Value, +Acc, -HighestValMoves)
%% Auxiliary predicate for highest_val_moves/2
highest_val_moves_aux([], _, Acc, Acc).
highest_val_moves_aux([Value-Move|Rest], Value, Acc, HighestValMoves) :-
  !,
  highest_val_moves_aux(Rest, Value, [Move|Acc], HighestValMoves).
highest_val_moves_aux([NewValue-_|Rest], Value, Acc, HighestValMoves) :-
  NewValue < Value,
  !,
  highest_val_moves_aux(Rest, Value, Acc, HighestValMoves).
highest_val_moves_aux([NewValue-NewMove|Rest], _, _Acc, HighestValMoves) :-
  NewValue > Value,
  highest_val_moves_aux(Rest, NewValue, [NewMove], HighestValMoves),
  !.


% add_to_lists(+(List1-List2), +TurtleStack, -(NewList1-NewList2))
%% Given the state's lists, adds each turtle in the stack to the respective list
add_to_lists(List1-List2, [], List1-List2).
add_to_lists(List1-List2, [(white-Number)|Rest], NewList1-NewList2) :-
  append(List1, [(white-Number)], NewTempList1),
  add_to_lists(NewTempList1-List2, Rest, NewList1-NewList2).
add_to_lists(List1-List2, [(black-Number)|Rest], NewList1-NewList2) :-
  append(List2, [(black-Number)], NewTempList2),
  add_to_lists(List1-NewTempList2, Rest, NewList1-NewList2).


% remove_from_nest(+Turn, +(Nest1-Nest2), +Turtle, -(NewNest1-NewNest2))
%% Remove the Turtle from the respective nest (upon hatching move)
remove_from_nest(1, Nest1-Nest2, Turtle, NewNest1-Nest2) :-
  select(Turtle, Nest1, NewNest1).
remove_from_nest(2, Nest1-Nest2, Turtle, Nest1-NewNest2) :-
  select(Turtle, Nest2, NewNest2).


%%% 2.2. Validating Moves (Scoring, To Empty, Climb, Push, Climb and Push)

% stack_can_move(+Board, +RowIndex, +ColumnIndex, +TurtleStack)
%% Check if the TurtleStack can move to the specified cell (base turtle is strong enough to carry the turtles above it)
stack_can_move(Board, RowIdx, ColIdx, TurtleStack) :-
  last(AboveTurtle, (Color-Number), TurtleStack), % Base turtle is the last turtle in the stack
  combined_weight(AboveTurtle, Weight),
  Number > Weight.


% cell_can_score(+Board, +RowIndex, +ColumnIndex, +TurtleColor)
%% Check if the move is a scoring move
cell_can_score(Board, 0, ColIdx, white).
cell_can_score(Board, RowIdx, ColIdx, black) :-
  board_sizes(Board, _, Length),
  RowIdx is Length+1.


% cell_can_climb(+Board, +RowIndex, +ColumnIndex, +TurtleStack)
%% Check if the TurtleStack can climb the top turtle of the stack at the specified (destination) cell
%% True if the base of TurtleStack is lighter than the top turtle of the stack in the cell
cell_can_climb(Board, RowIdx, ColIdx, TurtleStack) :-
  last(TurtleStack, BaseTurtle),
  cell_at(Board, RowIdx, ColIdx, [TopTurtle|_]),
  turtle_can_climb(BaseTurtle, TopTurtle).

% turtle_can_climb(+Turtle, +TargetTurtle)
%% Check if the turtle can climb the target turtle
turtle_can_climb((_-Number), (_-TargetNumber)) :-
  Number < TargetNumber.


% cell_can_push(+Board, +RowIndex, +ColumnIndex, +TurtleStack)
%% Check if the TurtleStack can push the stack at the specified (destination) cell
%% True if the base of TurtleStack is stronger than the weight of the stack in the cell
cell_can_push(Board, RowIdx, ColIdx, TurtleStack) :-
  last(TurtleStack, BaseTurtle),
  cell_at(Board, RowIdx, ColIdx, DestStack),
  turtle_can_push(BaseTurtle, DestStack).

% turtle_can_push(+Turtle, +TargetTurtleStack)
%% Check if the Turtle can push the TargetTurtleStack
%% True if the combined weight of the target stack (base turtle included) is less than the Turtle
turtle_can_push((_-Number), TurtleStack) :-
  combined_weight(TurtleStack, Weight),
  Number >= Weight.


% cell_can_climb_push(+Board, +RowIndex, +ColumnIndex, +TurtleStack)
%% Check if the TurtleStack can climb the stack at the specified (destination) cell and push what's above it
cell_can_climb_push(Board, RowIdx, ColIdx, TurtleStack) :-
  last(TurtleStack, BaseTurtle),
  cell_at(Board, RowIdx, ColIdx, DestStack),
  stack_can_climb_push(BaseTurtle, DestStack, [], _).

% stack_can_climb_push(+Turtle, +Stack, +CheckedStack, -DisplacedTurtleStack)
%% <<<Follows "resolving movement methodically" section of the game rules>>>
%% Check if any turtle in the stack is climbable and pushable (from the top to the bottom) by Turtle
%% CheckedStack contains the stack that has been checked so far
%% - i.e. the stack above the current turtle being checked - useful for weight calculation (for pushing)
%% - when a turtle is found that can be climbed and the turtles above it can be pushed, the stack of displaced turtles is returned
%% - when it is empty, the displaced stack (CheckedStack) is the initial stack being checked and is returned in DisplacedTurtleStack if the Turtle can push it
stack_can_climb_push(Turtle, [], CheckedStack, CheckedStack) :-
  turtle_can_push(Turtle, CheckedStack),
  !.
stack_can_climb_push(Turtle, [TopTurtle|Rest], CheckedStack, CheckedStack) :-
  turtle_can_climb(Turtle, TopTurtle),
  turtle_can_push(Turtle, CheckedStack),
  !.
stack_can_climb_push(Turtle, [TopTurtle|Rest], CheckedStack, _) :-
  stack_can_climb_push(Turtle, Rest, [TopTurtle|CheckedStack], _).


%%% 2.3. Moving Turtles (Scoring, To Empty, Climb, Push, Climb and Push)

% move_empty(+Board, +InitialRowIndex, +InitialColumnIndex, +DestinationRowIndex, +DestinationColumnIndex, +BaseTurtle, +TurtleStack, -NewBoard)
%% Move the stack above and including the BaseTurtle (TurtleStack) to an empty cell with coordinates (RowIdx, ColIdx)
%% Remove the moving stack from the initial position's cell
%% Return the new board state
move_empty(Board, InitRowIdx, InitColIdx, DestRowIdx, DestColIdx, BaseTurtle, TurtleStack, NewBoard) :-
  cell_at(Board, InitRowIdx, InitColIdx, FullTurtleStack),        % Whole Stack in the initial position (includes the moving stack)
  append(TurtleStack, StackBelow, FullTurtleStack),               % Get the stack below the moving stack
  set_cell(Board, InitRowIdx, InitColIdx, StackBelow, TempBoard), % Stack below remains in the cell
  move_empty(TempBoard, DestRowIdx, DestColIdx, TurtleStack, NewBoard).
% move_empty(+Board, +RowIndex, +ColumnIndex, +TurtleStack, -NewBoard)
%% Move the stack above and including the BaseTurtle (TurtleStack) to an empty cell with coordinates (RowIdx, ColIdx)
%% Without clearing the initial position's cell (for hatch moves and 2nd part of normal moves)
%% Return the new board state
move_empty(Board, RowIdx, ColIdx, TurtleStack, NewBoard) :-
  set_cell(Board, RowIdx, ColIdx, TurtleStack, NewBoard).

% move_climb(+Board, +InitialRowIndex, +InitialColumnIndex, +DestinationRowIndex, +DestinationColumnIndex, +BaseTurtle, +TurtleStack, -NewBoard)
%% Move the stack above and including the BaseTurtle (TurtleStack) to a cell with coordinates (RowIdx, ColIdx) with a stack that can be climbed
%% Remove the moving stack from the initial position's cell
%% Return the new board state
move_climb(Board, InitRowIdx, InitColIdx, DestRowIdx, DestColIdx, BaseTurtle, TurtleStack, NewBoard) :-
  cell_at(Board, InitRowIdx, InitColIdx, FullTurtleStack),
  append(TurtleStack, StackBelow, FullTurtleStack),
  set_cell(Board, InitRowIdx, InitColIdx, StackBelow, TempBoard),
  move_climb(TempBoard, DestRowIdx, DestColIdx, TurtleStack, NewBoard).
% move_climb(+Board, +RowIndex, +ColumnIndex, +TurtleStack, -NewBoard)
%% Move the stack above and including the BaseTurtle (TurtleStack) to a cell with coordinates (RowIdx, ColIdx) with a stack that can be climbed
%% Without clearing the initial position's cell (for hatch moves and 2nd part of normal moves)
%% Return the new board state
move_climb(Board, RowIdx, ColIdx, TurtleStack, NewBoard) :-
  cell_at(Board, RowIdx, ColIdx, DestStack),
  push_stack(TurtleStack, DestStack, NewStack),
  set_cell(Board, RowIdx, ColIdx, NewStack, NewBoard).

% move_push(+Board, +InitialRowIndex, +InitialColumnIndex, +DestinationRowIndex, +DestinationColumnIndex, +BaseTurtle, +TurtleStack, -NewBoard, -DisplacedTurtleStack)
%% Move the stack above and including the BaseTurtle (TurtleStack) to a cell with coordinates (RowIdx, ColIdx) with a stack that can be pushed
%% Remove the moving stack from the initial position's cell
%% Return the new board state and the stack of turtles that were displaced
move_push(Board, InitRowIdx, InitColIdx, DestRowIdx, DestColIdx, BaseTurtle, TurtleStack, NewBoard, DisplacedTurtleStack) :-
  cell_at(Board, InitRowIdx, InitColIdx, FullTurtleStack),
  append(TurtleStack, StackBelow, FullTurtleStack),
  set_cell(Board, InitRowIdx, InitColIdx, StackBelow, TempBoard),
  move_push(TempBoard, DestRowIdx, DestColIdx, TurtleStack, NewBoard, DisplacedTurtleStack).
% move_push(+Board, +RowIndex, +ColumnIndex, +TurtleStack, -NewBoard, -DisplacedTurtleStack)
%% Move the stack above and including the BaseTurtle (TurtleStack) to a cell with coordinates (RowIdx, ColIdx) with a stack that can be pushed
%% Without clearing the initial position's cell (for hatch moves and 2nd part of normal moves)
%% Return the new board state and the stack of turtles that were displaced
move_push(Board, RowIdx, ColIdx, TurtleStack, NewBoard, DisplacedTurtleStack) :-
  cell_at(Board, RowIdx, ColIdx, DisplacedTurtleStack),
  set_cell(Board, RowIdx, ColIdx, TurtleStack, NewBoard).

% move_climb_push(+Board, +InitialRowIndex, +InitialColumnIndex, +DestinationRowIndex, +DestinationColumnIndex, +BaseTurtle, +TurtleStack, -NewBoard, -DisplacedTurtleStack)
%% Move the TurtleStack to a cell with coordinates (RowIdx, ColIdx) with a stack which has at least 1 element that can be climbed and pushed
%% Remove the moving stack from the initial position's cell
%% Return the new board state and the stack of turtles that were displaced
move_climb_push(Board, InitRowIdx, InitColIdx, DestRowIdx, DestColIdx, BaseTurtle, TurtleStack, NewBoard, DisplacedTurtleStack) :-
  cell_at(Board, InitRowIdx, InitColIdx, FullTurtleStack),
  append(TurtleStack, StackBelow, FullTurtleStack),
  set_cell(Board, InitRowIdx, InitColIdx, StackBelow, TempBoard),
  move_climb_push(TempBoard, DestRowIdx, DestColIdx, TurtleStack, NewBoard, DisplacedTurtleStack).
% move_climb_push(+Board, +RowIndex, +ColumnIndex, +TurtleStack, -NewBoard)
%% Move the TurtleStack to a cell with coordinates (RowIdx, ColIdx) with a stack which has at least 1 element that can be climbed and pushed
%% Without clearing the initial position's cell (for hatch moves and 2nd part of normal moves)
%% Return the new board state
move_climb_push(Board, RowIdx, ColIdx, TurtleStack, NewBoard, DisplacedTurtleStack) :-
  cell_at(Board, RowIdx, ColIdx, DestStack),
  stack_can_climb_push(TurtleStack, DestStack, [], DisplacedTurtleStack),
  append(DisplacedTurtleStack, StackBelow, DestStack),
  push_stack(TurtleStack, StackBelow, NewStack),
  set_cell(Board, RowIdx, ColIdx, NewStack, NewBoard).


% move_outside_board(+OutsideMoveType, +EndColor, +Board, +RowIndex, +ColumnIndex, +Turtle, +TurtleStack, +(Nest1-Nest2), +(Scored1-Scored2), -(NewNest1-NewNest2), -NewBoard -(NewScored1-NewScored2))
%% Move the turtle off the board (but not a scoring move)
%% Clear the initial position's cell
%% OutsideMoveType: sides - turtles are moved to the sides of the board (outside the board) and returned to the respective nests
move_outside_board(sides, _, Board, RowIdx, ColIdx, Turtle, TurtleStack, Nest1-Nest2, _-_, NewNest1-NewNest2, NewBoard, _-_) :-
  cell_at(Board, InitRowIdx, InitColIdx, FullTurtleStack),
  append(TurtleStack, StackBelow, FullTurtleStack),
  set_cell(Board, InitRowIdx, InitColIdx, StackBelow, NewBoard),
  add_to_lists(Nest1-Nest2, TurtleStack, NewNest1-NewNest2).
%% OutsideMoveType: ends - turtles are moved to the ends of the board (outside the board)
%% - Returned to the respective nests if they are of the same color as the end
%% - Scored if they are of the opposite color
%% EndColor: Used to determine which end of the board the turtle(s) is moved to ('white' turtles' end or 'black' turtles' end)
move_outside_board(ends, EndColor, Board, RowIdx, ColIdx, Turtle, TurtleStack, Nest1-Nest2, Scored1-Scored2, SortedNest1-SortedNest2, NewBoard, SortedScored1-SortedScored2) :-
  cell_at(Board, RowIdx, ColIdx, FullTurtleStack),
  append(TurtleStack, StackBelow, FullTurtleStack),
  set_cell(Board, RowIdx, ColIdx, StackBelow, NewBoard),
  split_stack_by_color(TurtleStack, EndColor, SameColorStack, DifferentColorStack),
  add_to_lists(Nest1-Nest2, SameColorStack, NewNest1-NewNest2),
  add_to_lists(Scored1-Scored2, DifferentColorStack, NewScored1-NewScored2),
  sort(NewNest1, SortedNest1),
  sort(NewNest2, SortedNest2),
  sort(NewScored1, SortedScored1),
  sort(NewScored2, SortedScored2).
