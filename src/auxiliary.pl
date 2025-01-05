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

% max_cell_size(+Board, -MaxSize)
% Get the maximum size of all the cells on the board
max_cell_size(Board, MaxSize) :-
  findall(Size, (member(Row, Board), member(Cell, Row), length(Cell, Size)), Sizes),
  max_member(MaxSize, Sizes).


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

%%% 2.1. Miscellanous

% find_stack_to_move(+Board, +Turtle, -RowIdx, -ColIdx, -TurtleStack)
%% Find the turtle on the board and return the stack of turtles above it, including the turtle itself - stack-to-move
%% Return the position of the TurtleStack on the board
find_stack_to_move(Board, (Color-Number), RowIdx, ColIdx, TurtleStack) :-
  find_turtle(Board, (Color-Number), RowIdx, ColIdx),
  cell_at(Board, RowIdx, ColIdx, TurtleStack),
  split_stack(TurtleStack, (Color-Number), StackAbove, _),
  append(StackAbove, [(Color-Number)], TurtleStack).

% find_turtle(+Board, +Turtle, -RowIdx, -ColIdx)
%% Return the position of the turtle on the board
find_turtle(Board, Turtle, RowIdx, ColIdx) :-
  board_sizes(Board, Width, Length),
  between(1, Length, RowIdx),
  between(1, Width, ColIdx),
  turtle_in_cell(Board, RowIdx, ColIdx, Turtle).

% turtle_in_board(+Board, +Turtle)
%% Check if the turtle is in any of the stacks on the board
turtle_in_board(Board, Turtle) :-
  member(Row, Board),
  member(Stack, Row),
  member(Turtle, Stack),
  !.

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

% move_score(+Board, +RowIndex, +ColumnIndex, +TurtleStack, -NewBoard)
%% Move the turtle off the opponent's side of the board to score (clear the initial position's cell)
%% Return the new board state
move_score(Board, RowIdx, ColIdx, TurtleStack, NewBoard) :-
  set_cell(Board, RowIdx, ColIdx, [], NewBoard).

% move_empty(+Board, +InitialRowIndex, +InitialColumnIndex, +DestinationRowIndex, +DestinationColumnIndex, +TurtleStack, -NewBoard)
%% Move the turtle to an empty cell with coordinates (RowIdx, ColIdx)
%% Clear the initial position's cell
%% Return the new board state
move_empty(Board, InitRowIdx, InitColIdx, DestRowIdx, DestColIdx, TurtleStack, NewBoard) :-
  set_cell(Board, InitRowIdx, InitColIdx, [], TempBoard),
  set_cell(TempBoard, DestRowIdx, DestColIdx, TurtleStack, NewBoard).
% move_empty(+Board, +RowIndex, +ColumnIndex, +TurtleStack, -NewBoard)
%% Move the turtle to an empty cell with coordinates (RowIdx, ColIdx)
%% Without clearing the initial position's cell (for hatch moves and 2nd part of normal moves)
%% Return the new board state
move_empty(Board, RowIdx, ColIdx, TurtleStack, NewBoard) :-
  set_cell(Board, RowIdx, ColIdx, TurtleStack, NewBoard).

% move_climb(+Board, +InitialRowIndex, +InitialColumnIndex, +DestinationRowIndex, +DestinationColumnIndex, +TurtleStack, -NewBoard)
%% Move the turtle to a cell with coordinates (RowIdx, ColIdx) with a stack that can be climbed
%% Clear the initial position's cell
%% Return the new board state
move_climb(Board, InitRowIdx, InitColIdx, DestRowIdx, DestColIdx, TurtleStack, NewBoard) :-
  set_cell(Board, InitRowIdx, InitColIdx, [], TempBoard),
  move_climb(TempBoard, DestRowIdx, DestColIdx, TurtleStack, NewBoard).
% move_climb(+Board, +RowIndex, +ColumnIndex, +TurtleStack, -NewBoard)
%% Move the turtle to a cell with coordinates (RowIdx, ColIdx) with a stack that can be climbed
%% Without clearing the initial position's cell (for hatch moves and 2nd part of normal moves)
%% Return the new board state
move_climb(Board, RowIdx, ColIdx, TurtleStack, NewBoard) :-
  cell_at(Board, RowIdx, ColIdx, DestStack),
  push_stack(TurtleStack, DestStack, NewStack),
  set_cell(Board, RowIdx, ColIdx, NewStack, NewBoard).

% move_push(+Board, +InitialRowIndex, +InitialColumnIndex, +DestinationRowIndex, +DestinationColumnIndex, +TurtleStack, -NewBoard, -DisplacedTurtleStack)
%% Move the turtle to a cell with coordinates (RowIdx, ColIdx) with a stack that can be pushed
%% Clear the initial position's cell
%% Return the new board state and the stack of turtles that were displaced
move_push(Board, InitRowIdx, InitColIdx, DestRowIdx, DestColIdx, TurtleStack, NewBoard, DisplacedTurtleStack) :-
  set_cell(Board, InitRowIdx, InitColIdx, [], TempBoard),
  move_push(TempBoard, DestRowIdx, DestColIdx, TurtleStack, NewBoard, DisplacedTurtleStack).
% move_push(+Board, +RowIndex, +ColumnIndex, +TurtleStack, -NewBoard, -DisplacedTurtleStack)
%% Move the turtle to a cell with coordinates (RowIdx, ColIdx) with a stack that can be pushed
%% Without clearing the initial position's cell (for hatch moves and 2nd part of normal moves)
%% Return the new board state and the stack of turtles that were displaced
move_push(Board, RowIdx, ColIdx, TurtleStack, NewBoard, DisplacedTurtleStack) :-
  cell_at(Board, RowIdx, ColIdx, DisplacedTurtleStack),
  set_cell(Board, RowIdx, ColIdx, TurtleStack, NewBoard).

% move_climb_push(+Board, +InitialRowIndex, +InitialColumnIndex, +DestinationRowIndex, +DestinationColumnIndex, +TurtleStack, -NewBoard, -DisplacedTurtleStack)
%% Move the TurtleStack to a cell with coordinates (RowIdx, ColIdx) with a stack which has at least 1 element that can be climbed and pushed
%% Clear the initial position's cell
%% Return the new board state and the stack of turtles that were displaced
move_climb_push(Board, InitRowIdx, InitColIdx, DestRowIdx, DestColIdx, TurtleStack, NewBoard, DisplacedTurtleStack) :-
  set_cell(Board, InitRowIdx, InitColIdx, [], TempBoard),
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
