% TURTLE MANIPULATION

%% Stack Logic
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

% split_stack(+Stack, +Element, -AboveElement, -BelowElement)
%% Split the stack at the specified element
split_stack(Stack, Elem, Above, Below) :-
  append(Above, [Elem|Below], Stack).

% push_stack(+Stack, +DestinationStack, -NewDestinationStack)
push_stack(Stack, Dest, NewDest) :-
  append(Stack, Dest, NewDest).


%% Game Logic

% turtle_weight(+TurtleStack, -Weight)
%% Use of accumulator for tail recursion increased efficiency
combined_weight(TurtleStack, Weight) :-
  combined_weight_aux(TurtleStack, 0, Weight).

combined_weight_aux([], Acc, Acc).
combined_weight_aux([(_-Number)|Rest], Acc, Weight) :-
  NewAcc is Acc + Number,
  combined_weight_aux(Rest, NewAcc, Weight).



% BOARD MANIPULATION

%% Board Logic

% init_board(+Width, +Length, -Board)
%% Initialize the board with empty cells
init_board(Width, Length, Board) :-
  length(Board, Length),
  maplist(init_row(Width), Board).
% init_row(+Width, -Row)
%% Initialize a row with empty cells
init_row(Width, Row) :-
  length(Row, Width),
  maplist(init_cell, Row).
% init_cell(-Cell)
%% Initialize a cell with an empty list (to be used as a stack)
init_cell([]).

% board_sizes(+Board, -Width, -Length)
%% Get the width and length of the board
board_sizes(Board, Width, Length) :-
  length(Board, Length),
  nth1(1, Board, Row),
  length(Row, Width).

% cell_at(+Board, +RowNum, +ColNum, ?Stack)
%% Returns the stack of turtles at the specified cell or checks if the cell is empty
cell_at(Board, RowNum, ColNum, Stack) :-
  nth1(RowNum, Board, Row),
  nth1(ColNum, Row, Stack).

% cell_empty(+Board, +RowNum, +ColNum)
%% Check if the cell is empty
cell_empty(Board, RowNum, ColNum) :-
  cell_at(Board, RowNum, ColNum, []). % Empty cell is represented by an empty list

% max_cell_size(+Board, -MaxSize)
% Get the maximum size of all the cells on the board
max_cell_size(Board, MaxSize) :-
  findall(Size, (member(Row, Board), member(Cell, Row), length(Cell, Size)), Sizes),
  max_member(MaxSize, Sizes).


%% Game Logic

% find_turtle(+Board, +Turtle, -RowNum, -ColNum)
%% Returns the position of the turtle on the board
find_turtle(Board, Turtle, RowNum, ColNum) :-
  board_sizes(Board, Width, Length),
  between(1, Length, RowNum),
  between(1, Width, ColNum),
  turtle_in_cell(Board, RowNum, ColNum, Turtle).

% turtle_in_cell(+Board, +Turtle, +RowNum, +ColNum)
%% Check if the turtle is in the specified cell
turtle_in_cell(Board, RowNum, ColNum, Turtle) :-
  cell_at(Board, RowNum, ColNum, Stack),
  member(Turtle, Stack).


% cell_can_climb(+Board, +RowNum, +ColNum, +Turtle)
%% Check if the turtle can climb the top turtle of the stack at the specified cell
%% Turtle is lighter than the top turtle of the stack
cell_can_climb(Board, RowNum, ColNum, Turtle) :-
  cell_at(Board, RowNum, ColNum, [TopTurtle|_]),
  turtle_can_climb(Turtle, TopTurtle).

% turtle_can_climb(+Turtle, +TargetTurtle)
%% Check if the turtle can climb the target turtle
turtle_can_climb((_-Number), (_-TargetNumber)) :-
  Number < TargetNumber.


% cell_can_push(+Board, +RowNum, +ColNum, +Turtle)
%% Check if the turtle can push the stack at the specified cell
%% Turtle is stronger than the weight of the stack
cell_can_push(Board, RowNum, ColNum, Turtle) :-
  cell_at(Board, RowNum, ColNum, TurtleStack),
  length(TurtleStack, StackLength),
  nth1(StackLength, TurtleStack, BaseTurtle), % Base turtle is the last turtle in the stack
  turtle_can_push(Turtle, BaseTurtle, TurtleStack).

% turtle_can_push(+Turtle, +TargetTurtle, +TurtleStack)
%% Check if the Turtle can push the stack
%% Turtle stack is pushable if the combined weight of the stack (base turtle included) is less than the turtle
turtle_can_push((_-Number), _, TurtleStack) :-
  combined_weight(TurtleStack, Weight),
  Number >= Weight.

% cell_can_climb_push(+Board, +RowNum, +ColNum, +Turtle)
%% Check if the turtle can climb the stack and push what's above it at the specified cell
cell_can_climb_push(Board, RowNum, ColNum, Turtle) :-
  cell_at(Board, RowNum, ColNum, TurtleStack),
  stack_can_climb_push(Turtle, TurtleStack, []),

% stack_can_climb_push(+Turtle, +Stack, +CheckedStack)
%% Check if any turtle in the stack is climbable and pushable (from the top to the bottom) by Turtle
%% Follows "resolving movement methodically" section of the game rules
%% TODO CHANGE THIS DESCRIPTION VVVVVVVVV
%% CheckedStack contains the stack that has been checked so far
%% - i.e. the stack above the current turtle being checked - useful for weight calculation
%% - at the end, the stack that needs to be moved is in CheckedStack
stack_can_climb_push(_, [], _).
stack_can_climb_push(Turtle, [TopTurtle|Rest], CheckedStack) :-
  turtle_can_climb(Turtle, TopTurtle),
  turtle_can_push(Turtle, TopTurtle, CheckedStack),
  !,
  append(CheckedStack, [TopTurtle], NewCheckedStack).
stack_can_climb_push(Turtle, [_|Rest], _) :-
  stack_can_climb_push(Turtle, Rest).


can_move(Board, RowNum, ColNum, (Color-Number)) :-
  cell_at(Board, RowNum, ColNum, TurtleStack),
  split_stack(TurtleStack, (Color-Number), AboveTurtle, _),
  combined_weight(AboveTurtle, Weight),
  Number > Weight.
