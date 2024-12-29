:- consult(io).


config(Width, Length, Level1, Level2).

% GAME START
play :-
  banner,
  choose_gamemode(Gamemode),
  choose_difficulty(Gamemode, Level1-Level2),
  choose_players_names(Gamemode, Player1Name, Player2Name),
  choose_first_player(Gamemode, Player1Name, Player2Name, FirstPlayerName),
  choose_board_size(Width, Length),
  initial_state(config(Width, Length, Player1Name-Level1, Player2Name-Level2, FirstPlayerName), GameState),
  clear.


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

% CONFIGURING THE GAME
%% Gamemode
choose_gamemode(Gamemode) :-
  display_gamemode_menu,
  get_menu_choice_ln('Option', 1, 5, Choice),
  gamemode(Choice, Gamemode),
  handle_gamemode(Gamemode).

display_gamemode_menu :-
  writeln('Choose your game mode'),
  writeln('1. Human vs. Human (H/H)'),
  writeln('2. Human vs. Computer (H/PC)'),
  writeln('3. Computer vs. Human (PC/H)'),
  writeln('4. Computer vs. Computer (PC/PC)'),
  writeln('5. Exit').

gamemode(1, hh).
gamemode(2, hc).
gamemode(3, ch).
gamemode(4, cc).
gamemode(5, exit).

handle_gamemode(exit) :- halt.
handle_gamemode(_).


%% Player Names
choose_players_names(hh, Player1Name, Player2Name) :-
  get_string_ln('Choose Player 1\'s name (white turtles)', Player1Name),
  get_string_ln('Choose Player 2\'s name (black turtles)', Player2Name).
choose_players_names(hc, Player1Name, 'Computer') :-
  get_string_ln('Choose Player\'s name (white turtles)', Player1Name).
choose_players_names(ch, 'Computer', Player2Name) :-
  get_string_ln('Choose Player\'s name (black turtles)', Player2Name).
choose_players_names(cc, 'Computer 1', 'Computer 2').

%% First Player
choose_first_player(hh, Player1Name, Player2Name, FirstPlayerName) :-
  display_first_player_menu(Player1Name, Player2Name),
  get_menu_choice_ln('Option', 1, 2, Choice),
  nth1(Choice, [Player1Name, Player2Name], FirstPlayerName).
choose_first_player(hc, Player1Name, 'Computer', Player1Name).
choose_first_player(ch, 'Computer', Player2Name, Player2Name).
choose_first_player(cc, 'Computer 1', 'Computer 2', 'Computer 1').

display_first_player_menu(Player1Name, Player2Name) :-
  write('Choose who plays first: '), nl,
  format('1. ~w~n', [Player1Name]),
  format('2. ~w~n', [Player2Name]).


%% Difficulty
choose_difficulty(hh, Level1-Level2) :-
  difficulty_level(0, Level1),
  difficulty_level(0, Level2).
choose_difficulty(hc, Level1-Level2) :-
  display_difficulty_menu(1),
  get_menu_choice_ln('Option', 1, 2, DifficultyChoice),
  difficulty_level(0, Level1),
  difficulty_level(DifficultyChoice, Level2).
choose_difficulty(ch, Level1-Level2) :-
  display_difficulty_menu(1),
  get_menu_choice_ln('Option', 1, 2, DifficultyChoice),
  difficulty_level(DifficultyChoice, Level1),
  difficulty_level(0, Level2).
choose_difficulty(cc, Level1-Level2) :-
  display_difficulty_menu(1),
  get_menu_choice_ln('Option', 1, 2, DifficultyChoice1),
  difficulty_level(DifficultyChoice1, Level1),
  display_difficulty_menu(2),
  get_menu_choice_ln('Option', 1, 2, DifficultyChoice2),
  difficulty_level(DifficultyChoice2, Level2).

display_difficulty_menu(ComputerNum) :-
  format('Choose Computer ~d\'s difficulty level: ~n', [ComputerNum]),
  writeln('1. Easy'), % AI plays random moves
  writeln('2. Hard'). % AI plays the best move

%%% Difficulty Levels
%%%%  0 - Human, 1 - Easy, 2 - Hard
difficulty_level(0, human).
difficulty_level(1, easy).
difficulty_level(2, hard).


%% Board Size
choose_board_size(Width, Length) :-
  writeln('Turtles will be placed on a rectangular board. You can choose its size!'),
  display_board_width_menu,
  get_menu_choice_ln('Option', 2, 6, Width),
  display_board_length_menu,
  get_menu_choice_ln('Option', 4, 8, Length).

display_board_width_menu :-
  writeln('Choose the board width: 2, 3, 4, 5 or 6').

display_board_length_menu :-
  writeln('Choose the board length: 4, 5, 6, 7 or 8').


% TODO: Implement the following predicates

initial_state(+GameConfig, -GameState) :-
  format('Game started with ~w vs. ~w!~n', [Player1, Player2]).

display_game(+GameState).

move(+GameState, +Move, -NewGameState).

valid_moves(+GameState, -ListOfMoves).

game_over(+GameState, -Winner).

value(+GameState, +Player, -Value).

choose_move(+GameState, +Level, -Move).