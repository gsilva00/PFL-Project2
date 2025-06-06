:- use_module(library(between)).
:- use_module(library(lists)).


% INTEGER INPUT

% get_menu_choice(+ChoiceText, +Min, +Max, -Input)
%% Prompt the user to choose an option between the Min and Max values
%% Repeat until a valid option is chosen (see read_until_between/3)
%% Clear the input buffer after reading the option (to avoid reading the newline)
get_menu_choice(ChoiceText, Min, Max, Input) :-
  format('~a (between ~d and ~d): ', [ChoiceText, Min, Max]),
  read_until_between(Min, Max, Input).

% read_until_between(+Min, +Max, -Value)
%% Read a number until it is between Min and Max
read_until_between(Min, Max, Input) :-
  repeat,
  read_number(Input),
  between(Min, Max, Input),
  !. % Not read another number after a valid one

% read_number(-X)
%% Read a number from the input
read_number(X) :-
  read_number_aux(0,false,X).

% read_number_aux(+Acc,+HasAtLeastOneDigit,-X)
%% Auxiliary predicate to read_number/1
read_number_aux(Acc, _, X) :-
  read_line(Codes),        % Read line as a list of ASCII codes
  process_codes(Codes, Acc, X).

% process_codes(+Codes, +Acc, -X)
%% Auxiliary predicate to read_number_aux/3
%% Process the list of ASCII codes to extract the number
process_codes([C|Rest], Acc, X) :-
  between(48, 57, C), % Check if each code is between 0 and 9
  !,
  Acc1 is Acc*10 + (C-48),
  process_codes(Rest, Acc1, X).
process_codes(_, Acc, Acc). % Stop processing when a non-digit is encountered or the list is empty



% STRING INPUT

% get_string(+ChoiceText, -Input)
%% Prompt the user to input a string
get_string(ChoiceText, Input) :-
  format('~a: ', [ChoiceText]),
  read_string(Input),
  clear_input.

% read_string(-Str)
%% Read a string from the input
read_string(Str) :-
  read_string_aux([], false, Str).

% read_string_aux(+Acc,+HasAtLeastOneChar,-Str)
%% Auxiliary predicate to read_string/1
read_string_aux(Acc, _, Str) :-
  peek_char(C),
  C \= '\n',
  !,
  get_char(_),
  read_string_aux([C|Acc], true, Str).

read_string_aux(Acc, false, Str) :-   %it only enters here if the user only writes a newline
  get_char(_),                        %consume useless newline
  read_string_aux(Acc, false, Str).   %try obtain the string again

read_string_aux(Acc, true, Str) :-
  reverse(Acc, Reversed),     % Char list built in reverse due to efficient head insertion
  atom_chars(Str, Reversed). % Convert char list to atom (as it is only used for printing)



% UTILITIES

% get_menu_choice_ln(+ChoiceText, +Min, +Max, -Input)
%% Same as get_menu_choice/4, but writes a newline after the prompt (better readability/user experience)
get_menu_choice_ln(ChoiceText, Min, Max, Input) :-
  get_menu_choice(ChoiceText, Min, Max, Input),
  nl.

% get_string_ln(+ChoiceText, -Input)
%% Same as get_string/2, but writes a newline after the prompt (better readability/user experience)
get_string_ln(ChoiceText, Input) :-
  get_string(ChoiceText, Input),
  nl.

% write_ln(+X)
%% Write X to the console followed by a newline (avoiding the need to write '\n' all the time - boilerplate)
%% Exists in other Prolog implementations, but not in SICStus
write_ln(X) :-
  write(X),
  nl.

% clear_input/0
%% Clear the input buffer
clear_input :-
  repeat,
  get_char(C),
  C = '\n',
  !. % to avoid the true = ? on Sicstus 4.8.0+, works without the ! on older versions

% clear/0
%% Clear the console where the game is being played
%% For terminals that support ANSI escape codes
clear :-
  write('\e[H\e[2J'),
  flush_output.
