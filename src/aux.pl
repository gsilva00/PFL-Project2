:- use_module(library(between)).

% I/O

% get_choice_ln(+ChoiceText, -Input, +Min, +Max)
%% Same as get_choice/4, but writes a newline after the prompt.
get_choice_ln(ChoiceText,Input,Min,Max) :-
  get_choice(ChoiceText,Input,Min,Max), nl.

% get_choice(+ChoiceText, -Input, +Min, +Max)
%% Prompts the user to choose an option between the Min and Max values.
%% Repeats until a valid option is chosen.
%% Clears the input buffer after reading the option (to avoid reading the newline ).
get_choice(ChoiceText,Input,Min,Max) :-
  format('~a (between ~d and ~d): ', [ChoiceText, Min, Max]),
  read_until_between(Min,Max,Input),
  !,
  clear_buffer.

% read_until_between(+Min, +Max, -Value)
read_until_between(Min,Max,Value) :-
  repeat,
  read_number(Value),
  between(Min,Max,Value),
  !. % Para não pedir mais um número depois de dar sucesso

% read_number(-X)
%% Reads the first digits of a number. Throws the rest of the input away.
%% The 2nd argument is used to avoid returning 0 when no digit is read/something unexpected is read
read_number(X) :-
  read_number_aux(0,false,X).

% read_number_aux(+Acc,+HasAtLeastOneDigit,-X)
read_number_aux(Acc,_,X) :-
  peek_code(C),       % Read character's ASCII code from buffer
  between(48,57,C),  % Between 0 and 9
  !,                 % Para não pedir outro digito ao dar backtracking (sucede só 1 vez por backtracking, torna o predicado deterministico)
  get_code(_),
  Acc1 is Acc*10 + (C-48),
  read_number_aux(Acc1,true,X).
read_number_aux(X,true,X). % Só é chamada quando a de cima falha (quando o caracter não é um número) -> não termina o programa com false (segue para o próximo)

% write_ln(+X)
%% Writes X to the console followed by a newline (avoiding the need to write '\n' all the time - boilerplate).
%% As it exists in other Prolog implementations, but not in SICStus.
writeln(X) :-
  write(X), nl.

% clear_buffer/0.
%% Clears the input buffer.
clear_buffer :-
  repeat,
  get_char(C),
  C = '\n',
  !. % to avoid the true = ? on Sicstus 4.8.0+, works without the ! on older versions.

% clear/0.
%% Clears the console where the game is being played using ANSI escape codes.
%% (for terminals that support ANSI escape codes)
clear :-
  write('\33\[2J').
