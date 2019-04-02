:- module(infolog_tools, [print_location/1, location_affects_module/2,
                          decompose_location/5,location_to_atom/2,
                          print_information/1,
                          information_to_atom/2,
                          add_infolog_error/1, add_infolog_error/2, infolog_internal_error/2,
                          unop/3, binop/4, ternop/5,
                          pairs_to_list/2,
                          git_revision/1,
                          format_with_colour/4,
                          format_err/2, format_msg/2, format_warn/2,
                          start_terminal_colour/2, reset_terminal_colour/1
                          ]).

:- use_module(library(codesio),[format_to_codes/3]).

% various utilities:


% decompose location term
decompose_location(module_lines(M,L1,L2),Mod,unknown,L1,L2) :- !,Mod=M.
decompose_location(module_pred_lines(M,P,L1,L2),Mod,P,L1,L2) :- !, Mod=M.
decompose_location(module_loc(M),Mod,unknown,unknown,unknown) :- !, Mod=M.
decompose_location(module_pred(M,P),Mod,P,unknown,unknown) :- !, Mod=M.
decompose_location(_,unknown,unknown,unknown,unknown).

% print location info
print_location(module_lines(Module,From,To)) :- !, format(' in ~w [~w - ~w] ',[Module,From,To]).
print_location(module_pred_lines(Module,Predicate,From,To)) :- !, format(' in ~w [~w - ~w defining ~w] ',[Module,From,To,Predicate]).
print_location(module_pred(Module,Predicate)) :- !, format(' in ~w [definition of ~w] ',[Module,Predicate]).
print_location(module_loc(Module)) :- !, format(' in module ~w ',[Module]).
print_location(unknown) :- !.
print_location(E) :- add_infolog_error(informat('Illegal location: ~w',E)).

% convert location to atom
location_to_atom(Info,Atom) :-
  location_to_codes(Info,Codes), atom_codes(Atom,Codes).
location_to_codes(module_lines(Module,From,To),Codes) :- !,
     format_to_codes(' in ~w [~w - ~w] ',[Module,From,To],Codes).
location_to_codes(module_pred_lines(Module,Predicate,From,To),Codes) :- !,
     format_to_codes(' in ~w [~w - ~w defining ~w] ',[Module,From,To,Predicate],Codes).
location_to_codes(module_loc(Module),Codes) :- !, format_to_codes(' in module ~w ',[Module],Codes).
location_to_codes(unknown,[]) :- !.
location_to_codes(E,[]) :- add_infolog_error(informat('Illegal location: ~w',E)).

location_affects_module(module_lines(Module,_,_),Module).
location_affects_module(module_pred_lines(Module,_,_,_),Module).
location_affects_module(module_loc(Module),Module).

% print an (error) information term
print_information(Info) :- print_information(Info,user_output).
print_information(informat(Msg,Args),Stream) :- !, format(Stream,Msg,Args).
print_information(string(Msg),Stream) :- !, format(Stream,'~w',[Msg]).  % we could also simply use informat(Msg,[]) instead
print_information(E,_Stream) :- add_infolog_error(informat('Illegal information: ~w',E)).


% convert information term into atom
information_to_atom(Info,Atom) :-
  information_to_codes(Info,Codes), atom_codes(Atom,Codes).
information_to_codes(informat(Msg,Args),Codes) :- !, format_to_codes(Msg,Args,Codes).
information_to_codes(string(Msg),Codes) :- !, format_to_codes('~w',[Msg],Codes).  % we could also simply use informat(Msg,[]) instead
information_to_codes(E,[]) :- add_infolog_error(informat('Illegal information: ~w',E)).

% register an internal error
add_infolog_error(T) :- add_infolog_error(T,unknown).
add_infolog_error(T,Loc) :- format(user_error,'~n*** INTERNAL ERROR: ',[]),
   print_information(T,user_error),
   assert(infolog_internal_error(T,Loc)),
   print_location(Loc),
   format(user_error,'~n',[]).

:- dynamic infolog_internal_error/2.


% utilities to construct/deconstruct terms; faster than =..
unop(X,P,A1) :- functor(X,P,1), arg(1,X,A1).
binop(X,P,A1,A2) :- functor(X,P,2), arg(1,X,A1), arg(2,X,A2).
ternop(X,P,A1,A2,A3) :- functor(X,P,3), arg(1,X,A1), arg(2,X,A2), arg(3,X,A3).

pairs_to_list((X,Y), [X|R]) :- !, pairs_to_list(Y,R).
pairs_to_list(X, [X]).


:- use_module(library(process)).
git_revision(Sha) :-
   absolute_file_name('$SHELL', Shell),
   process_create(Shell, ['-c','cd $PROB_HOME && git rev-parse HEAD && cd - >/dev/null'],[stdout(pipe(F)), process(P)]),
   process_wait(P,_ExitCode),
   stream2code(F,Sha).

% read all characters from a stream
   stream2code(S,Atom) :-
   read_line(S,Text),
   atom_codes(Atom,Text).   


format_msg(Str,Args) :- format_with_colour(user_output,[blue],Str,Args).
format_err(Str,Args) :- format_with_colour(user_error,[red,bold],Str,Args).
format_warn(Str,Args) :- format_with_colour(user_output,[yellow,bold],Str,Args).

format_with_colour(Stream,Colour,Str,Args) :-
   start_terminal_colour(Colour,Stream),
   call_cleanup(format(Stream,Str,Args),
                 reset_terminal_colour(Stream)).

:- use_module(library(system), [environ/2]).
no_color :- environ('NO_COLOR',_). % see http://no-color.org
reset_terminal_colour(_) :- no_color,!.
reset_terminal_colour(Stream) :- write(Stream,'\e[0m').

% see https://misc.flogisoft.com/bash/tip_colors_and_formatting
start_terminal_colour(_,_) :- no_color,!.
start_terminal_colour(red,Stream) :- !, write(Stream,'\e[31m').
start_terminal_colour(green,Stream) :- !, write(Stream,'\e[32m').
start_terminal_colour(yellow,Stream) :- !, write(Stream,'\e[33m').
start_terminal_colour(blue,Stream) :- !, write(Stream,'\e[34m').
start_terminal_colour(magenta,Stream) :- !, write(Stream,'\e[35m').
start_terminal_colour(cyan,Stream) :- !, write(Stream,'\e[36m').
start_terminal_colour(light_gray,Stream) :- !, write(Stream,'\e[37m').
start_terminal_colour(dark_gray,Stream) :- !, write(Stream,'\e[90m').
start_terminal_colour(light_red,Stream) :- !, write(Stream,'\e[91m').
start_terminal_colour(light_green,Stream) :- !, write(Stream,'\e[92m').
start_terminal_colour(white,Stream) :- !, write(Stream,'\e[97m').
start_terminal_colour(bold,Stream) :- !, write(Stream,'\e[1m').
start_terminal_colour(underline,Stream) :- !, write(Stream,'\e[4m').
start_terminal_colour(dim,Stream) :- !, write(Stream,'\e[2m').
start_terminal_colour(black_background,Stream) :- !, write(Stream,'\e[49m').
start_terminal_colour(red_background,Stream) :- !, write(Stream,'\e[41m').
start_terminal_colour(green_background,Stream) :- !, write(Stream,'\e[42m').
start_terminal_colour(yellow_background,Stream) :- !, write(Stream,'\e[43m').
start_terminal_colour(white_background,Stream) :- !, write(Stream,'\e[107m').
start_terminal_colour(blink,Stream) :- !, write(Stream,'\e[5m').
start_terminal_colour(reverse,Stream) :- !, write(Stream,'\e[7m').
start_terminal_colour(hidden,Stream) :- !, write(Stream,'\e[8m').
start_terminal_colour(reset,Stream) :- !, write(Stream,'\e[0m').
start_terminal_colour([],_Stream) :- !.
start_terminal_colour([H|T],Stream) :- !, start_terminal_colour(H,Stream),start_terminal_colour(T,Stream).
start_terminal_colour(C,_Stream) :- format(user_error,'*** UNKNOWN COLOUR: ~w~n',[C]).